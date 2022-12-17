open! Import
open Ppxlib
open Ast_builder.Default

let namespace = "ppx_effects"
let pp_quoted ppf s = Format.fprintf ppf "`%s`" s
let raise_errorf ~loc fmt = Location.raise_errorf ~loc ("%s: " ^^ fmt) namespace

module Handler_kind = struct
  type t =
    | Deep
    | Shallow of [ `Continue | `Discontinue ]

  (* let to_module_ident t : Longident.t =
    let ident = match t with Deep -> "Deep" | Shallow _ -> "Shallow" in
    Ldot (Lident "Ppx_effects_runtime", ident)

  let to_continuation_type ~loc t (a, b) : core_type =
    ptyp_constr ~loc
      { loc; txt = Ldot (to_module_ident t, "continuation") }
      [ a; b ] *)

  let to_effc ~loc t effc =
    (* For some reason, metaquot doesn't like abstract types + unquoting?? *)
    match t with
    | Deep ->
      [%expr
        let effc
          : type continue_input.
            continue_input Ppx_effects_runtime.t
            -> ((continue_input, _) Ppx_effects_runtime.Deep.continuation -> _) option
          =
          [%e effc]
        in
        effc]
    | Shallow _ ->
      [%expr
        let effc
          : type continue_input.
            continue_input Ppx_effects_runtime.t
            -> ((continue_input, _) Ppx_effects_runtime.Shallow.continuation -> _) option
          =
          [%e effc]
        in
        effc]
  ;;
end

(** Cases of [match] / [try] can be partitioned into three categories:

    - exception patterns (uing the [exception] keyword);
    - effect patterns (written using [\[%effect? ...\]]);
    - return patterns (available only to [match]).

    The [Stdlib.Effect] API requires passing different continuations for each of
    these categories. *)
module Cases = struct
  type t =
    { ret : cases
    ; exn : cases
    ; eff : cases
    }

  let get_effect_payload ~loc : payload -> pattern = function
    | PPat (x, None) -> x
    | PPat (_, Some _) ->
      raise_errorf
        ~loc
        "when clauses not permitted in %a node.@,Hint: did you mean to use %a instead?"
        pp_quoted
        "[%effect? <pattern>]"
        pp_quoted
        "[%effect? <pattern>] when <expr>"
    | _ ->
      (* The user made a mistake and forgot to add [?] after [effect] (this
         node captures expressions rather than patterns). *)
      raise_errorf
        ~loc
        "invalid node %a used as a pattern.@,Hint: did you mean to use %a instead?"
        pp_quoted
        "[%effect <expr>]"
        pp_quoted
        "[%effect? <pattern>]"
  ;;

  let fold_case : ?map_expression:(expression -> expression) -> case -> t -> t =
   fun ?(map_expression = Fn.id) case acc ->
    match case.pc_lhs with
    | { ppat_desc = Ppat_extension ({ txt = "effect"; _ }, payload); _ } ->
      let loc = case.pc_lhs.ppat_loc in
      let body = get_effect_payload ~loc payload in
      (match body with
       | [%pat? [%p? eff_pattern], [%p? k_pattern]] ->
         let pc_rhs =
           let loc = case.pc_rhs.pexp_loc in
           [%expr Some (fun [%p k_pattern] -> [%e map_expression case.pc_rhs])]
         in
         let case = { pc_lhs = eff_pattern; pc_rhs; pc_guard = case.pc_guard } in
         { acc with eff = case :: acc.eff }
       (* Also allow a single [_] to wildcard both effect and pattern. *)
       | [%pat? _] ->
         let pc_rhs =
           let loc = case.pc_rhs.pexp_loc in
           [%expr Some (fun _ -> [%e map_expression case.pc_rhs])]
         in
         let case = { pc_lhs = [%pat? _]; pc_rhs; pc_guard = case.pc_guard } in
         { acc with eff = case :: acc.eff }
       (* Can't split the pattern into effect and continuation components. *)
       | _ ->
         let error_prefix =
           "invalid [%effect? ...] payload. Expected a pattern for an (effect, \
            continuation) pair"
         in
         (* Maybe the user missed a comma separating the two? *)
         (match body with
          | { ppat_desc =
                Ppat_construct
                  (effect, Some ([], { ppat_desc = Ppat_var { txt = "k"; _ }; _ }))
            ; _
            } ->
            raise_errorf
              ~loc
              "%s.@,Hint: did you mean %a?"
              error_prefix
              pp_quoted
              (Printf.sprintf "[%%effect? %s, k]" (Longident.name effect.txt))
          (* Otherwise, just raise a generic error. *)
          | _ ->
            raise_errorf ~loc "%s, e.g. %a." error_prefix pp_quoted "[%effect? Foo, k]"))
    | [%pat? exception [%p? exn_pattern]] ->
      let pc_rhs = map_expression case.pc_rhs in
      let case = { pc_lhs = exn_pattern; pc_rhs; pc_guard = case.pc_guard } in
      { acc with exn = case :: acc.exn }
    | _ -> { acc with ret = case :: acc.ret }
 ;;

  let partition : ?map_expression:(expression -> expression) -> cases -> t =
   fun ?map_expression cases ->
    List.fold_right
      cases
      ~init:{ ret = []; exn = []; eff = [] }
      ~f:(fold_case ?map_expression)
 ;;

  let contain_effect_handler : cases -> bool =
    List.exists ~f:(fun case ->
      match case.pc_lhs.ppat_desc with
      | Ppat_extension ({ txt = "effect"; _ }, _) -> true
      | _ -> false)
  ;;
end

(** The [Stdlib.Effect] API requires effects to happen under a function
    application *)
module Scrutinee = struct
  type t =
    { function_ : expression
    ; argument : expression
    }

  (* An expression is a syntactic value if its AST structure precludes it from
     raising an effect or an exception. Here we use a very simple
     under-approximation (avoiding multiple recursion): *)

  let rec expr_is_syntactic_value (expr : expression) : bool =
    match expr.pexp_desc with
    | Pexp_ident _ | Pexp_constant _ | Pexp_function _ | Pexp_fun _
    | Pexp_construct (_, None)
    | Pexp_variant (_, None)
    | Pexp_lazy _ -> true
    | Pexp_let _
    | Pexp_apply _
    | Pexp_match _
    | Pexp_try _
    | Pexp_setfield _
    | Pexp_array _
    | Pexp_ifthenelse _
    | Pexp_sequence _
    | Pexp_while _
    | Pexp_for _
    | Pexp_new _
    | Pexp_override _
    | Pexp_letmodule _
    | Pexp_object _
    | Pexp_pack _
    | Pexp_letop _
    | Pexp_extension _
    | Pexp_unreachable -> false
    (* Congruence cases: *)
    | Pexp_tuple es -> List.for_all es ~f:expr_is_syntactic_value
    | Pexp_record (fields, base) ->
      List.for_all fields ~f:(fun (_field, e) -> expr_is_syntactic_value e)
      && Option.for_all base ~f:expr_is_syntactic_value
    | Pexp_field (e, _)
    | Pexp_constraint (e, _)
    | Pexp_coerce (e, _, _)
    | Pexp_construct (_, Some e)
    | Pexp_variant (_, Some e)
    | Pexp_send (e, _)
    | Pexp_setinstvar (_, e)
    | Pexp_letexception (_, e)
    | Pexp_assert e
    | Pexp_newtype (_, e)
    | Pexp_open (_, e) -> expr_is_syntactic_value e
    | Pexp_poly _ -> assert false
  ;;

  let of_expression ~(handler_kind : Handler_kind.t) expr =
    match handler_kind, expr with
    | _, [%expr [%e? function_] [%e? argument]]
      when expr_is_syntactic_value function_ && expr_is_syntactic_value argument ->
      { function_; argument }
    | Deep, expr ->
      (* If the expression is not already of the form [f x] then we must
           allocate a thunk to delay the effect. *)
      let loc = expr.pexp_loc in
      (* NOTE: here we use [`Unit] over [()] in case the user has
           shadowed the unit constructor. *)
      let function_ = [%expr fun `Unit -> [%e expr]] in
      let argument = [%expr `Unit] in
      { function_; argument }
    | Shallow `Continue, expr ->
      let loc = expr.pexp_loc in
      let function_ =
        [%expr Ppx_effects_runtime.Shallow.fiber (fun `Unit -> [%e expr])]
      in
      let argument = [%expr `Unit] in
      { function_; argument }
    | Shallow `Discontinue, expr ->
      let loc = expr.pexp_loc in
      raise_errorf
        ~loc
        "invalid match%%discontinue scrutinee. Expected a %a expression."
        pp_quoted
        "continuation exception"
  ;;
end

(* Both [exnc] and [effc] require a noop case to represent an unhandled
   exception or effect respectively. [exnc] reraises the unhandled exception,
   and [effc] returns None.

   Caveat: it's possible that a noop case is not needed becuase the user's
   handler is exhaustive, resulting in an unwanted "redundant case" warning.
   We get around this by checking whether the users' cases are syntactically
   exhaustive and not adding the noop case if so.

   It'd be nice to solve this by just locally disabling the redundant case
   warning with [[@warning "-11"]], but this would have to go on the entire
   match (in which case it leaks the users' subexpressions). Unfortunately,
   OCaml doesn't support [[@warning "-11"]] on individual patterns. *)
let extensible_cases_are_exhaustive : cases -> bool =
  let pattern_matches_anything p =
    match p.ppat_desc with
    | Ppat_any | Ppat_var _ -> true
    | _ -> false
  in
  List.exists ~f:(fun case ->
    Option.is_none case.pc_guard && pattern_matches_anything case.pc_lhs)
;;

(* Given a list of effect handlers, build a corresponding [effc] continuation to
   pass to [Deep.{try,match}_with]. *)
let effc ~loc ~handler_kind (cases : cases) : expression =
  assert (not (List.is_empty cases));
  let noop_case =
    match extensible_cases_are_exhaustive cases with
    | true -> []
    | false -> [ case ~lhs:[%pat? _] ~guard:None ~rhs:[%expr None] ]
  in
  (* NOTE: the name [continue_input] is leaked to the user (accessible from
     their code, and appears in error message). *)
  Handler_kind.to_effc ~loc handler_kind (pexp_function ~loc (cases @ noop_case))
;;

(* Given a list of exception handlers, build a corresponding [exnc] continuation
   to pass to [Deep.{try,match}_with]. *)
let exnc ~loc (cases : cases) : expression =
  match cases with
  | [] -> [%expr Ppx_effects_runtime.raise]
  | _ :: _ ->
    let noop_case =
      match extensible_cases_are_exhaustive cases with
      | true -> []
      | false ->
        [ case ~lhs:[%pat? e] ~guard:None ~rhs:[%expr Ppx_effects_runtime.raise e] ]
    in
    pexp_function ~loc (cases @ noop_case)
;;

(* Captures top-level [%effect? _] in [try] / [match] expressions and converts
   them to [Deep.{try,match}_with].

   Also handles [exception%effect ...] in structures – see below. *)
let impl : structure -> structure =
  (object (this)
     inherit Ast_traverse.map as super

     method! expression expr =
       let loc = expr.pexp_loc in
       match expr with
       (* Handles: [ match _ with [%effect? E _, k] -> ... ] *)
       | { pexp_desc = Pexp_match (scrutinee, cases); _ }
         when Cases.contain_effect_handler cases ->
         let scrutinee =
           Scrutinee.of_expression ~handler_kind:Deep (this#expression scrutinee)
         in
         let cases = Cases.partition ~map_expression:this#expression cases in
         let retc =
           match cases.ret with
           | [] ->
             raise_errorf
               ~loc
               "none of the patterns in this %a expression match values."
               pp_quoted
               "match"
           | _ :: _ -> pexp_function ~loc cases.ret
         and exnc = exnc ~loc cases.exn
         and effc = effc ~loc ~handler_kind:Deep cases.eff in
         [%expr
           Ppx_effects_runtime.Deep.match_with
             [%e scrutinee.function_]
             [%e scrutinee.argument]
             { Ppx_effects_runtime.Deep.retc = [%e retc]
             ; exnc = [%e exnc]
             ; effc = [%e effc]
             }]
       (* Handles: [ try _ with [%effect? E _, k] -> ... ] *)
       | { pexp_desc = Pexp_try (scrutinee, cases); _ }
         when Cases.contain_effect_handler cases ->
         let scrutinee =
           Scrutinee.of_expression ~handler_kind:Deep (this#expression scrutinee)
         in
         let cases = Cases.partition ~map_expression:this#expression cases in
         let effc = effc ~loc ~handler_kind:Deep cases.eff in
         [%expr
           Ppx_effects_runtime.Deep.try_with
             [%e scrutinee.function_]
             [%e scrutinee.argument]
             { Ppx_effects_runtime.Deep.effc = [%e effc] }]
       | e -> super#expression e

     method! extension =
       function
       | { txt = "effect"; loc }, _ ->
         raise_errorf
           ~loc
           "dangling [%%effect ...] extension node. This node may be used as:\n\
           \ - the top level of %a or %a patterns as %a\n\
           \ - on an exception definition as %a."
           pp_quoted
           "match"
           pp_quoted
           "try"
           pp_quoted
           "[%effect? ...]"
           pp_quoted
           "exception%effect ..."
       | e -> super#extension e
  end)
    #structure
;;

let effect_decl_of_exn_decl ~loc (exn : type_exception) : type_extension =
  let name = exn.ptyexn_constructor.pext_name in
  let eff_type = Located.lident ~loc "Ppx_effects_runtime.t" in
  let constrs, args =
    match exn.ptyexn_constructor.pext_kind with
    (* TODO: Provide ability to match on existentials *)
    | Pext_decl (_, constrs, body) ->
      let body = Option.map ~f:(fun typ -> ptyp_constr ~loc eff_type [ typ ]) body in
      constrs, body
    | Pext_rebind _ ->
      raise_errorf
        ~loc
        "cannot process effect defined as an alias of %a."
        pp_quoted
        name.txt
  in
  let params = [ ptyp_any ~loc, (NoVariance, NoInjectivity) ] in
  type_extension
    ~loc
    ~path:eff_type
    ~params
    ~constructors:
      [ extension_constructor ~loc ~name ~kind:(Pext_decl ([], constrs, args)) ]
    ~private_:Public
;;

let match_shallow ~loc ~shallow_kind scrutinee cases =
  let scrutinee =
    Scrutinee.of_expression ~handler_kind:(Shallow shallow_kind) scrutinee
  in
  let cases = Cases.partition cases in
  let retc =
    match cases.ret with
    | [] ->
      raise_errorf
        ~loc
        "none of the patterns in this %a expression match values."
        pp_quoted
        "match"
    | _ :: _ -> pexp_function ~loc cases.ret
  and exnc = exnc ~loc cases.exn
  and effc = effc ~loc ~handler_kind:(Shallow shallow_kind) cases.eff in
  [%expr
    Ppx_effects_runtime.Shallow.continue_with
      [%e scrutinee.function_]
      [%e scrutinee.argument]
      { Ppx_effects_runtime.Shallow.retc = [%e retc]; exnc = [%e exnc]; effc = [%e effc] }]
;;

let match_continue =
  Extension.declare
    "continue"
    Expression
    Ast_pattern.(pstr (pstr_eval (pexp_match __ __) nil ^:: nil))
    (fun ~loc ~path:_ scrutinee cases ->
      match_shallow ~loc ~shallow_kind:`Continue scrutinee cases)
;;

let match_discontinue =
  Extension.declare
    "discontinue"
    Expression
    Ast_pattern.(pstr (pstr_eval (pexp_match __ __) nil ^:: nil))
    (fun ~loc ~path:_ scrutinee cases ->
      match_shallow ~loc ~shallow_kind:`Discontinue scrutinee cases)
;;

let str_effect_decl =
  Extension.declare
    "effect"
    Structure_item
    Ast_pattern.(pstr (pstr_exception __ ^:: nil))
    (fun ~loc ~path:_ exn -> pstr_typext ~loc (effect_decl_of_exn_decl ~loc exn))
;;

let sig_effect_decl =
  Extension.declare
    "effect"
    Signature_item
    Ast_pattern.(psig (psig_exception __ ^:: nil))
    (fun ~loc ~path:_ exn -> psig_typext ~loc (effect_decl_of_exn_decl ~loc exn))
;;

let () =
  Reserved_namespaces.reserve namespace;
  Driver.register_transformation
    ~extensions:[ str_effect_decl; sig_effect_decl; match_discontinue; match_continue ]
    ~impl
    namespace
;;
