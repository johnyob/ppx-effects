## ✨ ppx-effects
> A syntax extension for untyped effects in OCaml 5.0

[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https://ci.ocamllabs.io/badge/johnyob/ppx-effects/main&logo=ocaml)](https://ci.ocamllabs.io/github/johnyob/ppx-effects)


OCaml 5.0 supports [_algebraic effects_][effects-tutorial]! :tada:

However, since the effect implementation is currently untyped, the compiler
doesn't yet provide any dedicated syntax to support defining or handling
effects. This PPX provides a close approximation to the _proposed_ syntax,
hopefully making it simpler to use effects in your OCaml 5.0 code (and easing
future migrations to a dedicated syntax).



## Install

This library has not yet been released to `opam`. To install it, first 

```
opam pin add --yes https://github.com/johnyob/ppx-effects.git
opam install ppx_effects
```

Users of [`dune`](https://github.com/ocaml/dune/) can then use this PPX on their
libraries and executables by adding the appropriate stanza field:

```lisp
(library
 ...
 (preprocess (pps ppx_effects)))
```

[effects-tutorial]: https://github.com/ocamllabs/ocaml-effects-tutorial

## Syntax

In short:

- **Declaring effects**: `effect E : string -> int` is written as `exception%effect E : string -> int`. Local effects may be declared using `let exception%effect E : string -> int`. 
- **Handling effects**: `| effect (E _) k ->` is written as `| [%effect? (E _), k] ->`.
- **Shallow handlers** `match%continue` and `match%discontinue` for `Effect.Shallow.continue_with` and `Effect.Shallow.discontinue_with` respectively.

```ocaml
module State = struct
  type 'a t =
    { get : unit -> 'a
    ; set : 'a -> unit
    }

  let run (type a b) (fn : a t -> b) ~(init : a) : b =
    let exception%effect Get : a in
    let exception%effect Set : a -> unit in
    let state =
      { get = (fun () -> perform Get)
      ; set = (fun content -> perform (Set content)) 
      }
    in
    let comp =
      match fn state with
      | result -> fun _ -> result
      | [%effect? Get, k] -> 
          fun (content : a) -> continue k content content
      | [%effect? Set new_content, k] -> 
          fun (_content : a) -> continue k () new_content
    in
    comp init
  ;;
end
```
## Details

Using the PPX should ideally be exactly like using the dedicated syntax.
However, there are a few implementation details that can leak to PPX users:

- the expansion of `match` / `try` expressions containing top-level `[%effect?
  ...]` patterns introduces a locally-abstract type named `continue_input`
  representing the type of values passed to `continue` (and returned from a
  suspended `perform`). This type name can appear in error messages, but
  shouldn't be referred to from user code. (If you find you _do_ need this type
  name for some reason, raise an issue on this repository!)
  
- in order to use the low-level effects API provided by the compiler, an
  effectful computation being `match`-ed or `try`-ed must be wrapped in an
  allocated thunk (e.g. `fun () -> ...`). This thunk has a small performance
  cost, so very performance-critical code should arrange to make this expression
  a simple unary function application (as in, `match f x with` or `try f x
  with`) instead – this avoids needing to allocate the thunk.
