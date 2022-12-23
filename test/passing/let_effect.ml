type 'a state =
  { get : unit -> 'a
  ; set : 'a -> unit
  }

let run (type a b) (fn : a state -> b) ~(init : a) : b =
  let exception%effect Get : a in
  let exception%effect Set : a -> unit in
  let state =
    { get = (fun () -> perform Get); set = (fun content -> perform @@ Set content) }
  in
  (match fn state with
   | ret -> fun _ -> ret
   | [%effect? Get, k] -> fun (content : a) -> continue k content content
   | [%effect? Set new_content, k] -> fun (_content : a) -> continue k () new_content)
  @@ init
;;
