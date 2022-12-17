exception%effect E : string

let comp () =
  print_string "0 ";
  print_string (Effect.perform E);
  print_string "3 "

let raise f = f

let main () =
  try comp ()
  with [%effect? E, k] ->
    print_string "1 ";
    Effect.Deep.continue k "2 ";
    print_string "4 "

exception%effect Send : int -> unit
exception%effect Recv : int

let run (comp : unit -> unit) : unit =
  let rec loop_send : type a. (a, unit) Effect.Shallow.continuation -> a -> unit
      =
   fun k v ->
    match%continue k v with
    | return -> return
    | [%effect? Send n, k] -> loop_recv n k ()
    | [%effect? Recv, _] -> failwith "protocol violation"
  and loop_recv :
      type a. int -> (a, unit) Effect.Shallow.continuation -> a -> unit =
   fun n k v ->
    match%continue k v with
    | return -> return
    | [%effect? Recv, k] -> loop_send k n
    | [%effect? Send _, _] -> failwith "protocol violation"
  in
  loop_send (Effect.Shallow.fiber comp) ()