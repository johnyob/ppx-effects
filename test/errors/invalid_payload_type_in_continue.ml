let () =
  match%continue k () with
  | [%effect ()] -> ()
;;
