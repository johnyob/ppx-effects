let () =
  try () with
  | [%effect ()] -> ()
;;
