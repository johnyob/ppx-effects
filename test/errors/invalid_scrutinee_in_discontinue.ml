let () =
  match%discontinue () with
  | _ -> ()
  | [%effect? _] -> ()
;;