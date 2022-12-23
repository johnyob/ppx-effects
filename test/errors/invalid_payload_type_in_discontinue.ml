let () =
  match%discontinue k (Invalid_argument "failed") with
  | [%effect ()] -> ()
;;
