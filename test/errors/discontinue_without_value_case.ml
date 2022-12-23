let () =
  match%discontinue k (Invalid_argument "fail") with
  | [%effect? _] -> ()
;;