let rec lists_equal eqfn l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | _, [] | [], _ -> false
  | e1::sl1, e2::sl2 ->
    if not (eqfn e1 e2) then false else (lists_equal eqfn sl1 sl2)
