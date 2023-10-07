open Common

let rec compress_impl acc prev_item = function
  | [] -> acc
  | el::rest ->
    (compress_impl
       (if (String.equal el prev_item) then acc else el::acc)
       el
       rest)
let compress l =
  match l with
  | [] -> []
  | el::rest -> el :: (List.rev (compress_impl [] el rest))

let () =
  let input_list = [
    "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"
  ] in
  let output_list = compress input_list in
  assert (lists_equal String.equal output_list ["a"; "b"; "c"; "a"; "d"; "e"])
