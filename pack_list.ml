open Common

let rec pack_impl acc1 acc2 prev_item = function
  | [] -> acc2::acc1
  | el::rest ->
    if (String.equal el prev_item)
    then pack_impl acc1 (el::acc2) el rest
    else pack_impl (acc2::acc1) [el] el rest
let pack l =
  match l with
  | [] -> []
  | [el] -> [[el]]
  | el::rest -> (List.rev (pack_impl [] [el] el rest))

let () =
  let input_list = [
    "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"
  ] in
  let output_list = pack input_list
  and expected_list = [
    ["a"; "a"; "a"; "a"];
    ["b"];
    ["c"; "c"];
    ["a"; "a"];
    ["d"; "d"];
    ["e"; "e"; "e"; "e"]]
  in
  assert (lists_equal (lists_equal String.equal) output_list expected_list)
