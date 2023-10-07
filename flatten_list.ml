open Common

type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten_node acc = function
  | One el -> el::acc
  | Many l -> List.fold_left flatten_node acc l
(*
 * I don't know how idiomatic in Ocaml is reversing the list after building it,
 * I've seen it used in Elixir quite a lot.
 *)
and flatten node_list = List.rev (flatten_node [] (Many node_list))

let () =
  let flattened =
    (flatten
       [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]) in
  assert (lists_equal String.equal flattened ["a"; "b"; "c"; "d"; "e"])
