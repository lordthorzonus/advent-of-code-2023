open Core
module Network = Hashtbl.Make (String)

type network = (string * string) Network.t

let add_to_network (element_id : string) (left, right) (network : network) =
  Hashtbl.add_exn ~key:element_id ~data:(left, right) network;
  network

type navigation_instruction = First | Second

let parse_navigation_instructions (instructions : string) =
  instructions
  |> String.fold
       ~f:(fun acc c ->
         match c with
         | 'R' -> Array.append acc [| Second |]
         | 'L' -> Array.append acc [| First |]
         | _ -> acc)
       ~init:[||]

let execute_instruction (instruction : navigation_instruction)
    (nodes : string * string) =
  match instruction with
  | First -> fst nodes
  | Second -> snd nodes

let parse_network (network : string list) =
  let network_map : network = Network.create () in

  let parse_line line =
    let element_id_string, nodes_string = String.lsplit2_exn line ~on:'=' in
    let element_id = String.strip element_id_string in

    let nodes =
      nodes_string
      |> String.strip
      |> String.chop_prefix_exn ~prefix:"("
      |> String.chop_suffix_exn ~suffix:")"
      |> String.lsplit2_exn ~on:','
    in
    let left, right = (fst nodes, String.strip (snd nodes)) in

    (element_id, left, right)
  in

  List.map network ~f:parse_line
  |> List.fold
       ~f:(fun acc (element_id, left, right) ->
         add_to_network element_id (left, right) acc)
       ~init:network_map

let parse_input (input : string list) =
  let navigation_instructions =
    List.hd_exn input |> parse_navigation_instructions
  in
  let network =
    List.tl_exn input
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> parse_network
  in
  (network, navigation_instructions)

let count_path_to_exit navigation_instructions (network : network) =
  let starting_node = "AAA" in
  let max_instruction_index = Array.length navigation_instructions in

  let rec traverse_network node instruction_index count =
    let instruction = navigation_instructions.(instruction_index) in
    let nodes = Hashtbl.find_exn network node in
    let next_node = execute_instruction instruction nodes in
    let next_instruction_index =
      match instruction_index + 1 with
      | idx when idx < max_instruction_index -> idx
      | _ -> 0
    in

    match next_node with
    | "ZZZ" -> count + 1
    | _ -> traverse_network next_node next_instruction_index (count + 1)
  in

  traverse_network starting_node 0 0

module Day08 = struct
  let solve_part1 (input : string list) =
    input |> parse_input |> fun (network, navigation_instructions) ->
    count_path_to_exit navigation_instructions network
end

let test_input =
  [
    "RL";
    "";
    "AAA = (BBB, CCC)";
    "BBB = (DDD, EEE)";
    "CCC = (ZZZ, GGG)";
    "DDD = (DDD, DDD)";
    "EEE = (EEE, EEE)";
    "GGG = (GGG, GGG)";
    "ZZZ = (ZZZ, ZZZ)";
  ]

let test_input2 =
  [ "LLR"; ""; "AAA = (BBB, BBB)"; "BBB = (AAA, ZZZ)"; "ZZZ = (ZZZ, ZZZ)" ]

let%expect_test "part1" =
  let result = Day08.solve_part1 test_input in
  print_s [%sexp (result : int)];
  [%expect {| 2 |}];
  let result2 = Day08.solve_part1 test_input2 in
  print_s [%sexp (result2 : int)];
  [%expect {| 6 |}]
