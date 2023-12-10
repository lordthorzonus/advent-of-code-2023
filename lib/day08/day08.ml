open Core
module Network = Hashtbl.Make (String)

let gcd a b =
  let rec gcd' a b = if b = 0 then a else gcd' b (a mod b) in
  if a >= 0 && b >= 0 then gcd' a b
  else invalid_arg "gcd: arguments must be non-negative"

let lcm a b = if a = 0 || b = 0 then 0 else abs (a * b) / gcd a b
let lcm_list = List.fold ~f:lcm ~init:1

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

let count_path_to_exit navigation_instructions starting_node is_ending_node
    (network : network) =
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
    | s when is_ending_node s -> count + 1
    | _ -> traverse_network next_node next_instruction_index (count + 1)
  in

  traverse_network starting_node 0 0

let string_ends_with ~suffix str = String.equal (String.suffix str 1) suffix

let get_all_nodes_ending_with ~char (network : network) =
  Hashtbl.filter_keys network ~f:(fun key -> string_ends_with ~suffix:char key)
  |> Hashtbl.keys

module Day08 = struct
  let solve_part1 (input : string list) =
    input |> parse_input |> fun (network, navigation_instructions) ->
    count_path_to_exit navigation_instructions "AAA"
      (fun node -> String.equal node "ZZZ")
      network

  let solve_part2 (input : string list) =
    input |> parse_input |> fun (network, navigation_instructions) ->
    let all_starting_nodes = get_all_nodes_ending_with ~char:"A" network in
    List.map all_starting_nodes ~f:(fun node ->
        count_path_to_exit navigation_instructions node
          (fun node -> string_ends_with ~suffix:"Z" node)
          network)
    |> lcm_list
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

let test_input3 =
  [
    "LR";
    "";
    "11A = (11B, XXX)";
    "11B = (XXX, 11Z)";
    "11Z = (11B, XXX)";
    "22A = (22B, XXX)";
    "22B = (22C, 22C)";
    "22C = (22Z, 22Z)";
    "22Z = (22B, 22B)";
    "XXX = (XXX, XXX)";
  ]

let%expect_test "part1" =
  let result = Day08.solve_part1 test_input in
  print_s [%sexp (result : int)];
  [%expect {| 2 |}];
  let result2 = Day08.solve_part2 test_input2 in
  print_s [%sexp (result2 : int)];
  [%expect {| 6 |}]

let%expect_test "part2" =
  let result = Day08.solve_part2 test_input3 in
  print_s [%sexp (result : int)];
  [%expect {| 6 |}]
