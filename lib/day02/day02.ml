let test_input =
  [
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green";
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue";
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red";
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red";
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green";
  ]

type result =
  | BlueResult of int
  | GreenResult of int
  | RedResult of int
  | NoResult

type color = Blue | Green | Red
type game = { id : int; results : result list }

let cube_result_strings_to_results (result_strings : string list) =
  Core.List.map
    ~f:(fun result_string ->
      let trimmed_result_string = String.trim result_string in
      match Core.String.lsplit2 trimmed_result_string ~on:' ' with
      | Some (amount_str, "blue") -> BlueResult (int_of_string amount_str)
      | Some (amount_str, "green") -> GreenResult (int_of_string amount_str)
      | Some (amount_str, "red") -> RedResult (int_of_string amount_str)
      | _ -> NoResult)
    result_strings

let game_id_str_to_id (id_str : string) =
  match Core.String.lsplit2 id_str ~on:' ' with
  | Some (_, id) -> int_of_string id
  | None -> failwith "game id could not be parsed"

let parse_game_result_string (game_result : string) =
  game_result
  |> String.trim
  |> Core.String.split ~on:';'
  |> Core.List.map ~f:(fun subset ->
         subset
         |> String.trim
         |> Core.String.split ~on:','
         |> cube_result_strings_to_results)
  |> List.flatten

let parse_line_to_game (input : string) =
  match Core.String.lsplit2 input ~on:':' with
  | Some (game_id_str, game_results_string) ->
      ( game_id_str_to_id game_id_str,
        parse_game_result_string game_results_string )
      |> fun (id, results) -> { id; results }
  | None -> failwith "game could not be parsed"

let is_game_valid (game : game) =
  Core.List.for_all
    ~f:(fun result ->
      match result with
      | BlueResult amount -> amount <= 14
      | GreenResult amount -> amount <= 13
      | RedResult amount -> amount <= 12
      | NoResult -> true)
    game.results

let rec sum_game_ids (games : game list) =
  match games with
  | [] -> 0
  | game :: remainder -> game.id + sum_game_ids remainder

let filter_results_by_color (results : result list) ~(color : color) =
  Core.List.filter
    ~f:(fun result ->
      match result with
      | BlueResult _ when color = Blue -> true
      | GreenResult _ when color = Green -> true
      | RedResult _ when color = Red -> true
      | _ -> false)
    results

let max_amount_of_cubes_for_color (results : result list) ~(color : color) =
  results
  |> filter_results_by_color ~color
  |> Core.List.map ~f:(fun result ->
         match result with
         | BlueResult amount -> amount
         | GreenResult amount -> amount
         | RedResult amount -> amount
         | NoResult -> 0)
  |> Core.List.max_elt ~compare:Int.compare
  |> Option.value ~default:0

let power_of_min_amount_of_cubes_possible (game : game) =
  let blue_amount = max_amount_of_cubes_for_color game.results ~color:Blue in
  let green_amount = max_amount_of_cubes_for_color game.results ~color:Green in
  let red_amount = max_amount_of_cubes_for_color game.results ~color:Red in

  blue_amount * green_amount * red_amount

let rec sum_list_of_ints list =
  match list with
  | [] -> 0
  | element :: remainder -> element + sum_list_of_ints remainder

module Day02 = struct
  let solve_part1 input =
    input
    |> Core.List.map ~f:parse_line_to_game
    |> Core.List.filter ~f:is_game_valid
    |> sum_game_ids

  let solve_part2 input =
    input
    |> Core.List.map ~f:parse_line_to_game
    |> Core.List.map ~f:power_of_min_amount_of_cubes_possible
    |> sum_list_of_ints
end

let%expect_test "parse_game_result_string" =
  let parsed_game_results =
    parse_game_result_string "3 blue, 4 red; 2 green, 6 blue; 2 green"
  in

  Core.List.iter
    ~f:(fun result ->
      match result with
      | BlueResult amount -> print_int amount
      | GreenResult amount -> print_int amount
      | RedResult amount -> print_int amount
      | NoResult -> print_string "NoResult")
    parsed_game_results;
  [%expect {| 34262 |}]

let%expect_test "parse_line_to_game" =
  let parsed_game =
    parse_line_to_game "Game 1: 3 blue, 4 red; 2 green, 6 blue"
  in

  print_int parsed_game.id;
  [%expect {| 1 |}];
  print_int (List.length parsed_game.results);
  [%expect {| 4 |}]

let%expect_test "part_1" =
  let result = Day02.solve_part1 test_input in

  print_int result;
  [%expect {| 8 |}]

let%expect_test "filter_results_by_color" =
  let parsed_game =
    parse_line_to_game "Game 1: 3 blue, 4 red; 2 green, 6 blue; 2 green"
  in

  let blue_results = filter_results_by_color ~color:Blue parsed_game.results in
  let green_results =
    filter_results_by_color ~color:Green parsed_game.results
  in
  let red_results = filter_results_by_color ~color:Red parsed_game.results in

  print_int (List.length blue_results);
  [%expect {| 2 |}];
  print_int (List.length green_results);
  [%expect {| 2 |}];
  print_int (List.length red_results);
  [%expect {| 1 |}]

let%expect_test "part_2" =
  let result = Day02.solve_part2 test_input in

  print_int result;
  [%expect {| 2286 |}]
