open Core

type schematic_symbol = Digit of int | Symbol of char

let char_to_schematic_symbol (c : char) =
  match c with
  | '0' .. '9' -> (
      match Char.get_digit c with
      | Some d -> Some (Digit d)
      | _ -> None)
  | '.' -> None
  | _ -> Some (Symbol c)

let input_to_engine_schematic (input : string list) =
  let width =
    match List.hd input with
    | Some s -> String.length s
    | None -> 0
  in
  let height = List.length input in
  let matrix = Array.make_matrix ~dimy:height ~dimx:width None in

  List.iteri input ~f:(fun row_index row_value ->
      String.iteri row_value ~f:(fun column_index column_value ->
          matrix.(row_index).(column_index) <-
            char_to_schematic_symbol column_value));

  matrix

let is_symbol symbol =
  match symbol with
  | Some (Symbol _) -> true
  | _ -> false

type valid_part_number_position = { x : int; y : int }

let print_schematic_symbol (symbol : schematic_symbol option) =
  match symbol with
  | Some (Digit d) -> print_s [%message (d : int)]
  | Some (Symbol s) -> print_s [%message (s : char)]
  | None -> print_s [%message "None"]

let directions =
  [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]

let get_adjacent_entries x y engine_schematic =
  let get_entry (dx, dy) =
    let new_x, new_y = (x + dx, y + dy) in
    if
      new_x >= 0
      && new_x < Array.length engine_schematic.(y)
      && new_y >= 0
      && new_y < Array.length engine_schematic
    then engine_schematic.(new_y).(new_x)
    else None
  in

  List.map directions ~f:get_entry

let has_symbol_adjacent_to_digit
    (engine_schematic : schematic_symbol option array array)
    (position : valid_part_number_position) =
  let x = position.x in
  let y = position.y in

  let entries_contain_symbol =
    engine_schematic |> get_adjacent_entries x y |> List.exists ~f:is_symbol
  in

  match engine_schematic.(y).(x) with
  | Some (Digit _) -> if entries_contain_symbol then true else false
  | _ -> false

type number = { positions : int list; value : int }

let print_number = function
  | { positions; value } ->
      print_s [%message (positions : int list) (value : int)]

let get_numbers_adjacent_to_gear (numbers_around : number list)
    (row_index : int) (gear_position : valid_part_number_position) =
  let x, y = (gear_position.x, gear_position.y) in
  let positions_in_neighborhood =
    List.map directions ~f:(fun (dx, dy) -> (x + dx, y + dy))
  in
  let numbers_in_neighborhood =
    List.filter numbers_around ~f:(fun { positions; _ } ->
        List.exists positions ~f:(fun position ->
            List.exists positions_in_neighborhood
              ~f:(fun (neighbor_x, neighbor_y) ->
                neighbor_x = position && neighbor_y = row_index)))
  in

  numbers_in_neighborhood

let get_numbers_from_schematic_row
    (engine_schematic_row : schematic_symbol option array) =
  let columns = Array.length engine_schematic_row in
  let rec traverse (x : int) (current_number : number option)
      (numbers : number list) =
    if x < columns then
      match engine_schematic_row.(x) with
      | Some (Digit d) -> (
          match current_number with
          | Some { positions; value } ->
              traverse (x + 1)
                (Some { positions = x :: positions; value = (value * 10) + d })
                numbers
          | None ->
              traverse (x + 1) (Some { positions = [ x ]; value = d }) numbers)
      | Some (Symbol _)
      | None -> (
          match current_number with
          | Some n -> traverse (x + 1) None (n :: numbers)
          | None -> traverse (x + 1) None numbers)
    else
      match current_number with
      | Some n -> List.rev (n :: numbers)
      | None -> List.rev numbers
  in
  traverse 0 None []

let get_valid_part_numbers
    (engine_schematic : schematic_symbol option array array) =
  let rows = Array.length engine_schematic in
  let filter_valid_numbers (numbers : number list) ~(y : int) =
    List.filter numbers ~f:(fun { positions; _ } ->
        List.exists positions ~f:(fun position ->
            has_symbol_adjacent_to_digit engine_schematic { x = position; y }))
  in
  let rec traverse (y : int) (collected_numbers : number list) =
    if y < rows then
      let row = engine_schematic.(y) in
      let numbers = get_numbers_from_schematic_row row in
      let valid_numbers = numbers |> filter_valid_numbers ~y in
      traverse (y + 1) (List.append collected_numbers valid_numbers)
    else collected_numbers
  in
  traverse 0 []

type numbers_in_row = { row_index : int; numbers : number list }

let list_of_ints_to_int (l : int list) =
  List.fold l ~init:0 ~f:(fun acc x -> (acc * 10) + x)

let get_gear_ratios_for_valid_gear_positions
    (engine_schematic : schematic_symbol option array array) =
  let rows = Array.length engine_schematic in
  let gear_positions = ref [] in
  let numbers_in_row : numbers_in_row list ref = ref [] in
  let gear_ratios : int list ref = ref [] in

  for y = 0 to rows - 1 do
    let row = engine_schematic.(y) in
    for x = 0 to Array.length row - 1 do
      match row.(x) with
      | Some (Symbol '*') -> gear_positions := { x; y } :: !gear_positions
      | _ -> ()
    done;
    numbers_in_row :=
      { row_index = y; numbers = get_numbers_from_schematic_row row }
      :: !numbers_in_row
  done;

  List.iter !gear_positions ~f:(fun gear_position ->
      let rows_around_gear =
        [ gear_position.y - 1; gear_position.y; gear_position.y + 1 ]
      in
      let numbers_around =
        List.filter !numbers_in_row ~f:(fun { row_index; _ } ->
            List.exists rows_around_gear ~f:(fun row -> row = row_index))
      in

      let numbers_adjacent_to_gear =
        List.map numbers_around ~f:(fun { numbers; row_index } ->
            get_numbers_adjacent_to_gear numbers row_index gear_position)
        |> List.concat
      in

      if List.length numbers_adjacent_to_gear = 2 then
        gear_ratios :=
          (List.hd_exn numbers_adjacent_to_gear).value
          * (List.last_exn numbers_adjacent_to_gear).value
          :: !gear_ratios
      else ());

  !gear_ratios

module Day03 = struct
  let solve_part1 (input : string list) : int =
    input
    |> input_to_engine_schematic
    |> get_valid_part_numbers
    |> List.map ~f:(fun { value; _ } -> value)
    |> List.reduce_exn ~f:( + )

  let solve_part2 (input : string list) : int =
    input
    |> input_to_engine_schematic
    |> get_gear_ratios_for_valid_gear_positions
    |> List.reduce_exn ~f:( + )
end

let test_input =
  [
    "467..114..";
    "...*......";
    "..35..633.";
    "......#...";
    "617*......";
    ".....+.58.";
    "..592.....";
    "......755.";
    "...$.*....";
    ".664.598..";
  ]

let%expect_test "has_symbol_adjacent_to_digit" =
  let engine_schematic = input_to_engine_schematic test_input in
  let result = has_symbol_adjacent_to_digit engine_schematic { x = 3; y = 2 } in
  print_s [%message (result : bool)];
  [%expect "(result true)"]

let%expect_test "get_valid_part_numbers" =
  let engine_schematic = input_to_engine_schematic test_input in
  let valid_part_numbers = get_valid_part_numbers engine_schematic in
  List.iter valid_part_numbers ~f:(fun { positions; value } ->
      print_s [%message (positions : int list) (value : int)]);
  [%expect
    {| 
((positions (2 1 0)) (value 467))
((positions (3 2)) (value 35))
((positions (8 7 6)) (value 633))
((positions (2 1 0)) (value 617))
((positions (4 3 2)) (value 592))
((positions (8 7 6)) (value 755))
((positions (3 2 1)) (value 664))
((positions (7 6 5)) (value 598))
        |}]

let%expect_test "get_numbers_from_schematic_row" =
  let schematic_row =
    [|
      Some (Digit 1);
      Some (Digit 2);
      Some (Digit 3);
      Some (Symbol '+');
      Some (Digit 4);
      Some (Digit 5);
      Some (Digit 6);
      Some (Symbol '*');
      Some (Digit 7);
      Some (Digit 8);
      Some (Digit 9);
    |]
  in
  let numbers = get_numbers_from_schematic_row schematic_row in
  List.iter numbers ~f:(fun { positions; value } ->
      print_s [%message (positions : int list) (value : int)]);
  [%expect
    {| 
        ((positions (2 1 0)) (value 123)) 
        ((positions (6 5 4)) (value 456)) 
        ((positions (10 9 8)) (value 789)) 
    |}]

let%expect_test "solve_part1" =
  let result = Day03.solve_part1 test_input in
  print_s [%message (result : int)];
  [%expect "(result 4361)"]

let%expect_test "get_part_numbers_for_valid_gear_positions" =
  let engine_schematic = input_to_engine_schematic test_input in
  let gear_ratios = get_gear_ratios_for_valid_gear_positions engine_schematic in
  List.iter gear_ratios ~f:(fun gear_ratio ->
      print_s [%message (gear_ratio : int)]);
  [%expect {| 
    (gear_ratio 16345)
    (gear_ratio 451490)
        |}]

let%expect_test "solve_part2" =
  let result = Day03.solve_part2 test_input in
  print_s [%message (result : int)];
  [%expect "(result 467835)"]
