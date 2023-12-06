open Core

type race = { time : int; distance : int }

let solve_quadradic_equation a b c =
  let discriminant = (b * b) - (4 * a * c) in
  if discriminant < 0 then None
  else
    let sqrt_discriminant = sqrt (float_of_int discriminant) in
    let x1 = (float_of_int (-b) +. sqrt_discriminant) /. float_of_int (2 * a) in
    let x2 = (float_of_int (-b) -. sqrt_discriminant) /. float_of_int (2 * a) in
    Some (x1, x2)

let solve_button_press_amounts (race : race) =
  let a = -1 in
  let b = race.time in
  let c = -race.distance in

  let result = solve_quadradic_equation a b c in

  match result with
  | None -> failwith "No solution"
  | Some (x1, x2) ->
      let min_amount_to_press = Float.iround_exn ~dir:`Down x1 + 1 in
      let max_amount_to_press = Float.iround_exn ~dir:`Up x2 - 1 in
      List.range ~start:`inclusive ~stop:`inclusive min_amount_to_press
        max_amount_to_press

let count_ways_to_win_race race = solve_button_press_amounts race |> List.length

let number_list_from_string str =
  str
  |> String.split ~on:' '
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> List.map ~f:Int.of_string

let parse_input_part1 (input : string list) =
  let parse_line line =
    line |> String.lsplit2_exn ~on:':' |> snd |> number_list_from_string
  in
  let times = List.hd_exn input |> parse_line in
  let distances = List.nth_exn input 1 |> parse_line in

  List.zip_exn times distances
  |> List.map ~f:(fun (time, distance) -> { time; distance })

let parse_input_part2 (input : string list) =
  let parse_line line =
    line
    |> String.lsplit2_exn ~on:':'
    |> snd
    |> String.filter ~f:(fun c -> not (Char.is_whitespace c))
    |> Int.of_string
  in
  let time = List.hd_exn input |> parse_line in
  let distance = List.nth_exn input 1 |> parse_line in
  { time; distance }

module Day06 = struct
  let solve_part1 input =
    input
    |> parse_input_part1
    |> List.map ~f:count_ways_to_win_race
    |> List.fold ~init:1 ~f:( * )

  let solve_part2 input = input |> parse_input_part2 |> count_ways_to_win_race
end

let test_input = [ "Time:      7  15   30"; "Distance:  9  40  200" ]

let%expect_test "solve_button_press_amounts" =
  let result = solve_button_press_amounts { time = 7; distance = 9 } in
  print_s [%sexp (result : int list)];
  [%expect {| (2 3 4 5) |}];

  let result2 = solve_button_press_amounts { time = 15; distance = 40 } in
  print_s [%sexp (result2 : int list)];
  [%expect {| (4 5 6 7 8 9 10 11) |}];

  let result3 = solve_button_press_amounts { time = 30; distance = 200 } in
  print_s [%sexp (result3 : int list)];
  [%expect {| (11 12 13 14 15 16 17 18 19) |}]

let%expect_test "part1" =
  let result = Day06.solve_part1 test_input in
  print_s [%sexp (result : int)];
  [%expect {| 288 |}]

let%expect_test "part2" =
  let result = Day06.solve_part2 test_input in
  print_s [%sexp (result : int)];
  [%expect {| 71503 |}]
