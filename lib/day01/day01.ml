let is_digit c =
  match c with
  | '1' .. '9' -> true
  | _ -> false

let filter_digits_in_string input =
  String.fold_left
    (fun acc char -> if is_digit char then char :: acc else acc)
    [] input
  |> List.rev
  |> List.to_seq
  |> String.of_seq

let pick_first_and_last_char input =
  let first_char = input.[0] in
  let last_char = input.[String.length input - 1] in

  Core.Char.to_string first_char ^ Core.Char.to_string last_char

let part_2_regexp =
  Str.regexp
    "[1-9]\\|one\\|two\\|three\\|four\\|five\\|six\\|seven\\|eight\\|nine"

let word_to_digit word =
  match word with
  | "one" -> "1"
  | "two" -> "2"
  | "three" -> "3"
  | "four" -> "4"
  | "five" -> "5"
  | "six" -> "6"
  | "seven" -> "7"
  | "eight" -> "8"
  | "nine" -> "9"
  | x -> x

let find_first_digit input =
  match Str.search_forward part_2_regexp input 0 with
  | _ -> word_to_digit (Str.matched_string input)
  | exception Not_found -> ""

let find_last_digit input =
  match Str.search_backward part_2_regexp input (String.length input - 1) with
  | _ -> word_to_digit (Str.matched_string input)
  | exception Not_found -> ""

let rec sum_list_of_ints list =
  match list with
  | [] -> 0
  | element :: remainder -> element + sum_list_of_ints remainder

let parse_calibration_value_part1 s =
  s |> filter_digits_in_string |> pick_first_and_last_char |> int_of_string

let parse_calibration_value_part2 s =
  let first_digit = find_first_digit s in
  let last_digit = find_last_digit s in

  int_of_string (first_digit ^ last_digit)

module Day01 = struct
  let solve_part1 (input : string list) =
    input |> List.map parse_calibration_value_part1 |> sum_list_of_ints

  let solve_part2 (input : string list) =
    input |> List.map parse_calibration_value_part2 |> sum_list_of_ints
end

(* Tests *)
let%expect_test "find_ints_in_string" =
  print_string (filter_digits_in_string "1sdf2");
  [%expect {| 12 |}]

let%expect_test "parse_calibration_value_part1" =
  print_int (parse_calibration_value_part1 "1sdd3dgfdg4fdf");
  [%expect {| 14 |}]

let%expect_test "parse_calibration_value_part2" =
  print_int (parse_calibration_value_part2 "zoneight234");
  [%expect "14"];
  print_int (parse_calibration_value_part2 "7r");
  [%expect "77"];
  print_int (parse_calibration_value_part2 "eighthree");
  [%expect "83"]

let test_input_part1 = [ "1abc2"; "pqr3stu8vwx"; "a1b2c3d4e5f"; "treb7uchet" ]

let test_input_part2 =
  [
    "two1nine";
    "weightwothree";
    "abcone2threexyz";
    "xtwone3four";
    "4nineeightseven2";
    "zoneight234";
    "7pqrstsixteen";
  ]

let%expect_test "part1" =
  print_int (Day01.solve_part1 test_input_part1);
  [%expect {| 142 |}]

let%expect_test "part2" =
  print_int (Day01.solve_part2 test_input_part2);
  [%expect {| 281 |}]
