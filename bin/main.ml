open Advent_of_code_2023.Day01
open Advent_of_code_2023.Day02
open Advent_of_code_2023.Day03
open Advent_of_code_2023.Day04
open Advent_of_code_2023.Day05
open Advent_of_code_2023.Day06
open Advent_of_code_2023.Day07
open Advent_of_code_2023.Day08
open Advent_of_code_2023.Day09
open Advent_of_code_2023.File_utils

let usage_msg = "adv2023 -d <day number>"
let input_day : int option ref = ref None
let input_file : string option ref = ref None

let speclist =
  [
    ("-d", Arg.Int (fun d -> input_day := Some d), "Day number");
    ("-i", Arg.String (fun i -> input_file := Some i), "Input file");
  ]

type answer = IntAnswer of int | StringAnswer of string

let part_not_implemented = "not implemented"

let solve_day day input =
  match day with
  | 1 ->
      (IntAnswer (Day01.solve_part1 input), IntAnswer (Day01.solve_part2 input))
  | 2 ->
      (IntAnswer (Day02.solve_part1 input), IntAnswer (Day02.solve_part2 input))
  | 3 ->
      (IntAnswer (Day03.solve_part1 input), IntAnswer (Day03.solve_part2 input))
  | 4 ->
      (IntAnswer (Day04.solve_part1 input), IntAnswer (Day04.solve_part2 input))
  | 5 ->
      (IntAnswer (Day05.solve_part1 input), IntAnswer (Day05.solve_part2 input))
  | 6 ->
      (IntAnswer (Day06.solve_part1 input), IntAnswer (Day06.solve_part2 input))
  | 7 ->
      (IntAnswer (Day07.solve_part1 input), IntAnswer (Day07.solve_part2 input))
  | 8 ->
      (IntAnswer (Day08.solve_part1 input), IntAnswer (Day08.solve_part2 input))
  | 9 ->
      (IntAnswer (Day09.solve_part1 input), IntAnswer (Day09.solve_part2 input))
  | _ -> (StringAnswer part_not_implemented, StringAnswer part_not_implemented)

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  let day = Option.get !input_day in
  let file = Option.get !input_file in
  let input = File_utils.read_file_to_list_of_strings file in

  print_endline ("The answers for day " ^ string_of_int day ^ "\n");

  match solve_day day input with
  | IntAnswer part1, IntAnswer part2 ->
      Printf.printf "part1 is: %d\n" part1;
      Printf.printf "part2 is: %d\n" part2
  | StringAnswer part1, StringAnswer part2 ->
      print_string ("part1 is: " ^ part1 ^ "\n");
      print_string ("part2 is: " ^ part2 ^ "\n")
  | IntAnswer part1, StringAnswer part2 ->
      Printf.printf "part1 is: %d\n" part1;
      print_string ("part2 is: " ^ part2 ^ "\n")
  | _ ->
      print_string "Unexpected answer shape";
      print_endline ""
