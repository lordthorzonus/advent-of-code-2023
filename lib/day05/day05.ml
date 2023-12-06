open Core

type map_definition = {
  source_range_start : int;
  destination_range_start : int;
  range_length : int;
}
[@@deriving sexp]

let parse_seeds_part1 (input : string) =
  input
  |> String.lsplit2_exn ~on:':'
  |> snd
  |> String.strip
  |> String.split ~on:' '
  |> List.map ~f:Int.of_string

let rec ints_to_tuples list =
  match list with
  | [] -> []
  | x :: y :: rest -> (x, y) :: ints_to_tuples rest
  | _ -> failwith "Incomplete pair in the list."

let seed_pair_to_range (pair : int * int) =
  let start_of_range, length_from_start = pair in
  let end_of_range = start_of_range + length_from_start in

  (start_of_range, end_of_range)

let parse_seeds_part2 (input : string) =
  input |> parse_seeds_part1 |> ints_to_tuples |> List.map ~f:seed_pair_to_range

let parse_map_row (input : string) =
  match String.split ~on:' ' input with
  | [ destination_range_start; source_range_start; range_length ] ->
      {
        source_range_start = Int.of_string source_range_start;
        destination_range_start = Int.of_string destination_range_start;
        range_length = Int.of_string range_length;
      }
  | _ -> failwith "Invalid map row"

let rec parse_map_definitions (almanac : string list) (map_string : string) =
  match almanac with
  | [] -> []
  | header_row :: rest ->
      if String.equal header_row map_string then
        let map_rows =
          List.take_while rest ~f:(fun x -> not (String.equal x ""))
        in
        let map_definitions = List.map ~f:parse_map_row map_rows in
        map_definitions
      else parse_map_definitions rest map_string

type almanac = {
  seed_to_soil_map : map_definition list;
  soil_to_fertilizer_map : map_definition list;
  fertilizer_to_water_map : map_definition list;
  water_to_light_map : map_definition list;
  light_to_temperature_map : map_definition list;
  temperature_to_humidity_map : map_definition list;
  humidity_to_location_map : map_definition list;
}
[@@deriving sexp]

let parse_almanac (input : string list) =
  {
    seed_to_soil_map = parse_map_definitions input "seed-to-soil map:";
    soil_to_fertilizer_map =
      parse_map_definitions input "soil-to-fertilizer map:";
    fertilizer_to_water_map =
      parse_map_definitions input "fertilizer-to-water map:";
    water_to_light_map = parse_map_definitions input "water-to-light map:";
    light_to_temperature_map =
      parse_map_definitions input "light-to-temperature map:";
    temperature_to_humidity_map =
      parse_map_definitions input "temperature-to-humidity map:";
    humidity_to_location_map =
      parse_map_definitions input "humidity-to-location map:";
  }

let rec map_destination (map_definitions : map_definition list) (source : int) =
  let in_range min max i = i >= min && i < max in

  let map_destination_with_definition map_definition =
    let source_range_end =
      map_definition.source_range_start + map_definition.range_length
    in
    let destination_range_end =
      map_definition.destination_range_start + map_definition.range_length
    in
    match source with
    | s when in_range map_definition.source_range_start source_range_end s ->
        let offset = source - map_definition.source_range_start in
        let mapped_destination =
          map_definition.destination_range_start + offset
        in
        if
          in_range map_definition.destination_range_start destination_range_end
            mapped_destination
        then Some mapped_destination
        else None
    | _ -> None
  in

  match map_definitions with
  | [] -> source
  | map_definition :: rest -> (
      match map_destination_with_definition map_definition with
      | Some mapped_destination -> mapped_destination
      | None -> map_destination rest source)

let solve_seed_to_location (almanac : almanac) (seed : int) =
  seed
  |> map_destination almanac.seed_to_soil_map
  |> map_destination almanac.soil_to_fertilizer_map
  |> map_destination almanac.fertilizer_to_water_map
  |> map_destination almanac.water_to_light_map
  |> map_destination almanac.light_to_temperature_map
  |> map_destination almanac.temperature_to_humidity_map
  |> map_destination almanac.humidity_to_location_map

let extend_to_ints ((start, stop) : int * int) =
  List.range start stop ~start:`inclusive ~stop:`exclusive

module Day05 = struct
  let solve_part1 (input : string list) =
    parse_seeds_part1 @@ List.hd_exn input
    |> List.map ~f:(solve_seed_to_location (parse_almanac input))
    |> List.min_elt ~compare:Int.compare
    |> Option.value_exn ~message:"No solution found"

  let solve_part2 (input : string list) =
    parse_seeds_part2 @@ List.hd_exn input
    |> List.concat_map ~f:extend_to_ints
    |> List.map ~f:(solve_seed_to_location (parse_almanac input))
    |> List.min_elt ~compare:Int.compare
    |> Option.value_exn ~message:"No solution found"
end

let test_input =
  [
    "seeds: 79 14 55 13";
    "";
    "seed-to-soil map:";
    "50 98 2";
    "52 50 48";
    "";
    "soil-to-fertilizer map:";
    "0 15 37";
    "37 52 2";
    "39 0 15";
    "";
    "fertilizer-to-water map:";
    "49 53 8";
    "0 11 42";
    "42 0 7";
    "57 7 4";
    "";
    "water-to-light map:";
    "88 18 7";
    "18 25 70";
    "";
    "light-to-temperature map:";
    "45 77 23";
    "81 45 19";
    "68 64 13";
    "";
    "temperature-to-humidity map:";
    "0 69 1";
    "1 0 69";
    "";
    "humidity-to-location map:";
    "60 56 37";
    "56 93 4";
  ]

let%expect_test "parse_map_row" =
  let map_row = "50 98 2" in
  let map_definition = parse_map_row map_row in
  print_s [%sexp (map_definition : map_definition)];
  [%expect
    {| ((source_range_start 98) (destination_range_start 50) (range_length 2)) |}]

let%expect_test "parse_map_definitions" =
  let map_definitions = parse_map_definitions test_input "seed-to-soil map:" in
  print_s [%sexp (map_definitions : map_definition list)];
  [%expect
    {|
        (((source_range_start 98) (destination_range_start 50) (range_length 2))
         ((source_range_start 50) (destination_range_start 52) (range_length 48))) |}]

let%expect_test "map_destination" =
  let map_definitions = [ parse_map_row "50 98 2"; parse_map_row "52 50 48" ] in
  let mapped_destination = map_destination map_definitions 98 in
  print_s [%sexp (mapped_destination : int)];
  [%expect {| 50 |}];
  let mapped_destination2 = map_destination map_definitions 99 in
  print_s [%sexp (mapped_destination2 : int)];
  [%expect {| 51 |}];
  let mapped_destination3 = map_destination map_definitions 100 in
  print_s [%sexp (mapped_destination3 : int)];
  [%expect {| 100 |}];
  let mapped_destination4 = map_destination map_definitions 49 in
  print_s [%sexp (mapped_destination4 : int)];
  [%expect {| 49 |}];
  let mapped_destination5 = map_destination map_definitions 50 in
  print_s [%sexp (mapped_destination5 : int)];
  [%expect {| 52 |}]

let%expect_test "solve day 5 part 1" =
  let solution = Day05.solve_part1 test_input in
  print_s [%sexp (solution : int)];
  [%expect {| 35 |}]

(*Commented out because of the brute force solution*)
(* let%expect_test "solve day 5 part 2" = *)
(*   let solution = Day05.solve_part2 test_input in *)
(*   print_s [%sexp (solution : int)]; *)
(*   [%expect {| 46 |}] *)
