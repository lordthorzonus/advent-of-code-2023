open Core

type card = { id : int; winning_numbers : int list; numbers_in_card : int list }
type card_result_part = { id : int; copy_ids : int list }

let normalize_int_string_to_int s = s |> String.strip |> int_of_string

let parse_card_numbers (numbers_string : string) =
  numbers_string
  |> String.strip
  |> String.split ~on:' '
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> List.map ~f:normalize_int_string_to_int

let parse_line_to_card (line : string) =
  let card_id_string, card_string = String.lsplit2_exn ~on:':' line in
  let _, card_id = String.lsplit2_exn ~on:' ' card_id_string in
  let winning_numbers_string, numbers_in_card_string =
    String.lsplit2_exn ~on:'|' card_string
  in
  {
    id = normalize_int_string_to_int card_id;
    winning_numbers = parse_card_numbers winning_numbers_string;
    numbers_in_card = parse_card_numbers numbers_in_card_string;
  }

let list_contains_entry (list : 'a list) (entry : 'a) =
  List.exists ~f:(fun x -> x = entry) list

let count_matched_numbers (card : card) =
  card.numbers_in_card
  |> List.filter ~f:(list_contains_entry card.winning_numbers)
  |> List.length

let calculate_score (matches : int) =
  match matches with
  | 0 -> 0
  | 1 -> 1
  | 2 -> 2
  | n -> Int.pow 2 (n - 1)

let score_card (card : card) = card |> count_matched_numbers |> calculate_score
let array_sum (array : int array) = Array.fold ~init:0 ~f:( + ) array

let count_number_of_winned_cards cards =
  let cards_count = Array.create ~len:(List.length cards) 1 in
  let matches = List.map ~f:count_matched_numbers cards in

  List.iteri
    ~f:(fun i number_of_matches ->
      for j = i to i + number_of_matches - 1 do
        cards_count.(j + 1) <- cards_count.(j + 1) + cards_count.(i)
      done)
    matches;

  cards_count |> array_sum

module Day04 = struct
  let solve_part1 (input : string list) =
    input
    |> List.map ~f:parse_line_to_card
    |> List.map ~f:score_card
    |> List.reduce_exn ~f:( + )

  let solve_part2 (input : string list) =
    input |> List.map ~f:parse_line_to_card |> count_number_of_winned_cards
end

let test_input =
  [
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53";
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19";
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1";
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83";
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36";
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11";
  ]

let%expect_test "parse_line_to_card" =
  let card =
    parse_line_to_card "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
  in
  print_s [%message (card.id : int)];
  [%expect {| (card.id 1) |}];
  print_s [%message (card.winning_numbers : int list)];
  [%expect {| (card.winning_numbers (41 48 83 86 17)) |}];
  print_s [%message (card.numbers_in_card : int list)];
  [%expect {| (card.numbers_in_card (83 86 6 31 17 9 48 53)) |}]

let%expect_test "calculate_score" =
  print_s [%message (calculate_score 0 : int)];
  [%expect {| ("calculate_score 0" 0) |}];
  print_s [%message (calculate_score 1 : int)];
  [%expect {| ("calculate_score 1" 1) |}];
  print_s [%message (calculate_score 2 : int)];
  [%expect {| ("calculate_score 2" 2) |}];
  print_s [%message (calculate_score 3 : int)];
  [%expect {| ("calculate_score 3" 4) |}];
  print_s [%message (calculate_score 4 : int)];
  [%expect {| ("calculate_score 4" 8) |}];
  print_s [%message (calculate_score 5 : int)];
  [%expect {| ("calculate_score 5" 16) |}]

let%expect_test "score_card" =
  let card =
    parse_line_to_card "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
  in
  print_s [%message (score_card card : int)];
  [%expect {| ("score_card card" 2) |}];

  let card2 =
    parse_line_to_card "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
  in
  print_s [%message (score_card card2 : int)];
  [%expect {| ("score_card card2" 0) |}]

let%expect_test "solve_part1" =
  print_s [%message (Day04.solve_part1 test_input : int)];
  [%expect {| ("Day04.solve_part1 test_input" 13) |}]

let%expect_test "count_number_of_winned_cards" =
  let cards = test_input |> List.map ~f:parse_line_to_card in
  print_s [%message (count_number_of_winned_cards cards : int)];
  [%expect {| ("count_number_of_winned_cards cards" 30) |}]
