open Core

let char_to_card_value ?(joker = false) = function
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'T' -> 10
  | 'J' -> if joker then 1 else 11
  | 'Q' -> 12
  | 'K' -> 13
  | 'A' -> 14
  | _ -> failwith "Invalid card value"

type hand_type =
  | FiveOfAKind
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard
[@@deriving sexp]

type hand = { hand_type : hand_type; cards : int list; bid : int }
[@@deriving sexp]

module Hand = struct
  type t = hand

  let group_cards cards =
    cards
    |> List.sort ~compare:(fun a b -> Int.compare b a)
    |> List.group ~break:( <> )
    |> List.sort ~compare:(fun a b ->
           Int.compare (List.length b) (List.length a))

  let apply_jokers hand =
    let cards_without_jokers =
      List.filter hand.cards ~f:(fun card -> card <> 1)
    in
    let card_groups = group_cards cards_without_jokers in
    let joker_count =
      List.length hand.cards - List.length cards_without_jokers
    in

    match joker_count with
    | 4
    | 5 ->
        { hand with hand_type = FiveOfAKind }
    | 3 -> (
        match card_groups with
        | [ [ _; _ ] ] -> { hand with hand_type = FiveOfAKind }
        | _ -> { hand with hand_type = FourOfAKind })
    | 2 -> (
        match card_groups with
        | [ [ _; _; _ ] ] -> { hand with hand_type = FiveOfAKind }
        | [ [ _; _ ]; [ _ ] ] -> { hand with hand_type = FourOfAKind }
        | _ -> { hand with hand_type = ThreeOfAKind })
    | 1 -> (
        match card_groups with
        | [ [ _; _; _; _ ] ] -> { hand with hand_type = FiveOfAKind }
        | [ [ _; _; _ ]; [ _ ] ] -> { hand with hand_type = FourOfAKind }
        | [ [ _; _ ]; [ _; _ ] ] -> { hand with hand_type = FullHouse }
        | [ [ _; _ ]; [ _ ]; [ _ ] ] -> { hand with hand_type = ThreeOfAKind }
        | _ -> { hand with hand_type = OnePair })
    | _ -> hand

  let parse_hand ?(joker = false) hand =
    let card_string, bid_string = String.lsplit2_exn hand ~on:' ' in
    let cards =
      String.to_list card_string |> List.map ~f:(char_to_card_value ~joker)
    in

    let card_groups = group_cards cards in

    let bid = int_of_string bid_string in

    let resolve_hand_type = function
      | [ [ _; _; _; _; _ ] ] -> FiveOfAKind
      | [ [ _; _; _; _ ]; [ _ ] ] -> FourOfAKind
      | [ [ _; _; _ ]; [ _; _ ] ] -> FullHouse
      | [ [ _; _; _ ]; [ _ ]; [ _ ] ] -> ThreeOfAKind
      | [ [ _; _ ]; [ _; _ ]; [ _ ] ] -> TwoPair
      | [ [ _; _ ]; [ _ ]; [ _ ]; [ _ ] ] -> OnePair
      | _ -> HighCard
    in

    match joker with
    | true ->
        apply_jokers { hand_type = resolve_hand_type card_groups; cards; bid }
    | _ -> { hand_type = resolve_hand_type card_groups; cards; bid }

  let to_int hand =
    match hand.hand_type with
    | FiveOfAKind -> 7
    | FourOfAKind -> 6
    | FullHouse -> 5
    | ThreeOfAKind -> 4
    | TwoPair -> 3
    | OnePair -> 2
    | HighCard -> 1

  let compare hand1 hand2 =
    match (to_int hand1, to_int hand2) with
    | hand1_int, hand2_int when hand1_int > hand2_int -> 1
    | hand1_int, hand2_int when hand1_int < hand2_int -> -1
    | _ -> List.compare Int.compare hand1.cards hand2.cards
end

module Day07 = struct
  let solve_part1 input =
    input
    |> List.map ~f:(Hand.parse_hand ~joker:false)
    |> List.sort ~compare:Hand.compare
    |> List.foldi ~init:0 ~f:(fun index acc hand ->
           let multiplier = index + 1 in
           acc + (hand.bid * multiplier))

  let solve_part2 input =
    input
    |> List.map ~f:(Hand.parse_hand ~joker:true)
    |> List.sort ~compare:Hand.compare
    |> List.foldi ~init:0 ~f:(fun index acc hand ->
           let multiplier = index + 1 in
           acc + (hand.bid * multiplier))
end

let test_input =
  [ "32T3K 765"; "T55J5 684"; "KK677 28"; "KTJJT 220"; "QQQJA 483" ]

let%expect_test "parse_hand" =
  let hand = Hand.parse_hand ~joker:false "32T3K 765" in
  print_s [%sexp (hand : hand)];
  [%expect {| ((hand_type OnePair) (cards (3 2 10 3 13)) (bid 765)) |}];
  let hand2 = Hand.parse_hand ~joker:false "T55J5 684" in
  print_s [%sexp (hand2 : hand)];
  [%expect {| ((hand_type ThreeOfAKind) (cards (10 5 5 11 5)) (bid 684)) |}];
  let hand3 = Hand.parse_hand ~joker:false "KK677 28" in
  print_s [%sexp (hand3 : hand)];
  [%expect {| ((hand_type TwoPair) (cards (13 13 6 7 7)) (bid 28)) |}]

let%expect_test "solve_part1" =
  let result = Day07.solve_part1 test_input in
  print_s [%sexp (result : int)];
  [%expect {| 6440 |}]

let%expect_test "solve_part2" =
  let result = Day07.solve_part2 test_input in
  print_s [%sexp (result : int)];
  [%expect {| 5905 |}]
