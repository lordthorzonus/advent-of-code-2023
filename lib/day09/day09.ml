open Core

let calculate_next_line (line : int list) =
  let rec aux (line : int list) (acc : int list) =
    match line with
    | [] -> acc
    | _ :: [] -> acc
    | [ x; y ] -> (y - x) :: acc
    | x :: y :: rest -> aux (y :: rest) ((y - x) :: acc)
  in
  aux line [] |> List.rev

let input_line_to_list (line : string) =
  line |> String.split ~on:' ' |> List.map ~f:int_of_string

let predict_next_value (line : int list) =
  let is_end = List.for_all ~f:(fun x -> x = 0) in
  let rec resolve_history (line : int list) (history : int list list) =
    match line with
    | [] -> history
    | l ->
        let next_line = calculate_next_line l in
        if is_end next_line then l :: history
        else resolve_history next_line (l :: history)
  in

  let rec resolve_next_value (history : int list list) (acc : int) =
    match history with
    | [] -> acc
    | line :: rest ->
        let next_value = acc + List.last_exn line in
        resolve_next_value rest next_value
  in

  let history = resolve_history line [] in
  resolve_next_value history 0

module Day09 = struct
  let solve_part1 (input : string list) =
    input
    |> List.map ~f:input_line_to_list
    |> List.map ~f:predict_next_value
    |> List.fold ~init:0 ~f:( + )

  let solve_part2 (input : string list) =
    input
    |> List.map ~f:input_line_to_list
    |> List.map ~f:List.rev
    |> List.map ~f:predict_next_value
    |> List.fold ~init:0 ~f:( + )
end

let test_input = [ "0 3 6 9 12 15"; "1 3 6 10 15 21"; "10 13 16 21 30 45" ]

let%expect_test "calculate_next_line" =
  let result = calculate_next_line [ 0; 3; 6; 9; 12; 15 ] in
  print_s [%sexp (result : int list)];
  [%expect {| (3 3 3 3 3) |}];
  let result2 = calculate_next_line [ 3; 3; 3; 3; 3 ] in
  print_s [%sexp (result2 : int list)];
  [%expect {| (0 0 0 0) |}]

let%expect_test "predict_next_value" =
  let result = predict_next_value [ 0; 3; 6; 9; 12; 15 ] in
  print_s [%sexp (result : int)];
  [%expect {| 18 |}];
  let result2 = predict_next_value [ 1; 3; 6; 10; 15; 21 ] in
  print_s [%sexp (result2 : int)];
  [%expect {| 28 |}]

let%expect_test "part1" =
  let result = Day09.solve_part1 test_input in
  print_s [%sexp (result : int)];
  [%expect {| 114 |}]

let%expect_test "part2" =
  let result = predict_next_value (List.rev [ 10; 13; 16; 21; 30; 45 ]) in
  print_s [%sexp (result : int)];
  [%expect {| 5 |}];
  let result2 = predict_next_value (List.rev [ 1; 3; 6; 10; 15; 21 ]) in
  print_s [%sexp (result2 : int)];
  [%expect {| 0 |}];
  let result3 = Day09.solve_part2 test_input in
  print_s [%sexp (result3 : int)];
  [%expect {| 2 |}]
