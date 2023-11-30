open Advent_of_code_2023.Day01

let usage_msg = "adv2023 -d <day number>"
let day : int option ref = ref None
let speclist = [
    ("-d", Arg.Int (fun d -> day:= Some(d)) , "Day number")
]

let solve_day: int -> string = function 
    | 1 -> Day01.solve()
    | d -> "Day " ^ (string_of_int d) ^ " not implemented"

let () =
    Arg.parse speclist (fun _ -> ()) usage_msg;
    match !day with
    | None -> print_endline "No day specified"
    | Some d -> print_endline (solve_day d);
