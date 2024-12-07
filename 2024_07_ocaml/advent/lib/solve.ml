open! Base
open! Core

let sample_1 =
  {|190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
|}
  |> String.split_lines
;;

type input =
  { goal : int
  ; opes : int list
  }
[@@deriving sexp]

let parse l =
  List.map l ~f:(String.split_on_chars ~on:[ ':'; ' ' ])
  |> List.map ~f:(function
    | goal :: "" :: opes ->
      let goal = Int.of_string goal in
      let opes = List.map opes ~f:Int.of_string in
      { goal; opes }
    | split -> raise_s [%message (split : string list)])
;;

let apply a b = function
  | `Mul -> a * b
  | `Add -> a + b
  | `Con ->
    let open Int in
    String.concat [ to_string a; to_string b ] |> of_string
;;

let rec can_reach goal current ops = function
  | [] -> Int.equal goal current
  | a :: rest ->
    List.exists ops ~f:(fun op -> can_reach goal (apply current a op) ops rest)
;;

let partx (lines : string list) ops =
  lines
  |> parse
  |> List.filter ~f:(fun input -> can_reach input.goal 0 ops input.opes)
  |> List.sum (module Int) ~f:(fun input -> input.goal)
;;

let part1 (lines : string list) = partx lines [ `Mul; `Add ]
let part2 (lines : string list) = partx lines [ `Mul; `Add; `Con ]

let%expect_test _ =
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect {|
    ("part1 sample_1" 3749)
    ("part2 sample_1" 11387)
    |}]
;;
