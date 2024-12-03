open! Base
open! Core

let sample_1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
let sample_2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
let regex = Re2.of_string "mul\\(\\d{1,3},\\d{1,3}\\)"
let regex' = Re2.of_string "mul\\(\\d{1,3},\\d{1,3}\\)|don't\\(\\)|do\\(\\)"
let parse s = Re2.find_all_exn regex s
let parse' s = Re2.find_all_exn regex' s

let mult s =
  match String.split_on_chars s ~on:[ '('; ','; ')' ] with
  | [ _; a; b; "" ] -> Int.of_string a * Int.of_string b
  | res -> raise_s [%message "Invalid string" (s : string) (res : string list)]
;;

let sum = List.sum (module Int)
let part1 (lines : string list) = sum lines ~f:(fun l -> parse l |> sum ~f:mult)
let part2 (_lines : string list) = 0

let%expect_test _ =
  let result = String.split_lines sample_1 |> part1 in
  print_s [%message (parse sample_1 : string list)];
  print_s [%message (parse' sample_2 : string list)];
  print_s [%message (result : int)];
  [%expect
    {|
    ("parse sample_1" ("mul(2,4)" "mul(5,5)" "mul(11,8)" "mul(8,5)"))
    ("parse' sample_2"
     ("mul(2,4)" "don't()" "mul(5,5)" "mul(11,8)" "do()" "mul(8,5)"))
    (result 161)
    |}]
;;
