open! Base
open! Core

let sample_1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
let sample_2 = "xmul(2,4)&mul[3,7]!^don't()mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
let regex = Re2.of_string "mul\\(\\d{1,3},\\d{1,3}\\)"
let regex' = Re2.of_string "mul\\(\\d{1,3},\\d{1,3}\\)|don't\\(\\)|do\\(\\)"
let parse = Re2.find_all_exn
let sum = List.sum (module Int)

let mult s =
  match String.split_on_chars s ~on:[ '('; ','; ')' ] with
  | [ _; a; b; "" ] -> Int.of_string a * Int.of_string b
  | res -> raise_s [%message "Invalid string" (s : string) (res : string list)]
;;

let rec only_enabled res enabled lines =
  match lines, enabled with
  | [], _ -> List.rev res
  | "don't()" :: l, _ -> only_enabled res false l
  | "do()" :: l, _ -> only_enabled res true l
  | _ :: l, false -> only_enabled res false l
  | s :: l, true -> only_enabled (s :: res) true l
;;

let parse' regex s = Re2.find_all_exn regex s |> only_enabled [] true
let part1 (lines : string list) = String.concat lines |> parse regex |> sum ~f:mult
let part2 (lines : string list) = String.concat lines |> parse' regex' |> sum ~f:mult

let%expect_test _ =
  let result = String.split_lines sample_1 |> part1 in
  let parsed = parse regex sample_1 in
  let parsed' = parse' regex' sample_2 in
  let result' = String.split_lines sample_2 |> part2 in
  print_s [%message (parsed : string list)];
  print_s [%message (parsed' : string list)];
  print_s [%message (result : int)];
  print_s [%message (result' : int)];
  [%expect
    {|
    (parsed ("mul(2,4)" "mul(5,5)" "mul(11,8)" "mul(8,5)"))
    (parsed' ("mul(2,4)" "mul(8,5)"))
    (result 161)
    (result' 48)
    |}]
;;
