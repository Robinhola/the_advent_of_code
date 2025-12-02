open! Base
open! Core

let sample_1 =
  {|11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124|}
  |> String.split_lines
;;

let _sample_1 = {|1491-1766|} |> String.split_lines
let ranges l = String.split ~on:',' l

let range s =
  match String.split ~on:'-' s with
  | [ a; b ] -> Int.of_string a, Int.of_string b
  | _ -> raise_s [%message "hallo" (s : string)]
;;

let intlog10 a = a |> Float.of_int |> Float.log10 |> Int.of_float
let mul10 a = Int.pow 10 (intlog10 a + 1) * a

let div10 a =
  let log = intlog10 a in
  let half = (log + 1) / 2 in
  a / Int.pow 10 half
;;

let test (a, b) x =
  let () = if a >= b then raise_s [%message "oops"] in
  let value = x + mul10 x in
  match a <= value, value <= b with
  | false, _ -> false
  | true, true -> true
  | true, false -> false
;;

let rec test_range (a, b) x res =
  (*print_s [%message ((a, b) : int * int) (x : int)];*)
  let next_value = x + mul10 x in
  match test (a, b) x, next_value > b with
  | false, true -> res
  | false, false -> test_range (a, b) (x + 1) res
  | true, _ -> test_range (a, b) (x + 1) (next_value :: res)
;;

let start a =
  match intlog10 a % 2 with
  | 0 ->
    let value = Int.pow 10 (intlog10 a + 1) in
    div10 value
  | 1 -> div10 a
  | _ -> raise_s [%message "cannot"]
;;

let part1 (lines : string list) =
  let ranges = List.hd_exn lines |> ranges in
  let ranges = List.map ranges ~f:range in
  List.fold ~init:0 ranges ~f:(fun acc (a, b) ->
    let res = test_range (a, b) (start a) [] in
    print_s [%message ((a, b) : int * int) (res : int list)];
    List.sum (module Int) ~f:(fun x -> x) res + acc)
;;

let part2 (lines : string list) =
  let _ = lines in
  0
;;

let%expect_test _ =
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    (("(a, b)" (11 22)) (res (22 11)))
    (("(a, b)" (95 115)) (res (99)))
    (("(a, b)" (998 1012)) (res (1010)))
    (("(a, b)" (1188511880 1188511890)) (res (1188511885)))
    (("(a, b)" (222220 222224)) (res (222222)))
    (("(a, b)" (1698522 1698528)) (res ()))
    (("(a, b)" (446443 446449)) (res (446446)))
    (("(a, b)" (38593856 38593862)) (res (38593859)))
    (("(a, b)" (565653 565659)) (res ()))
    (("(a, b)" (824824821 824824827)) (res ()))
    (("(a, b)" (2121212118 2121212124)) (res ()))
    ("part1 sample_1" 1227775554)
    ("part2 sample_1" 0)
    |}]
;;
