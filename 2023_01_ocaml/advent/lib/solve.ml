open! Base
open! Core

let sample_1 = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
let digits = [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ] |> Char.Set.of_list
let is_digit = Set.mem digits

let rec first_digit (line : char list) =
  match line with
  | [] -> raise_s [%message "No digit found"]
  | a :: _ when is_digit a -> a
  | _ :: rest -> first_digit rest
;;

let part1 (lines : string list) =
  let line_value line =
    let line = String.to_list line in
    let a = first_digit line in
    let b = List.rev line |> first_digit in
    [ a; b ] |> String.of_list |> Int.of_string
  in
  List.sum (module Int) ~f:line_value lines
;;

let%expect_test _ =
  let result = String.split_lines sample_1 |> part1 in
  print_s [%message (result : int)];
  [%expect {| (result 142) |}]
;;

let sample_2 =
  "two1nine\n\
   eightwothree\n\
   abcone2threexyz\n\
   xtwone3four\n\
   4nineeightseven2\n\
   zoneight234\n\
   7pqrstsixteen"
;;

let nums_to_val =
  [ "zero", '0'
  ; "one", '1'
  ; "two", '2'
  ; "three", '3'
  ; "four", '4'
  ; "five", '5'
  ; "six", '6'
  ; "seven", '7'
  ; "eight", '8'
  ; "nine", '9'
  ]
  |> String.Map.of_alist_exn
;;

let nums = Map.key_set nums_to_val
let inverted_nums = String.Set.map ~f:String.rev nums
let starts_with ~s start = String.prefix s (String.length start) |> String.equal start

let rec first_digit_or_num line was_reversed =
  match String.to_list line with
  | [] -> raise_s [%message "No digit found"]
  | a :: _ when is_digit a -> a
  | _ :: rest ->
    let keys =
      match was_reversed with
      | true -> inverted_nums
      | false -> nums
    in
    (match Set.find keys ~f:(starts_with ~s:line) with
     | None -> first_digit_or_num (String.of_list rest) was_reversed
     | Some key ->
       let real_key =
         match was_reversed with
         | true -> String.rev key
         | false -> key
       in
       Map.find_exn nums_to_val real_key)
;;

let part2 (lines : string list) =
  let line_value line =
    let a = first_digit_or_num line false in
    let b = first_digit_or_num (String.rev line) true in
    [ a; b ] |> String.of_list |> Int.of_string
  in
  List.sum (module Int) ~f:line_value lines
;;

let%expect_test _ =
  let s1 = String.split_lines sample_1 |> part2 in
  let s2 = String.split_lines sample_2 |> part2 in
  print_s [%message (s1 : int) (s2 : int)];
  [%expect {| (result 142) |}];
  [%expect {| (result 281) |}]
;;
