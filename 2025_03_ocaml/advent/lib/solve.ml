open! Base
open! Core

[@@@warning "-32"]

let should_print_debug = ref false
let debug sexp = if !should_print_debug then print_s sexp

let sample_1 =
  {|987654321111111
811111111111119
234234234234278
818181911112111|}
  |> String.split_lines
;;

let sublist l first_i first_i_excluded =
  (*let n = List.length l in*)
  (*let l = if first_i = 0 then l else List.drop l first_i in*)
  (*let l =*)
  (*  let to_drop = n - first_i_excluded in*)
  (*  List.take l (List.length l - to_drop)*)
  (*in*)
  (*l*)
  List.slice l first_i first_i_excluded
;;

let rec best_of l i n number =
  (*print_s [%message (i : int) (n : int) (number : string)];*)
  if n = 0
  then Int.of_string number
  else (
    let n = n - 1 in
    let sub = List.slice l i (List.length l - n) in
    let m, index =
      List.max_elt sub ~compare:(fun (a, _) (b, _) -> Int.compare a b) |> Option.value_exn
    in
    best_of l (index + 1) n (number ^ Int.to_string m))
;;

let part1 (lines : string list) =
  let lines =
    List.map lines ~f:(fun s ->
      s |> String.to_list |> List.mapi ~f:(fun i x -> Int.of_string (Char.to_string x), i))
  in
  List.sum (module Int) lines ~f:(fun l -> best_of l 0 2 "")
;;

let part2 (lines : string list) =
  let lines =
    List.map lines ~f:(fun s ->
      s |> String.to_list |> List.mapi ~f:(fun i x -> Int.of_string (Char.to_string x), i))
  in
  List.sum (module Int) lines ~f:(fun l -> best_of l 0 12 "")
;;

let%expect_test _ =
  should_print_debug := true;
  print_s [%message (sublist (String.to_list "12345") 1 3 : char list)];
  print_s [%message (sublist (String.to_list "12345") 0 3 : char list)];
  print_s [%message (sublist (String.to_list "12345") 0 5 : char list)];
  print_s [%message (sublist (String.to_list "12345") 2 4 : char list)];
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    ("sublist (String.to_list \"12345\") 1 3" (2 3))
    ("sublist (String.to_list \"12345\") 0 3" (1 2 3))
    ("sublist (String.to_list \"12345\") 0 5" (1 2 3 4 5))
    ("sublist (String.to_list \"12345\") 2 4" (3 4))
    ((i 0) (n 2) (number ""))
    ((i 1) (n 1) (number 9))
    ((i 2) (n 0) (number 98))
    ((i 0) (n 2) (number ""))
    ((i 1) (n 1) (number 8))
    ((i 15) (n 0) (number 89))
    ((i 0) (n 2) (number ""))
    ((i 14) (n 1) (number 7))
    ((i 15) (n 0) (number 78))
    ((i 0) (n 2) (number ""))
    ((i 7) (n 1) (number 9))
    ((i 12) (n 0) (number 92))
    ("part1 sample_1" 357)
    ((i 0) (n 12) (number ""))
    ((i 1) (n 11) (number 9))
    ((i 2) (n 10) (number 98))
    ((i 3) (n 9) (number 987))
    ((i 4) (n 8) (number 9876))
    ((i 5) (n 7) (number 98765))
    ((i 6) (n 6) (number 987654))
    ((i 7) (n 5) (number 9876543))
    ((i 8) (n 4) (number 98765432))
    ((i 9) (n 3) (number 987654321))
    ((i 10) (n 2) (number 9876543211))
    ((i 11) (n 1) (number 98765432111))
    ((i 12) (n 0) (number 987654321111))
    ((i 0) (n 12) (number ""))
    ((i 1) (n 11) (number 8))
    ((i 2) (n 10) (number 81))
    ((i 3) (n 9) (number 811))
    ((i 4) (n 8) (number 8111))
    ((i 5) (n 7) (number 81111))
    ((i 6) (n 6) (number 811111))
    ((i 7) (n 5) (number 8111111))
    ((i 8) (n 4) (number 81111111))
    ((i 9) (n 3) (number 811111111))
    ((i 10) (n 2) (number 8111111111))
    ((i 11) (n 1) (number 81111111111))
    ((i 15) (n 0) (number 811111111119))
    ((i 0) (n 12) (number ""))
    ((i 3) (n 11) (number 4))
    ((i 5) (n 10) (number 43))
    ((i 6) (n 9) (number 434))
    ((i 7) (n 8) (number 4342))
    ((i 8) (n 7) (number 43423))
    ((i 9) (n 6) (number 434234))
    ((i 10) (n 5) (number 4342342))
    ((i 11) (n 4) (number 43423423))
    ((i 12) (n 3) (number 434234234))
    ((i 13) (n 2) (number 4342342342))
    ((i 14) (n 1) (number 43423423427))
    ((i 15) (n 0) (number 434234234278))
    ((i 0) (n 12) (number ""))
    ((i 1) (n 11) (number 8))
    ((i 3) (n 10) (number 88))
    ((i 5) (n 9) (number 888))
    ((i 7) (n 8) (number 8889))
    ((i 8) (n 7) (number 88891))
    ((i 9) (n 6) (number 888911))
    ((i 10) (n 5) (number 8889111))
    ((i 11) (n 4) (number 88891111))
    ((i 12) (n 3) (number 888911112))
    ((i 13) (n 2) (number 8889111121))
    ((i 14) (n 1) (number 88891111211))
    ((i 15) (n 0) (number 888911112111))
    ("part2 sample_1" 3121910778619)
    |}]
;;
