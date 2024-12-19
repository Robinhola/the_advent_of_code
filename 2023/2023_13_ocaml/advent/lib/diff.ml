open! Base
open! Core

let rec has_only_one_difference' a b current =
  match a, b, current with
  | _, _, f when f > 1 -> false
  | [], [], 0 -> false
  | [], [], 1 -> true
  | [], [], _ -> false
  | [], _, _ -> false
  | _, [], _ -> false
  | ca :: resta, cb :: restb, current when Char.equal ca cb ->
    has_only_one_difference' resta restb current
  | _ :: resta, _ :: restb, current -> has_only_one_difference' resta restb (current + 1)
;;

let has_only_one_difference a b =
  has_only_one_difference' (String.to_list a) (String.to_list b) 0
;;

let%expect_test _ =
  print_s [%message (has_only_one_difference "abc" "abd" : bool)];
  print_s [%message (has_only_one_difference "abc" "abc" : bool)];
  print_s [%message (has_only_one_difference "abc" "cbd" : bool)];
  [%expect
    {|
    ("has_only_one_difference \"abc\" \"abd\"" true)
    ("has_only_one_difference \"abc\" \"abc\"" false)
    ("has_only_one_difference \"abc\" \"cbd\"" false)
    |}]
;;
