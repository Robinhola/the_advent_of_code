open! Base
open! Core
open! Import

(*
--- Day 4: Secure Container ---

You arrive at the Venus fuel depot only to discover it's protected by a
password. The Elves had written the password on a sticky note, but someone threw
it out.

However, they do remember a few key facts about the password:
  - It is a six-digit number.
  - The value is within the range given in your puzzle input.
  - Two adjacent digits are the same (like 22 in 122345).
  - Going from left to right, the digits never decrease; they only ever increase
    or stay the same (like 111123 or 135679).
  - Other than the range rule, the following are true:

111111 meets these criteria (double 11, never decreases).
223450 does not meet these criteria (decreasing pair of digits 50).
123789 does not meet these criteria (no double).

How many different passwords within the range given in your puzzle input meet
these criteria?
*)

let range_start = 272091
let range_end = 815432

module T : sig
  include Day.T

end = struct
  let name = "--- Day 4: Secure Container ---";;
  let has_six_digits x = String.length (Int.to_string x) = 6

  let%expect_test "six_digs" =
  let t1 = (123456) in
  let t2 = (1234) in
  let t3 = (1234567) in
  printf "%b %b %b" (has_six_digits t1) (has_six_digits t2) (has_six_digits t3);
  [%expect {| true false false |}]

  let two_identical_adjacent x =
    let l = String.to_list (Int.to_string x) in 
    let rec f l = 
      match l with
      | a :: b :: l -> Char.equal a b || f (b :: l)
      | _ -> false
    in f l
  ;;

  let%expect_test "two_identical_adjacent" =
  let t1 = (123456) in
  let t2 = (1224) in
  let t3 = (1234566) in
  printf "%b %b %b" (two_identical_adjacent t1) (two_identical_adjacent t2) (two_identical_adjacent t3);
  [%expect {| false true true |}]
  
  let left_to_right_never_decrease x = 
    let l = List.map (String.to_list (Int.to_string x)) ~f:Char.to_int in
    let rec f l =
      match l with
      | a :: b :: l -> b >= a && f (b :: l)
      | _ -> true
    in 
    f l
  ;;

  let%expect_test "left_to_right_never_decrease" =
  let t1 = (123456) in
  let t2 = (2222) in
  let t3 = (1234560) in
  printf "%b %b %b" (left_to_right_never_decrease t1) (left_to_right_never_decrease t2) (left_to_right_never_decrease t3);
  [%expect {| true true false |}]

  let check_is_valid x = has_six_digits x && two_identical_adjacent x && left_to_right_never_decrease x;;

    let rec how_many_valid x count =
      match x > range_end with 
      | true -> count
      | false -> let count = count + (
          match check_is_valid x with
          | true -> 1
          | false -> 0)
        in how_many_valid (x + 1) count
      ;;

  let part1 = how_many_valid range_start 0

  (*
  --- Part Two ---
  An Elf just remembered one more important detail: the two adjacent matching
  digits are not part of a larger group of matching digits.
  
  Given this additional criterion, but still ignoring the range rule, the
  following are now true:
    - 112233 meets these criteria because the digits never decrease and all
      repeated digits are exactly two digits long.
    - 123444 no longer meets the criteria (the repeated 44 is part of a larger
      group of 444).
    - 111122 meets the criteria (even though 1 is repeated more than twice, it
      still contains a double 22).

  How many different passwords within the range given in your puzzle input meet
  all of the criteria?
  *)

  let two_identical_adjacent' x =
    let l = String.to_list (Int.to_string x) in 
    let rec f l last = 
      match l with 
      | a :: b :: l -> (
        let t = List.hd l |> Option.value ~default:'a' in
        let is_valid = Char.equal a b && not (Char.equal a last) && not (Char.equal b t) in 
        let last = a in 
        is_valid || f (b :: l) last
      )
      | _ -> false
    in
    f l 'a'
  ;;

  let%expect_test "Numbers repeating don't count" =
  let t1 = (1234999) in
  let t2 = (1222366) in
  let t3 = (1222666) in
  let t4 = (1111111) in
  printf "%b %b %b %b" (two_identical_adjacent' t1) (two_identical_adjacent' t2) (two_identical_adjacent' t3) (two_identical_adjacent' t4);
  [%expect {| false true false false |}]

  let check_is_valid x = check_is_valid x && two_identical_adjacent' x;;
  let rec how_many_valid x count =
    match x > range_end with 
    | true -> count
    | false -> let count = count + (
        match check_is_valid x with
        | true -> 1
        | false -> 0)
      in how_many_valid (x + 1) count
    ;;

  let part2 = how_many_valid range_start 0

end
