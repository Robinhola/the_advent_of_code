open! Base
open! Core

let sample_1 =
  "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9" |> String.split_lines
;;

let parse lines =
  List.map lines ~f:(fun line -> String.split line ~on:' ' |> List.map ~f:Int.of_string)
;;

let rec is_all'' ~can_skip ~f = function
  | _ :: [] | [] -> raise_s [%message "Should not happen"]
  | [ a; b ] -> f a b || can_skip
  | a :: b :: c :: rest ->
    (match f a b && f b c with
     | true -> is_all'' ~can_skip ~f (b :: c :: rest)
     | false ->
       can_skip
       && (is_all'' ~can_skip:false ~f (a :: c :: rest)
           || is_all'' ~can_skip:false ~f (b :: c :: rest)
           || is_all'' ~can_skip:false ~f (a :: b :: rest)))
;;

type test =
  | And of test * test
  | Test of (int -> int -> bool)

let rec apply_test test a b =
  match test with
  | And (ta, tb) -> apply_test ta a b && apply_test tb a b
  | Test t -> t a b
;;

let decr = Test (fun a b -> a > b)
let incr = Test (fun a b -> a < b)
let less_than n = Test (fun a b -> abs (a - b) <= n)

let all_follows_one_of' ?(can_skip = true) tests levels =
  let apply t = is_all'' ~can_skip ~f:(apply_test t) levels in
  List.exists tests ~f:apply
;;

let part1 (lines : string list) =
  List.count
    ~f:
      (all_follows_one_of'
         ~can_skip:false
         [ And (decr, less_than 3); And (incr, less_than 3) ])
    (parse lines)
;;

let part2 (lines : string list) =
  List.count
    ~f:(all_follows_one_of' [ And (decr, less_than 3); And (incr, less_than 3) ])
    (parse lines)
;;

let%expect_test _ =
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect {|
    ("part1 sample_1" 2)
    ("part2 sample_1" 4)
    |}]
;;
