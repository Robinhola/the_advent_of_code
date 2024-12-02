open! Base
open! Core

let sample_1 =
  "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9" |> String.split_lines
;;

let parse lines =
  List.map lines ~f:(fun line -> String.split line ~on:' ' |> List.map ~f:Int.of_string)
;;

let rec is_all direction l =
  match direction, l with
  | `Decreasing, a :: b :: rest -> a < b && is_all direction (b :: rest)
  | `Increasing, a :: b :: rest -> a > b && is_all direction (b :: rest)
  | _ -> true
;;

let rec is_within ~n = function
  | a :: b :: rest -> abs (a - b) <= n && is_within ~n (b :: rest)
  | _ -> true
;;

let part1 (lines : string list) =
  let l = parse lines in
  List.count l ~f:(fun line ->
    (is_all `Decreasing line || is_all `Increasing line) && is_within ~n:3 line)
;;

let rec is_all' ?(prev = None) ?(can_skip = true) ~f l =
  match l with
  | [] -> raise_s [%message "Should not be empty"]
  | _ :: [] -> true
  | a :: b :: rest ->
    let is_level_valid =
      match prev with
      | None -> f a b
      | Some prev -> f prev a && f a b
    in
    (match is_level_valid, can_skip with
     | false, false -> false
     | true, _ -> is_all' ~prev:(Some a) ~f ~can_skip (b :: rest)
     | false, true ->
       is_all' ~prev ~f ~can_skip:false (a :: rest)
       || is_all' ~prev ~f ~can_skip:false (b :: rest))
;;

let decrease a b = a < b && abs (b - a) <= 3
let increase a b = a > b && abs (b - a) <= 3

let all_follows_one_of tests levels =
  let apply f = is_all' ~f levels in
  List.exists tests ~f:apply
;;

let part2 (lines : string list) =
  List.count ~f:(all_follows_one_of [ decrease; increase ]) (parse lines)
;;

let%expect_test _ =
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect {|
    ("part1 sample_1" 2)
    ("part2 sample_1" 4)
    |}]
;;
