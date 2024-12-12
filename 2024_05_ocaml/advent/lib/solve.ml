open! Base
open! Core

let sample_1 =
  {|47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47|}
  |> String.split_lines
;;

module Pair = struct
  module T = struct
    type t =
      { left : int
      ; right : int
      }
    [@@deriving compare, sexp, hash, equal]
  end

  include T
  module Set = Set.Make (T)
  module Map = Map.Make (T)
  module Hash_set = Hash_set.Make (T)

  let parse s =
    match String.split ~on:'|' s with
    | [ left; right ] ->
      let left = Int.of_string left in
      let right = Int.of_string right in
      { left; right }
    | _ -> raise_s [%message "wrong string" (s : string)]
  ;;

  let reverse t = { left = t.right; right = t.left }
end

let parse_rule s = String.split ~on:',' s |> List.map ~f:Int.of_string

let rec parse pairs rules state lines =
  match state, lines with
  | `Pairs, "" :: rest -> parse pairs rules `Rules rest
  | `Pairs, line :: rest -> parse (Pair.parse line :: pairs) rules `Pairs rest
  | `Rules, line :: rest -> parse pairs (parse_rule line :: rules) `Rules rest
  | `Rules, [] -> Pair.Set.of_list pairs, List.rev rules
  | state, rest ->
    raise_s [%message "Wrong state" (state : [ `Pairs | `Rules ]) (rest : string list)]
;;

let compare pairs left right =
  let key = Pair.{ left; right } in
  let rkey = Pair.reverse key in
  let is_in k = Set.mem pairs k in
  match is_in key, is_in rkey with
  | true, false -> -1
  | false, true -> 1
  | true, true -> raise_s [%message "found twice?" (key : Pair.t) (rkey : Pair.t)]
  | false, false -> raise_s [%message "Did not find" (key : Pair.t) (rkey : Pair.t)]
;;

let is_valid pairs = List.is_sorted ~compare:(compare pairs)

let get_middle l =
  let mid = List.length l / 2 in
  List.nth_exn l mid
;;

let part1 (lines : string list) =
  let pairs, rules = parse [] [] `Pairs lines in
  List.filter rules ~f:(List.is_sorted ~compare:(compare pairs))
  |> List.sum (module Int) ~f:get_middle
;;

let part2 (lines : string list) =
  let pairs, rules = parse [] [] `Pairs lines in
  List.filter rules ~f:(Fn.compose not (List.is_sorted ~compare:(compare pairs)))
  |> List.map ~f:(List.sort ~compare:(compare pairs))
  |> List.sum (module Int) ~f:get_middle
;;

let%expect_test _ =
  let pairs, rules = parse [] [] `Pairs sample_1 in
  let () =
    List.iter rules ~f:(fun rule ->
      let is_valid = is_valid pairs rule in
      print_s [%message (rule : int list) (is_valid : bool) (get_middle rule : int)])
  in
  print_s [%message (pairs : Pair.Set.t) (rules : int list list)];
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    ((rule (75 47 61 53 29)) (is_valid true) ("get_middle rule" 61))
    ((rule (97 61 53 29 13)) (is_valid true) ("get_middle rule" 53))
    ((rule (75 29 13)) (is_valid true) ("get_middle rule" 29))
    ((rule (75 97 47 61 53)) (is_valid false) ("get_middle rule" 47))
    ((rule (61 13 29)) (is_valid false) ("get_middle rule" 13))
    ((rule (97 13 75 29 47)) (is_valid false) ("get_middle rule" 75))
    ((pairs
      (((left 29) (right 13)) ((left 47) (right 13)) ((left 47) (right 29))
       ((left 47) (right 53)) ((left 47) (right 61)) ((left 53) (right 13))
       ((left 53) (right 29)) ((left 61) (right 13)) ((left 61) (right 29))
       ((left 61) (right 53)) ((left 75) (right 13)) ((left 75) (right 29))
       ((left 75) (right 47)) ((left 75) (right 53)) ((left 75) (right 61))
       ((left 97) (right 13)) ((left 97) (right 29)) ((left 97) (right 47))
       ((left 97) (right 53)) ((left 97) (right 61)) ((left 97) (right 75))))
     (rules
      ((75 47 61 53 29) (97 61 53 29 13) (75 29 13) (75 97 47 61 53) (61 13 29)
       (97 13 75 29 47))))
    ("part1 sample_1" 143)
    ("part2 sample_1" 123)
    |}]
;;
