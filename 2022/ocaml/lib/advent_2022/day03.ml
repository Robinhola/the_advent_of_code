open! Base
open! Core
open! Import

(* https://adventofcode.com/2022/day/3 *)

let lines =
  Advent.read_lines "day03" ~for_tests:
  [ "vJrwpWtwJgWrhcsFMMfFFhFp"
  ; "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
  ; "PmmdzqPrVvPwwTWBwg"
  ; "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
  ; "ttgJtRGJQctTZtZT"
  ; "CrZsJsPPZsGzwwsLwLmpwMDw"
  ] ()
  |> List.map ~f:String.to_list
;;

let rec divide_in_halves current = function
  | rest when phys_equal (List.length rest) (List.length current) -> current, rest
  | a :: rest -> rest |> divide_in_halves (a :: current)
  | rest when List.length rest < List.length current -> failwith "not divisible by 2"
  | [] -> failwith "wrong input"
;;

let characters_in_common (left, right) =
  let left = left |> Char.Set.of_list in
  let right = right |> Char.Set.of_list in
  let intersection = Set.inter left right |> Set.to_list in
  intersection
;;

let which_badge (a, b, c) =
  let d = characters_in_common (a, b) in
  characters_in_common (c, d)
;;

let upper_a_score = Char.to_int 'A'

let lower_a_score = Char.to_int 'a'

let compute reference offset letter =
  (letter |> Char.to_int) - reference + offset
;;

let char_to_score = function
  | letter when letter |> Char.is_uppercase -> letter |> compute upper_a_score 27
  | letter when letter |> Char.is_lowercase -> letter |> compute lower_a_score 1
  | letter -> failwith ("wrong letter: " ^ (Char.to_string letter))
;;

let rec group_by_3 groups = function
  | [] -> groups
  | a :: b :: c :: rest -> group_by_3 ((a, b, c) :: groups) rest
  | _ -> failwith "not divisible by 3"
;;

let to_score l = l
  |> List.map ~f:char_to_score
  |> List.fold ~init:0 ~f:Int.(+)
;;

let extract_single_character = function
  | badge :: [] -> badge
  | _ -> failwith "Could not find unique match"
;;

module T : sig
  include Day.T
end = struct
  let name = "--- Day 3: Rucksack Reorganization ---"
  let part1 = lines
    |> List.map ~f:(divide_in_halves [])
    |> List.map ~f:characters_in_common
    |> List.map ~f:extract_single_character
    |> to_score
  ;;
  let part2 = lines
    |> group_by_3 []
    |> List.map ~f:which_badge
    |> List.map ~f:extract_single_character
    |> to_score
  ;;

  let%expect_test "" = 
    print_s [%message (part1: int) (part2: int)];
    [%expect {| ((part1 157) (part2 70)) |}]
  ;;

end