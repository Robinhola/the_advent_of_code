open! Base
open! Core
open! Import
open Advent

let lines = Advent.read_lines "day08" ~for_tests:
  [ "30373"
  ; "25512"
  ; "65332"
  ; "33549"
  ; "35390"
  ] ()
;;

module IntPairs = struct
  type t = int * int [@@deriving sexp, compare]
end

module PairsSet = Set.Make(IntPairs)

module Grid = struct
  type t = int Array.t Array.t
  [@@deriving sexp]

  let empty_size_of lines =
    let x = List.hd_exn lines |> String.length in
    let y = List.length lines in
    Array.make_matrix ~dimx:x ~dimy:y 0
  ;;

  let make lines =
    let m = empty_size_of lines in
    lines
    |> List.iteri ~f:(fun y line ->
      line
      |> String.to_list
      |> List.iteri ~f:(
        fun x c ->
          m.(y).(x) <- c |> String.of_char |> Int.of_string
      )
    );
    m
  ;;

  let find_bounds grid =
    let max_x = Array.length grid in
    let max_y = Array.length grid.(0) in
    max_x, max_y
  ;;

end

let%expect_test "" =
  let grid = Grid.make lines in
  print_s [%message (grid : int array array)];
  [%expect {|
    (grid ((3 0 3 7 3) (2 5 5 1 2) (6 5 3 3 2) (3 3 5 4 9) (3 5 3 9 0))) |}]
;;

let rec can_you_see_me ~grid ~x ~y ~dir ?(current_maximum = -1) is_seen =
  let max_x, max_y = Grid.find_bounds grid in
  match 0 <= x && x < max_x && 0 <= y && y < max_y with
    | false -> is_seen
    | true ->
      let value = grid.(y).(x) in
      let is_seen =
        if value > current_maximum then Set.add is_seen (x, y)
        else is_seen
      in
      let x, y = Direction.inc ~x ~y dir in
      let current_maximum = max value current_maximum in
      can_you_see_me ~grid ~x ~y ~dir ~current_maximum is_seen
;;

  let rec from_y ~grid ~x is_seen  =
    let max_x, max_y = Grid.find_bounds grid in
    if x < max_x then
      is_seen
      |> can_you_see_me ~grid ~x ~y:0 ~dir:Direction.Down
      |> can_you_see_me ~grid ~x ~y:(max_y - 1) ~dir:Direction.Up
      |> from_y ~grid ~x:Direction.(inc1 x Right)
    else is_seen
  ;;

  let rec from_x ~grid ~y is_seen =
    let max_x, max_y = Grid.find_bounds grid in
    if y < max_y then
      is_seen
      |> can_you_see_me ~grid ~x:0 ~y ~dir:Direction.Right
      |> can_you_see_me ~grid ~x:(max_x - 1) ~y ~dir:Direction.Left
      |> from_x ~grid ~y:Direction.(inc1 y Down)
    else is_seen
  ;;

let from_all_direction grid =
  PairsSet.empty
  |> from_x ~grid ~y:0
  |> from_y ~grid ~x:0
;;

let%expect_test "" =
  let grid = Grid.make lines in
  let is_seen = from_all_direction grid in
  print_s [%message (is_seen: PairsSet.t)];
  [%expect {|
    (is_seen
     ((0 0) (0 1) (0 2) (0 3) (0 4) (1 0) (1 1) (1 2) (1 4) (2 0) (2 1) (2 3)
      (2 4) (3 0) (3 2) (3 4) (4 0) (4 1) (4 2) (4 3) (4 4))) |}]
;;

let assign_score grid ~x ~y =
  let max_x, max_y = Grid.find_bounds grid in
  let current_max = grid.(y).(x) in
  let rec look ~x ~y dir total =
    match 0 <= x && x < max_x && 0 <= y && y < max_y with
    | false -> total
    | true ->
      let total = total + 1 in
      let value = grid.(y).(x) in
      if value >= current_max then total
      else
        let x, y = Direction.inc ~x ~y dir in
        look ~x ~y dir total
  in
  let scores =
    [ look ~x         ~y:(y - 1) Direction.Up 0
    ; look ~x:(x - 1) ~y         Direction.Left 0
    ; look ~x:(x + 1) ~y         Direction.Right 0
    ; look ~x         ~y:(y + 1) Direction.Down 0
    ]
  in
  scores |> List.fold ~init:1 ~f:(Int.( * ))
;;

let%expect_test "" =
  let grid = Grid.make lines in
  let score1 = assign_score grid ~x:2 ~y:1 in
  let score2 = assign_score grid ~x:2 ~y:3 in
  print_s [%message (score1: int) (score2: int)];
  [%expect {| ((score1 4) (score2 8)) |}]
;;

let rec find_best_scenic_score grid ~x ~y ~best =
  let max_x, max_y = Grid.find_bounds grid in
  match x >= max_x, y >= max_y with
  | false, true -> best
  | true, false -> find_best_scenic_score grid ~x:0 ~y:(y + 1) ~best
  | false, false ->
    let score = assign_score grid ~x ~y in
    let best = score |> max best in
    find_best_scenic_score grid ~x:(x + 1) ~y ~best
  | _ -> failwith "impossible"
;;

module T : sig
  include Day.T
end = struct
  let name = "--- Day 8: Treetop Tree House ---"
  let grid = Grid.make lines
  let part1 =
    from_all_direction grid
    |> Set.length
  ;;
  let part2 = find_best_scenic_score grid ~x:0 ~y:0 ~best:0

  let%expect_test "" =
    print_s [%message (part1: int) (part2: int)];
    [%expect {| ((part1 21) (part2 8)) |}]
  ;;
end