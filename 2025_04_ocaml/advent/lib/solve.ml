open! Base
open! Core
open! Robin_advent_lib

[@@@warning "-32"]
[@@@warning "-27"]
[@@@warning "-26"]

let should_print_debug = ref false
let debug sexp = if !should_print_debug then print_s sexp
let sum = List.sum (module Int)

let sample_1 =
  {|..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.|}
  |> String.split_lines
;;

let parse = Robin_advent_lib.Matrix.parse
let all_around t c = Dir.for_all ~f:(Matrix.next t c) |> List.filter_opt

let next_round t =
  Matrix.all_indices t
  |> List.iter ~f:(fun c ->
    if Char.(Matrix.get t c = '.')
    then ()
    else (
      let number_of_neighbours =
        all_around t c
        |> sum ~f:(fun c ->
          match Matrix.get t c with
          | 'x' | '@' -> 1
          | '.' -> 0
          | _ -> raise_s [%message "impossible"])
      in
      if number_of_neighbours < 4 then Matrix.set t c 'x'));
  t
;;

let remove_and_count t =
  Matrix.all_indices t
  |> sum ~f:(fun c ->
    match Matrix.get t c with
    | 'x' ->
      Matrix.set t c '.';
      1
    | _ -> 0)
;;

let part1 (lines : string list) =
  let t = parse lines in
  let t = next_round t in
  remove_and_count t
;;

let rec remove_all t total =
  let t = next_round t in
  match remove_and_count t with
  | 0 -> total
  | x -> remove_all t (total + x)
;;

let part2 (lines : string list) =
  let t = parse lines in
  remove_all t 0
;;

let%expect_test _ =
  should_print_debug := true;
  let t = parse sample_1 in
  Matrix.print t;
  print_s [%message (Matrix.all_within t (Coord.of_tuple (0, 0)) 1 : Coord.t list)];
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    ..@@.@@@@.
    @@@.@.@.@@
    @@@@@.@.@@
    @.@@@@..@.
    @@.@@@@.@@
    .@@@@@@@.@
    .@.@.@.@@@
    @.@@@.@@@@
    .@@@@@@@@.
    @.@.@@@.@.
    ("Matrix.all_within t (Coord.of_tuple (0, 0)) 1"
     (((x 0) (y 0)) ((x 0) (y 1)) ((x 1) (y 0))))
    ("part1 sample_1" 13)
    ("part2 sample_1" 43)
    |}]
;;
