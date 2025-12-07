open! Base
open! Core
open! Robin_advent_lib

[@@@warning "-32"]
[@@@warning "-27"]

let should_print_debug = ref false
let debug sexp = if !should_print_debug then print_s sexp
let impossible () = raise_s [%message "impossible"]
let sum' = List.sum (module Int)
let sum = List.sum (module Int) ~f:Fn.id
let mul = List.fold ~init:1 ~f:(fun a b -> a * b)

let sample_1 =
  {|.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............|}
  |> String.split_lines
;;

let rec beam_down t i previous_beams =
  if i + 1 >= (Matrix.dims t).y
  then t
  else (
    let next_beams =
      List.map previous_beams ~f:(fun c ->
        let down = Matrix.next t c Dir.Down |> Option.value_exn in
        match Matrix.get t down with
        | '.' ->
          Matrix.set t down '|';
          [ down ]
        | '|' -> []
        | '^' -> [ Dir.Left; Dir.Right ] |> List.filter_map ~f:(Matrix.next t down)
        | _ -> impossible ())
    in
    beam_down t (i + 1) (List.concat next_beams))
;;

let count_splits t =
  Matrix.all_indices t
  |> List.filter_map ~f:(fun c ->
    match Matrix.get t c with
    | '^' -> Matrix.next t c Dir.Up
    | _ -> None)
  |> List.map ~f:(fun c ->
    match Matrix.get t c with
    | '|' -> 1
    | '.' | 'S' | '^' -> 0
    | _ -> impossible ())
  |> sum
;;

let part1 (lines : string list) =
  let t = Matrix.parse lines in
  let s =
    Matrix.all_indices t
    |> List.find ~f:(fun c ->
      let x = Matrix.get t c in
      Char.equal x 'S')
    |> Option.value_exn
  in
  let t = beam_down t 0 [ s ] in
  if !should_print_debug then Matrix.print t;
  count_splits t
;;

let part2 (lines : string list) = 0

let%expect_test _ =
  should_print_debug := true;
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    .......S.......
    .......|.......
    .......^.......
    ......|.|......
    ......^.^......
    .....|.|.|.....
    .....^.^.^.....
    ....|.|.|.|....
    ....^.^.|.^....
    ...|.|.|||.|...
    ...^.^.||^.^...
    ..|.|.|||.|.|..
    ..^.|.^||.|.^..
    .|.|||.||.||.|.
    .^.^|^.^|^||.^.
    |.|.|.|.|.|||.|
    ("part1 sample_1" 21)
    ("part2 sample_1" 0)
    |}]
;;
