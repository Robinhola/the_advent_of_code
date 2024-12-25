open! Base
open! Core
open! Robin_advent_lib

let sample_1 =
  {|#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####|}
  |> String.split_lines
;;

(* I invert everything because I think it will work *)
let parse lines =
  let ms, current =
    List.fold lines ~init:([], []) ~f:(fun (ms, current) line ->
      match line with
      | "" ->
        let ms = Matrix.parse current :: ms in
        let current = [] in
        ms, current
      | line ->
        let current = line :: current in
        ms, current)
  in
  Matrix.parse current :: ms
;;

let do_fit a b =
  Matrix.all_indices a
  |> List.fold_until
       ~init:true
       ~f:(fun _ c ->
         let a = Matrix.get a c in
         let b = Matrix.get b c in
         match a, b with
         | '#', '#' ->
           (*print_s [%message (a : char) (b : char) (c : Coord.t)];*)
           Stop false
         | _ -> Continue true)
       ~finish:Fn.id
;;

let%expect_test _ =
  let a =
    {|.....
.....
#.#..
###..
###.#
###.#
#####|}
    |> String.split_lines
    |> Matrix.parse
  in
  let b =
    {|#####
##.##
.#.##
...##
...#.
...#.
.....|}
    |> String.split_lines
    |> Matrix.parse
  in
  print_s [%message (do_fit a b : bool)];
  print_s [%message (do_fit a a : bool)];
  print_s [%message (do_fit b b : bool)];
  [%expect
    {|
    ("do_fit a b" true)
    ("do_fit a a" false)
    ("do_fit b b" false)
    |}]
;;

let all_combinations l =
  let rec all_combinations total = function
    | _ :: [] | [] -> total
    | a :: rest ->
      let for_a = List.map rest ~f:(fun b -> a, b) in
      all_combinations (List.concat [ for_a; total ]) rest
  in
  all_combinations [] l
;;

let part1 (lines : string list) =
  let matrices = parse lines in
  all_combinations matrices
  |> List.sum (module Int) ~f:(fun (a, b) -> if do_fit a b then 1 else 0)
;;

let part2 (lines : string list) =
  let _ = lines in
  0
;;

let%expect_test _ =
  print_s [%message (parse sample_1 : Matrix.t list)];
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    ("parse sample_1"
     (((words
        ((# # # # #) (# . # . #) (# . # . .) (# . . . .) (. . . . .) (. . . . .)
         (. . . . .)))
       (dims ((x 5) (y 7))))
      ((words
        ((# # # # #) (# # # . #) (# # # . #) (# # # . .) (# . # . .) (. . . . .)
         (. . . . .)))
       (dims ((x 5) (y 7))))
      ((words
        ((# # # # #) (# . # # #) (# . # . #) (# . . . #) (# . . . .) (# . . . .)
         (. . . . .)))
       (dims ((x 5) (y 7))))
      ((words
        ((. . . . .) (. . . # .) (. . . # .) (. . . # #) (. # . # #) (# # . # #)
         (# # # # #)))
       (dims ((x 5) (y 7))))
      ((words
        ((. . . . .) (. # . . .) (. # . # .) (. # # # #) (. # # # #) (. # # # #)
         (# # # # #)))
       (dims ((x 5) (y 7))))))
    ("part1 sample_1" 3)
    ("part2 sample_1" 0)
    |}]
;;
