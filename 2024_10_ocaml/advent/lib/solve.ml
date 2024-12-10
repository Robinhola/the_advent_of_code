open! Base
open! Core
open! Robin_advent_lib

let sample_1 =
  {|89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732|}
  |> String.split_lines
;;

let parse l = Matrix.parse l

let next = function
  | '0' -> `Continue '1'
  | '1' -> `Continue '2'
  | '2' -> `Continue '3'
  | '3' -> `Continue '4'
  | '4' -> `Continue '5'
  | '5' -> `Continue '6'
  | '6' -> `Continue '7'
  | '7' -> `Continue '8'
  | '8' -> `Continue '9'
  | '9' -> `Done
  | c -> raise_s [%message (c : char)]
;;

let rec find m target pos =
  let c = Matrix.get m pos in
  match Char.equal target c, next c with
  | true, `Continue n ->
    [ Dir.Up; Dir.Right; Dir.Down; Dir.Left ]
    |> List.filter_map ~f:(Matrix.next m pos)
    |> List.concat_map ~f:(find m n)
  | true, `Done -> [ Some pos ]
  | false, _ -> [ None ]
;;

let part1 (lines : string list) =
  let matrix = parse lines in
  Matrix.all_indices matrix
  |> List.map ~f:(fun c -> find matrix '0' c |> List.filter_opt |> Coord.Set.of_list)
  |> List.sum (module Int) ~f:Set.length
;;

let part2 (lines : string list) =
  let _ = lines in
  let matrix = parse lines in
  Matrix.all_indices matrix
  |> List.map ~f:(fun c -> find matrix '0' c |> List.filter_opt)
  |> List.sum (module Int) ~f:List.length
;;

let%expect_test _ =
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    ("part1 sample_1" 36)
    ("part2 sample_1" 81)
    |}]
;;
