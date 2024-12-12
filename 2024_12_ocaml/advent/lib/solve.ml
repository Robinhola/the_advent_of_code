open! Base
open! Core
open! Robin_advent_lib

let sample_1 =
  {|RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE|}
  |> String.split_lines
;;

let parse lines = Matrix.parse lines

let regions m =
  let seen = Coord.Hashtbl.create () in
  let already_seen c = Hashtbl.find seen c |> Option.is_some in
  let explore goal c =
    let rec explore' current = function
      | [] -> current
      | n :: rest ->
        let next_candidates =
          [ Dir.Up; Dir.Right; Dir.Down; Dir.Left ]
          |> List.filter_map ~f:(fun dir ->
            match Matrix.next m n dir with
            | Some c when Char.equal goal (Matrix.get m c) && not (already_seen c) ->
              Hashtbl.add_exn seen ~key:c ~data:();
              Some c
            | _ -> None)
        in
        let rest = List.append rest next_candidates in
        explore' (n :: current) rest
    in
    if already_seen c
    then []
    else (
      Hashtbl.add_exn seen ~key:c ~data:();
      explore' [] [ c ])
  in
  Matrix.all_indices m
  |> List.map ~f:(fun c -> explore (Matrix.get m c) c)
  |> List.filter ~f:(Fn.compose not List.is_empty)
;;

let sum = List.sum (module Int)

let perimiter m =
  sum ~f:(fun c ->
    let value = Matrix.get m c in
    [ Dir.Up; Dir.Right; Dir.Down; Dir.Left ]
    |> List.map ~f:(Matrix.next m c)
    |> List.filter ~f:(function
      | Some n ->
        let next_value = Matrix.get m n in
        Char.equal value next_value |> not
      | None -> true)
    |> List.length)
;;

let is_side m coord dir =
  let value = Matrix.get m coord in
  match Matrix.next m coord dir with
  | None -> true
  | Some c ->
    let neighbour = Matrix.get m c in
    not (Char.equal neighbour value)
;;

let next_side m current dir =
  [ Some (current, Dir.rotate dir)
  ; Option.map (Matrix.next m current (Dir.rotate dir)) ~f:(fun c -> c, dir)
  ; Matrix.next m current (Dir.rotate dir)
    |> Option.bind ~f:(fun c ->
      let dir = dir in
      Matrix.next m c dir)
    |> Option.map ~f:(fun c -> c, Dir.rotate_left dir)
  ]
  |> List.filter_opt
;;

let%expect_test _ =
  let open Coord in
  let m = parse sample_1 in
  print_s [%message (next_side m { x = 0; y = 0 } Dir.Up : (Coord.t * Dir.t) list)];
  print_s [%message (next_side m { x = 0; y = 1 } Dir.Up : (Coord.t * Dir.t) list)];
  print_s [%message (next_side m { x = 5; y = 5 } Dir.Left : (Coord.t * Dir.t) list)];
  [%expect
    {|
    ("next_side m { x = 0; y = 0 } Dir.Up"
     ((((x 0) (y 0)) Right) (((x 1) (y 0)) Up)))
    ("next_side m { x = 0; y = 1 } Dir.Up"
     ((((x 0) (y 1)) Right) (((x 1) (y 1)) Up) (((x 1) (y 0)) Left)))
    ("next_side m { x = 5; y = 5 } Dir.Left"
     ((((x 5) (y 5)) Up) (((x 5) (y 4)) Left) (((x 4) (y 4)) Down)))
    |}]
;;

let count_sides m coord =
  let dir =
    List.find_exn [ Dir.Up; Dir.Right; Dir.Down; Dir.Left ] ~f:(is_side m coord)
  in
  let start_dir = dir in
  let start_coord = coord in
  let rec count_sides count current dir =
    let next_c, next_d =
      List.find_exn (next_side m current dir) ~f:(fun (c, d) -> is_side m c d)
    in
    let count = if Dir.equal dir next_d then count else count + 1 in
    if Dir.equal start_dir next_d && Coord.equal start_coord next_c
    then count
    else count_sides count next_c next_d
  in
  count_sides 0 coord dir
;;

let part1 (lines : string list) =
  let m = parse lines in
  let regions = regions m in
  List.sum
    (module Int)
    regions
    ~f:(fun r ->
      let area = List.length r in
      let perimiter = perimiter m r in
      area * perimiter)
;;

let part2 (lines : string list) =
  let m = parse lines in
  let regions = regions m in
  List.sum
    (module Int)
    regions
    ~f:(fun r ->
      let area = List.length r in
      let size = count_sides m (List.hd_exn r) in
      area * size)
;;

let%expect_test _ =
  let m = parse sample_1 in
  let regions = regions m in
  print_s [%message (List.map regions ~f:List.length : int list)];
  print_s [%message (List.map regions ~f:(perimiter m) : int list)];
  print_s
    [%message (List.map regions ~f:(Fn.compose (count_sides m) List.hd_exn) : int list)];
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    ("List.map regions ~f:List.length" (12 4 14 10 13 11 1 13 14 5 3))
    ("List.map regions ~f:(perimiter m)" (18 8 28 18 20 20 4 18 22 12 8))
    ("List.map regions ~f:(Fn.compose (count_sides m) List.hd_exn)"
     (10 4 22 12 10 12 4 8 16 6 6))
    ("part1 sample_1" 1930)
    ("part2 sample_1" 1206)
    |}]
;;
