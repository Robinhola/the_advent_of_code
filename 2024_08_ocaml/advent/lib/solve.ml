open! Base
open! Core
open! Robin_advent_lib

let sample_1 =
  {|............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............|}
  |> String.split_lines
;;

type antenna =
  { pos : Coord.t
  ; value : char
  }
[@@deriving sexp]

type t =
  { matrix : Matrix.t
  ; antennas : antenna List.t
  }
[@@deriving sexp]

let parse l =
  let matrix = Matrix.parse l in
  let antennas =
    Matrix.all_indices matrix
    |> List.filter_map ~f:(fun c ->
      match Matrix.get matrix c with
      | '.' -> None
      | value -> Some { pos = c; value })
  in
  { matrix; antennas }
;;

let offset (a : Coord.t) (b : Coord.t) =
  let x = b.x - a.x in
  let y = b.y - a.y in
  Coord.{ x; y }
;;

let apply (b : Coord.t) (offset : Coord.t) =
  let x = b.x + offset.x in
  let y = b.y + offset.y in
  Coord.{ x; y }
;;

let within_bounds (matrix : Matrix.t) (c : Coord.t) =
  let mx, my = Coord.to_tuple matrix.dims in
  match Coord.to_tuple c with
  | x, y when x < 0 || y < 0 -> false
  | x, y when x >= mx || y >= my -> false
  | _ -> true
;;

let part1 (lines : string list) =
  let t = parse lines in
  let antipods =
    List.cartesian_product t.antennas t.antennas
    |> List.filter_map ~f:(fun (a, b) ->
      if Char.equal a.value b.value && not (Coord.equal a.pos b.pos)
      then (
        let a = a.pos in
        let b = b.pos in
        let antipod = apply b (offset a b) in
        Option.some_if (within_bounds t.matrix antipod) antipod)
      else None)
    |> Coord.Set.of_list
  in
  Set.length antipods
;;

let rec antipods t start offset current =
  let antipod = apply start offset in
  if within_bounds t.matrix antipod
  then antipods t antipod offset (antipod :: current)
  else current
;;

let antipods' t =
  List.cartesian_product t.antennas t.antennas
  |> List.filter_map ~f:(fun (a, b) ->
    if Char.equal a.value b.value && not (Coord.equal a.pos b.pos)
    then (
      let a = a.pos in
      let b = b.pos in
      Some (antipods t b (offset a b) [ b ]))
    else None)
  |> List.concat
  |> Coord.Set.of_list
;;

let%expect_test _ =
  let t = parse sample_1 in
  let antipods = antipods' t in
  Set.iter antipods ~f:(fun coord -> Matrix.set t.matrix coord '#');
  Array.iter t.matrix.words ~f:(fun line -> String.of_array line |> print_endline);
  [%expect
    {|
    ##....#....#
    .#.#....#...
    ..#.##....#.
    ..##...#....
    ....#....#..
    .#...##....#
    ...#..#.....
    #....#.#....
    ..#.....#...
    ....#....#..
    .#........#.
    ...#......##
    |}]
;;

let part2 (lines : string list) =
  let t = parse lines in
  Set.length (antipods' t)
;;

let%expect_test _ =
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect {|
    ("part1 sample_1" 14)
    ("part2 sample_1" 34)
    |}]
;;
