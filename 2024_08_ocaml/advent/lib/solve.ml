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

let rec antipods t start offset current =
  let antipod = Coord.apply start ~offset in
  if Matrix.within_bounds t.matrix antipod
  then antipods t antipod offset (antipod :: current)
  else current
;;

let antipods' t with_resonance (a, b) =
  if Char.equal a.value b.value && not (Coord.equal a.pos b.pos)
  then (
    let a = a.pos in
    let b = b.pos in
    let offset = Coord.offset a b in
    let antipod = Coord.apply b ~offset in
    match with_resonance with
    | `Part1 -> Option.some_if (Matrix.within_bounds t.matrix antipod) [ antipod ]
    | `Part2 -> Some (antipods t b offset [ b ]))
  else None
;;

let antipods'' t with_resonance =
  List.cartesian_product t.antennas t.antennas
  |> List.filter_map ~f:(antipods' t with_resonance)
  |> List.concat
  |> Coord.Set.of_list
;;

let partx (lines : string list) with_resonance =
  let t = parse lines in
  let antipods = antipods'' t with_resonance in
  Set.length antipods
;;

let part1 (lines : string list) = partx lines `Part1
let part2 (lines : string list) = partx lines `Part2

let%expect_test _ =
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  let t = parse sample_1 in
  let antipods = antipods'' t `Part2 in
  Set.iter antipods ~f:(fun coord -> Matrix.set t.matrix coord '#');
  Matrix.print t.matrix;
  [%expect
    {|
    ("part1 sample_1" 14)
    ("part2 sample_1" 34)
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
