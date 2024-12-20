open! Base
open! Core
open! Robin_advent_lib

let sample_1 =
  {|###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############|}
  |> String.split_lines
;;

let rec solve ~m ?(seen = Coord.Hashtbl.create ()) c i =
  Hashtbl.add_exn seen ~key:c ~data:i;
  if Matrix.get m c |> Char.equal 'E'
  then seen
  else (
    let next =
      [ Dir.Up; Dir.Right; Dir.Down; Dir.Left ]
      |> List.find_map_exn ~f:(fun dir ->
        match Matrix.next m c dir with
        | None -> None
        | Some c' when Matrix.get m c' |> Char.equal '#' -> None
        | Some c' when Hashtbl.mem seen c' -> None
        | Some c' -> Some c')
    in
    solve ~m ~seen next (i + 1))
;;

type t =
  { m : Matrix.t
  ; path : int Coord.Hashtbl.t
  }
[@@deriving sexp]

let parse lines =
  let m = Matrix.parse lines in
  let start =
    Matrix.all_indices m |> List.find_exn ~f:(fun c -> Matrix.get m c |> Char.equal 'S')
  in
  let path = solve ~m start 0 in
  { m; path }
;;

let all_from m (c : Coord.t) d =
  let till_d = List.range ~start:`inclusive ~stop:`inclusive (-d) d in
  List.cartesian_product till_d till_d
  |> List.filter_map ~f:(fun (x, y) ->
    if abs (x + y) > d
    then None
    else (
      let c = Coord.{ x = c.x + x; y = c.y + y } in
      Option.some_if
        (Matrix.within_bounds m c && not (Matrix.get m c |> Char.equal '#'))
        c))
;;

let%expect_test _ =
  let t = parse sample_1 in
  print_s [%message (all_from t.m (Coord.of_tuple (0, 0)) 2 : Coord.t list)];
  print_s [%message (all_from t.m (Coord.of_tuple (5, 5)) 2 : Coord.t list)];
  [%expect
    {|
    ("all_from t.m (Coord.of_tuple (0, 0)) 2" (((x 1) (y 1))))
    ("all_from t.m (Coord.of_tuple (5, 5)) 2"
     (((x 3) (y 7)) ((x 4) (y 7)) ((x 5) (y 3)) ((x 5) (y 7)) ((x 7) (y 3))
      ((x 7) (y 4)) ((x 7) (y 5))))
    |}]
;;

let partx lines max_d save_at_least =
  let t = parse lines in
  let valid =
    t.path
    |> Hashtbl.to_alist
    |> List.concat_map ~f:(fun (start, d) ->
      all_from t.m start max_d
      |> List.filter ~f:(fun end_ ->
        match Hashtbl.find t.path end_ with
        | None -> false
        | Some d' ->
          let distance_from_one_another =
            abs (end_.x - start.x) + abs (end_.y - start.y)
          in
          distance_from_one_another <= max_d
          && d' - d - distance_from_one_another >= save_at_least))
  in
  List.length valid
;;

let part1 (lines : string list) = partx lines 2 100
let part2 (lines : string list) = partx lines 20 100

let%expect_test _ =
  let t = parse sample_1 in
  print_s [%message (Hashtbl.length t.path : int)];
  print_s [%message (partx sample_1 2 20 : int)];
  print_s [%message (partx sample_1 20 74 : int)];
  [%expect
    {|
    ("Hashtbl.length t.path" 85)
    ("partx sample_1 2 20" 5)
    ("partx sample_1 20 74" 7)
    |}]
;;
