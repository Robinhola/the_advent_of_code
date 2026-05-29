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
  {|7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3|}
  |> String.split_lines
;;

let area (c1 : Coord.t) (c2 : Coord.t) =
  let width = if c1.x > c2.x then c1.x - c2.x + 1 else c2.x - c1.x + 1 in
  let height = if c1.y > c2.y then c1.y - c2.y + 1 else c2.y - c1.y + 1 in
  width * height
;;

let part1 (lines : string list) =
  let coords =
    List.map lines ~f:(fun s ->
      match String.split s ~on:',' with
      | [ x; y ] -> Coord.of_tuple (Int.of_string x, Int.of_string y)
      | _ -> impossible ())
  in
  debug [%message (coords : Coord.t list)];
  List.cartesian_product coords coords
  |> List.map ~f:(fun (a, b) -> area a b)
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn
;;

let is_horizontaly_green
      (start : Coord.t)
      (end_ : Coord.t)
      (all_points : (Coord.t * Coord.t) list)
  =
  let test = if start.x <= end_.y then Int.( >= ) else Int.( <= ) in
  let test' = if start.x <= end_.y then Int.( <= ) else Int.( >= ) in
  let same_height_further (b : Coord.t) = test b.x end_.x && end_.y = b.y in
  let same_width_further (a : Coord.t) (b : Coord.t) =
    a.x = b.x
    && test a.x end_.x
    && ((test a.y end_.y && test' b.y end_.y) || (test b.y end_.y && test' a.y end_.y))
  in
  List.find all_points ~f:(fun (a, b) ->
    same_height_further a || same_height_further b || same_width_further a b)
  |> Option.is_some
;;

let test coords (a : Coord.t) (b : Coord.t) =
  let sx = Int.min a.x b.x in
  let sy = Int.min a.y b.y in
  let ex = Int.max a.x b.x in
  let ey = Int.max a.y b.y in
  let start = Coord.of_tuple (sx, sy) in
  let end_ = Coord.of_tuple (ex, ey) in
  if
    (Coord.equal start a && Coord.equal end_ b)
    || (Coord.equal start b && Coord.equal end_ a)
  then (
    match
      ( is_horizontaly_green start (Coord.of_tuple (ex, sy)) coords
      , is_horizontaly_green end_ (Coord.of_tuple (sx, ey)) coords )
    with
    | true, true -> Some (area a b)
    | _, _ -> None)
  else None
;;

let part2 (lines : string list) =
  let coords =
    List.map lines ~f:(fun s ->
      match String.split s ~on:',' with
      | [ x; y ] -> Coord.of_tuple (Int.of_string x, Int.of_string y)
      | _ -> impossible ())
  in
  let coords = List.cartesian_product coords coords in
  print_endline "starting";
  let l =
    List.filter_map coords ~f:(fun (a, b) ->
      test coords a b |> Option.map ~f:(fun c -> a, b, c))
  in
  print_endline "end";
  List.map l ~f:(fun (a, b, c) -> c)
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn
;;

(*debug [%message (l : (Coord.t * Coord.t * int) list)];*)

(*|> List.max_elt ~compare:Int.compare*)
(*|> Option.value_exn*)

(*
   01234567890123
----------------
0 |..............
1 |.......#XXX#..
2 |.......XXXXX..
3 |..OXXXX#XXXX..
4 |..XXXXXXXXXX..
5 |..#XXXXXX#XX..
6 |.........XXX..
7 |.........#XO..
8 |..............
*)

let%expect_test _ =
  should_print_debug := true;
  let coords =
    List.map sample_1 ~f:(fun s ->
      match String.split s ~on:',' with
      | [ x; y ] -> Coord.of_tuple (Int.of_string x, Int.of_string y)
      | _ -> impossible ())
  in
  let coords = List.cartesian_product coords coords in
  print_s [%message (test coords { x = 9; y = 5 } { x = 2; y = 3 } : int option)];
  print_s [%message (test coords { x = 2; y = 3 } { x = 9; y = 5 } : int option)];
  print_s [%message (test coords { x = 11; y = 7 } { x = 2; y = 3 } : int option)];
  print_s [%message (test coords { x = 2; y = 3 } { x = 11; y = 7 } : int option)];
  print_s [%message (test coords { x = 9; y = 7 } { x = 11; y = 7 } : int option)];
  print_s [%message (test coords { x = 8; y = 7 } { x = 11; y = 7 } : int option)];
  print_s [%message (test coords { x = 7; y = 7 } { x = 11; y = 7 } : int option)];
  print_s [%message (test coords { x = 7; y = 5 } { x = 11; y = 7 } : int option)];
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    ("test coords { x = 9; y = 5 } { x = 2; y = 3 }" (24))
    ("test coords { x = 2; y = 3 } { x = 9; y = 5 }" (24))
    ("test coords { x = 11; y = 7 } { x = 2; y = 3 }" ())
    ("test coords { x = 2; y = 3 } { x = 11; y = 7 }" ())
    ("test coords { x = 9; y = 7 } { x = 11; y = 7 }" (3))
    ("test coords { x = 8; y = 7 } { x = 11; y = 7 }" ())
    ("test coords { x = 7; y = 7 } { x = 11; y = 7 }" ())
    ("test coords { x = 7; y = 5 } { x = 11; y = 7 }" ())
    (coords
     (((x 7) (y 1)) ((x 11) (y 1)) ((x 11) (y 7)) ((x 9) (y 7)) ((x 9) (y 5))
      ((x 2) (y 5)) ((x 2) (y 3)) ((x 7) (y 3))))
    ("part1 sample_1" 50)
    ("part2 sample_1" 24)
    |}]
;;
