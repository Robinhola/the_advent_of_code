open! Base
open! Core

type t =
  { words : char array array
  ; dims : Coord.t
  }
[@@deriving sexp]

let within_bounds t (c : Coord.t) =
  let m = t.dims in
  not (c.x < 0 || c.y < 0 || c.x >= m.x || c.y >= m.y)
;;

let print t =
  Array.to_list t.words |> List.map ~f:String.of_array |> List.iter ~f:print_endline
;;

let parse l =
  let words = List.map l ~f:String.to_array |> List.to_array in
  let dims = { Coord.x = Array.length words.(0); y = Array.length words } in
  { words; dims }
;;

let make (dims : Coord.t) v =
  let l = List.range 0 dims.y |> List.map ~f:(fun _ -> String.make dims.x v) in
  parse l
;;

let next t coord direction =
  let mx, my = Coord.to_tuple t.dims in
  let x, y = Coord.to_tuple coord in
  let ox, oy =
    match direction with
    | Dir.Up -> 0, -1
    | Down -> 0, 1
    | Left -> -1, 0
    | Right -> 1, 0
    | Up_right -> 1, -1
    | Up_left -> -1, -1
    | Down_right -> 1, 1
    | Down_left -> -1, 1
  in
  match x + ox, y + oy with
  | nx, ny when Int.equal nx x && Int.equal ny y -> None
  | nx, ny when nx < 0 || ny < 0 -> None
  | nx, ny when nx >= mx || ny >= my -> None
  | nx, ny -> Some { Coord.x = nx; y = ny }
;;

let all_within t c d =
  let till_d = List.range ~start:`inclusive ~stop:`inclusive (-d) d in
  List.cartesian_product till_d till_d
  |> List.filter_map ~f:(fun (x, y) ->
    if abs (x + y) > d
    then None
    else (
      let c = Coord.{ x = c.x + x; y = c.y + y } in
      Option.some_if (within_bounds t c) c))
;;

let%expect_test _ =
  let t =
    "....#.....\n\
     .........#\n\
     ..........\n\
     ..#.......\n\
     .......#..\n\
     ..........\n\
     .#..^.....\n\
     ........#.\n\
     #.........\n\
     ......#..."
    |> String.split_lines
    |> parse
  in
  print_s [%message (all_within t (Coord.of_tuple (0, 0)) 2 : Coord.t list)];
  print_s [%message (all_within t (Coord.of_tuple (5, 5)) 2 : Coord.t list)];
  [%expect
    {|
    ("all_within t (Coord.of_tuple (0, 0)) 2"
     (((x 0) (y 0)) ((x 0) (y 1)) ((x 0) (y 2)) ((x 1) (y 0)) ((x 1) (y 1))
      ((x 2) (y 0))))
    ("all_within t (Coord.of_tuple (5, 5)) 2"
     (((x 3) (y 5)) ((x 3) (y 6)) ((x 3) (y 7)) ((x 4) (y 4)) ((x 4) (y 5))
      ((x 4) (y 6)) ((x 4) (y 7)) ((x 5) (y 3)) ((x 5) (y 4)) ((x 5) (y 5))
      ((x 5) (y 6)) ((x 5) (y 7)) ((x 6) (y 3)) ((x 6) (y 4)) ((x 6) (y 5))
      ((x 6) (y 6)) ((x 7) (y 3)) ((x 7) (y 4)) ((x 7) (y 5))))
    |}]
;;

let all_indices t =
  List.cartesian_product (List.range 0 t.dims.y) (List.range 0 t.dims.x)
  |> List.map ~f:(fun (y, x) -> Coord.{ x; y })
;;

let%expect_test _ =
  let t = { words = Array.of_list []; dims = Coord.{ x = 2; y = 3 } } in
  all_indices t |> List.iter ~f:(fun c -> print_s [%sexp (c : Coord.t)]);
  [%expect
    {|
    ((x 0) (y 0))
    ((x 1) (y 0))
    ((x 0) (y 1))
    ((x 1) (y 1))
    ((x 0) (y 2))
    ((x 1) (y 2))
    |}]
;;

let get t (coord : Coord.t) = t.words.(coord.y).(coord.x)
let set t (coord : Coord.t) value = Array.set t.words.(coord.y) coord.x value

let%expect_test _ =
  let t =
    "....#.....\n\
     .........#\n\
     ..........\n\
     ..#.......\n\
     .......#..\n\
     ..........\n\
     .#..^.....\n\
     ........#.\n\
     #.........\n\
     ......#..."
    |> String.split_lines
    |> parse
  in
  let open Coord in
  print_s [%message (get t { x = 4; y = 6 } : char)];
  print_s [%message (set t { x = 4; y = 6 } 'R' : unit)];
  print_s [%message (get t { x = 4; y = 6 } : char)];
  [%expect
    {|
    ("get t { x = 4; y = 6 }" ^)
    ("set t { x = 4; y = 6 } 'R'" ())
    ("get t { x = 4; y = 6 }" R)
    |}]
;;

let iteri t ~f = all_indices t |> List.iter ~f:(fun c -> f c (get t c))

let transpose t =
  let t' = make (Coord.swap t.dims) 'x' in
  iteri t ~f:(fun c x ->
    let c' = Coord.swap c in
    set t' c' x);
  t'
;;

let dims t = t.dims

let%expect_test _ =
  let t = "123456\n123456" |> String.split_lines |> parse in
  print_s [%message (t : t)];
  print_s [%message (transpose t : t)];
  [%expect
    {|
    (t ((words ((1 2 3 4 5 6) (1 2 3 4 5 6))) (dims ((x 6) (y 2)))))
    ("transpose t"
     ((words ((1 1) (2 2) (3 3) (4 4) (5 5) (6 6))) (dims ((x 2) (y 6)))))
    |}]
;;

let to_list t = Array.to_list t.words |> List.map ~f:Array.to_list

let%expect_test _ =
  let t = "123456\n123456" |> String.split_lines |> parse in
  print_s [%message (t : t)];
  print_s [%message (to_list t : char list list)];
  [%expect
    {|
    (t ((words ((1 2 3 4 5 6) (1 2 3 4 5 6))) (dims ((x 6) (y 2)))))
    ("to_list t" ((1 2 3 4 5 6) (1 2 3 4 5 6)))
    |}]
;;
