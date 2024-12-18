open! Base
open! Core

type t =
  { words : char array array
  ; dims : Coord.t
  }
[@@deriving sexp]

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

let within_bounds t (c : Coord.t) =
  let m = t.dims in
  not (c.x < 0 || c.y < 0 || c.x >= m.x || c.y >= m.y)
;;

let to_strings t = Array.to_list t.words |> List.map ~f:String.of_array
let print t = to_strings t |> List.iter ~f:print_endline
