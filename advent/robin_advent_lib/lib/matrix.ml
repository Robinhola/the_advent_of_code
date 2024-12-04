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
  List.cartesian_product (List.range 0 t.dims.x) (List.range 0 t.dims.y)
  |> List.map ~f:Coord.of_tuple
;;
