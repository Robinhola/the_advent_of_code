open! Base
open! Core

let sample_1 =
  {|MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX|}
  |> String.split_lines
;;

module Coord = struct
  module T = struct
    type t =
      { x : int
      ; y : int
      }
    [@@deriving sexp, compare, equal, hash]
  end

  include T
  module Map = Map.Make (T)
  module Hash_set = Hash_set.Make (T)
end

type coord = Coord.t [@@deriving sexp, compare, equal, hash]

let to_tuple (coord : Coord.t) = coord.x, coord.y

type matrix =
  { words : char array array
  ; dims : coord
  }
[@@deriving sexp]

let parse l = List.map l ~f:String.to_array |> List.to_array

let parse' l =
  let words = parse l in
  let dims = { Coord.x = Array.length words.(0); y = Array.length words } in
  { words; dims }
;;

type dir =
  | Up
  | Down
  | Left
  | Right
  | Up_right
  | Up_left
  | Down_right
  | Down_left
[@@deriving sexp, enumerate]

let next max_coord coord direction =
  let mx, my = to_tuple max_coord in
  let x, y = to_tuple coord in
  let ox, oy =
    match direction with
    | Up -> 0, -1
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

let rec look ~matrix (coord : coord) direction chars =
  let value = matrix.words.(coord.y).(coord.x) in
  let is_equal = Char.equal value in
  match chars with
  | [] -> raise_s [%message "Should not happen"]
  | c :: [] when is_equal c -> 1
  | c :: rest when is_equal c ->
    (match next matrix.dims coord direction with
     | None -> 0
     | Some coord -> look ~matrix coord direction rest)
  | _ -> 0
;;

let for_all_dir f = List.map ~f all_of_dir

let part1 (lines : string list) =
  let word = String.to_list "XMAS" in
  let matrix = parse' lines in
  let coords =
    List.cartesian_product (List.range 0 matrix.dims.x) (List.range 0 matrix.dims.y)
  in
  List.sum
    (module Int)
    coords
    ~f:(fun (x, y) ->
      for_all_dir (fun dir -> look ~matrix { Coord.x; y } dir word)
      |> List.sum (module Int) ~f:Fn.id)
;;

let other_way = function
  | Up -> Down
  | Down -> Up
  | Left -> Right
  | Right -> Left
  | Up_right -> Down_left
  | Up_left -> Down_right
  | Down_right -> Up_left
  | Down_left -> Up_right
;;

let start_of max_coord coord dir =
  let dir = other_way dir in
  next max_coord coord dir
;;

let xlook ~matrix (center : coord) =
  let chars = "MAS" |> String.to_list in
  let is_valid direction =
    let coord = start_of matrix.dims center direction in
    Option.map coord ~f:(fun coord -> look ~matrix coord direction chars |> Int.equal 1)
    |> Option.value ~default:false
  in
  [ Down_right, Up_right; Up_right, Up_left; Up_left, Down_left; Down_left, Down_right ]
  |> List.filter_map ~f:(fun (a, b) ->
    if is_valid a && is_valid b
    then (
      let result = center, a, b in
      Some result)
    else None)
;;

let part2' (lines : string list) =
  let matrix = parse' lines in
  let coords =
    List.cartesian_product (List.range 0 matrix.dims.x) (List.range 0 matrix.dims.y)
  in
  List.map coords ~f:(fun (x, y) ->
    let coord = { Coord.x; y } in
    xlook ~matrix coord)
;;

let part2 (lines : string list) = List.sum (module Int) (part2' lines) ~f:List.length

let%expect_test _ =
  print_s [%message (part1 sample_1 : int)];
  part2' sample_1
  |> List.filter ~f:(Fn.compose not List.is_empty)
  |> List.concat
  |> List.iter ~f:(fun xmas -> print_s [%message (xmas : coord * dir * dir)]);
  [%expect
    {|
    ("part1 sample_1" 18)
    (xmas (((x 1) (y 7)) Up_right Up_left))
    (xmas (((x 2) (y 1)) Down_right Up_right))
    (xmas (((x 2) (y 3)) Down_right Up_right))
    (xmas (((x 3) (y 7)) Up_right Up_left))
    (xmas (((x 4) (y 3)) Up_left Down_left))
    (xmas (((x 5) (y 7)) Up_right Up_left))
    (xmas (((x 6) (y 2)) Down_left Down_right))
    (xmas (((x 7) (y 2)) Up_right Up_left))
    (xmas (((x 7) (y 7)) Up_right Up_left))
    |}]
;;
