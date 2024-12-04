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
  | DownUp_left
[@@deriving enumerate]

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
    | DownUp_left -> -1, 1
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

let part2 (_lines : string list) = 0

let%expect_test _ =
  let result = sample_1 |> part1 in
  print_s [%message (result : int)];
  [%expect {| (result 18) |}]
;;
