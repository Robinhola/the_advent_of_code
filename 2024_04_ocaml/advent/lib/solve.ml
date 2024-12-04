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

  let to_tuple t = t.x, t.y
end

type matrix =
  { words : char array array
  ; dims : Coord.t
  }
[@@deriving sexp]

let parse' l =
  let words = List.map l ~f:String.to_array |> List.to_array in
  let dims = { Coord.x = Array.length words.(0); y = Array.length words } in
  { words; dims }
;;

module Dir = struct
  type t =
    | Up
    | Down
    | Left
    | Right
    | Up_right
    | Up_left
    | Down_right
    | Down_left
  [@@deriving sexp, enumerate]

  let for_all ~f = List.map ~f all

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
end

let next max_coord coord direction =
  let mx, my = Coord.to_tuple max_coord in
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

let rec look ~matrix (coord : Coord.t) direction chars =
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

let all_indices matrix =
  List.cartesian_product (List.range 0 matrix.dims.x) (List.range 0 matrix.dims.y)
;;

let sum (l : 'a list) = List.sum (module Int) l

let part1 (lines : string list) =
  let word = String.to_list "XMAS" in
  let matrix = parse' lines in
  let coords = all_indices matrix in
  sum coords ~f:(fun (x, y) ->
    Dir.for_all ~f:(fun dir -> look ~matrix { x; y } dir word)
    |> List.sum (module Int) ~f:Fn.id)
;;

let start_of max_coord coord dir =
  (* this works because we only go one in the opposite direction to find the start *)
  let dir = Dir.other_way dir in
  next max_coord coord dir
;;

let xlook ~matrix (center : Coord.t) =
  let chars = "MAS" |> String.to_list in
  let is_valid direction =
    let coord = start_of matrix.dims center direction in
    Option.map coord ~f:(fun coord -> look ~matrix coord direction chars |> Int.equal 1)
    |> Option.value ~default:false
  in
  List.filter_map
    Dir.
      [ Down_right, Up_right
      ; Up_right, Up_left
      ; Up_left, Down_left
      ; Down_left, Down_right
      ]
    ~f:(fun (a, b) -> if is_valid a && is_valid b then Some (center, a, b) else None)
;;

let part2' (lines : string list) =
  let matrix = parse' lines in
  let coords = all_indices matrix in
  List.map coords ~f:(fun (x, y) -> xlook ~matrix { x; y })
;;

let part2 (lines : string list) = sum ~f:List.length (part2' lines)

let%expect_test _ =
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  part2' sample_1
  |> List.filter ~f:(Fn.compose not List.is_empty)
  |> List.concat
  |> List.iter ~f:(fun xmas -> print_s [%message (xmas : Coord.t * Dir.t * Dir.t)]);
  [%expect
    {|
    ("part1 sample_1" 18)
    ("part2 sample_1" 9)
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
