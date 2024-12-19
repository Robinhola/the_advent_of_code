open! Base
open! Core
open! Robin_advent_lib

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

let rec look ~(matrix : Matrix.t) (coord : Coord.t) direction chars =
  let value = matrix.words.(coord.y).(coord.x) in
  let is_equal = Char.equal value in
  match chars with
  | [] -> raise_s [%message "Should not happen"]
  | c :: [] when is_equal c -> 1
  | c :: rest when is_equal c ->
    (match Matrix.next matrix coord direction with
     | None -> 0
     | Some coord -> look ~matrix coord direction rest)
  | _ -> 0
;;

let sum (l : 'a list) = List.sum (module Int) l

let part1 (lines : string list) =
  let word = String.to_list "XMAS" in
  let matrix = Matrix.parse lines in
  let coords = Matrix.all_indices matrix in
  sum coords ~f:(fun coord ->
    Dir.for_all ~f:(fun dir -> look ~matrix coord dir word) |> sum ~f:Fn.id)
;;

let start_of matrix coord dir =
  (* this works because we only go one in the opposite direction to find the start *)
  let dir = Dir.other_way dir in
  Matrix.next matrix coord dir
;;

let xlook ~matrix (center : Coord.t) =
  let chars = "MAS" |> String.to_list in
  let is_valid direction =
    let coord = start_of matrix center direction in
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
  let matrix = Matrix.parse lines in
  let coords = Matrix.all_indices matrix in
  List.map coords ~f:(xlook ~matrix)
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
