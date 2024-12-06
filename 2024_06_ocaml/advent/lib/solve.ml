open! Base
open! Core
open! Robin_advent_lib

let sample_1 =
  {|....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...|}
  |> String.split_lines
;;

type state =
  { matrix : Matrix.t
  ; guard : Coord.t
  ; dir : Dir.t
  ; visited : Dir.t list Coord.Hashtbl.t
  ; possible_obstructions : Coord.Hash_set.t
  }
[@@deriving sexp]

let set (matrix : Matrix.t) coord value =
  let x, y = Coord.to_tuple coord in
  Array.set matrix.words.(y) x value
;;

let get (matrix : Matrix.t) coord =
  let x, y = Coord.to_tuple coord in
  matrix.words.(y).(x)
;;

let is_obstacle state coord = get state coord |> Char.equal '#'

let rotate = function
  | Dir.Up -> Dir.Right
  | Right -> Dir.Down
  | Down -> Dir.Left
  | Left -> Dir.Up
  | Up_right -> Down_right
  | Down_right -> Down_left
  | Down_left -> Up_left
  | Up_left -> Up_right
;;

let make l : state =
  let matrix = Matrix.parse l in
  let guard =
    Matrix.all_indices matrix
    |> List.find_exn ~f:(fun coord ->
      let x, y = Coord.to_tuple coord in
      Char.equal matrix.words.(y).(x) '^')
  in
  let () = set matrix guard '.' in
  let dir = Dir.Up in
  let visited = Coord.Hashtbl.of_alist_exn [ guard, [ Dir.Up ] ] in
  let possible_obstructions = Coord.Hash_set.create () in
  { matrix; guard; dir; visited; possible_obstructions }
;;

let would_loop state =
  (* whether adding an obstacle would lead to an already visited path with new dir *)
  let diversion = rotate state.dir in
  let if_diverted = Matrix.next state.matrix state.guard diversion in
  let diverted_path =
    let%bind.Option if_diverted = if_diverted in
    Hashtbl.find_multi state.visited if_diverted |> List.find ~f:(Dir.equal diversion)
  in
  Option.is_some diverted_path
;;

let rec move state =
  let next_move = Matrix.next state.matrix state.guard state.dir in
  match next_move with
  | None -> state
  | Some c when is_obstacle state.matrix c ->
    let dir = rotate state.dir in
    let guard = state.guard in
    move { state with dir; guard }
  | Some c ->
    if would_loop state then Hash_set.add state.possible_obstructions c;
    Hashtbl.add_multi state.visited ~key:c ~data:state.dir;
    let guard = c in
    move { state with guard }
;;

let part1 (lines : string list) =
  let state = make lines in
  let final_state = move state in
  Hashtbl.length final_state.visited
;;

let part2 (lines : string list) =
  let state = make lines in
  let final_state = move state in
  Hash_set.length final_state.possible_obstructions
;;

let%expect_test _ =
  let state = make sample_1 in
  let result1 = part1 sample_1 in
  let result2 = part2 sample_1 in
  print_s [%message (state : state)];
  print_s [%message (result1 : int)];
  print_s [%message (result2 : int)];
  [%expect
    {|
    (state
     ((matrix
       ((words
         ((. . . . # . . . . .) (. . . . . . . . . #) (. . . . . . . . . .)
          (. . # . . . . . . .) (. . . . . . . # . .) (. . . . . . . . . .)
          (. # . . . . . . . .) (. . . . . . . . # .) (# . . . . . . . . .)
          (. . . . . . # . . .)))
        (dims ((x 10) (y 10)))))
      (guard ((x 4) (y 6))) (dir Up) (visited ((((x 4) (y 6)) (Up))))
      (possible_obstructions ())))
    (result1 41)
    (result2 3)
    |}]
;;
