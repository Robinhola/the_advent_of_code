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
  ; visited : Dir.t list Coord.Map.t
  ; possible_obstructions : unit Coord.Hashtbl.t
  }
[@@deriving sexp]

let is_obstacle state coord = Matrix.get state coord |> Char.equal '#'

let make l : state =
  let matrix = Matrix.parse l in
  let guard =
    Matrix.all_indices matrix
    |> List.find_exn ~f:(fun coord ->
      let x, y = Coord.to_tuple coord in
      Char.equal matrix.words.(y).(x) '^')
  in
  let () = Matrix.set matrix guard '.' in
  let dir = Dir.Up in
  let visited = Coord.Map.of_alist_exn [ guard, [ Dir.Up ] ] in
  let possible_obstructions = Coord.Hashtbl.create () in
  { matrix; guard; dir; visited; possible_obstructions }
;;

let in_map m c = Map.find m c |> Option.is_some
let in_hashset s c = Hashtbl.find s c |> Option.is_some

(* Some branches will loop, some branches (like the default one) will end outside *)
let rec explore_graph state can_change =
  match Matrix.next state.matrix state.guard state.dir with
  | None -> `Not_loop state
  | Some next_pos when is_obstacle state.matrix next_pos ->
    explore_graph { state with dir = Dir.rotate state.dir } can_change
  | Some next_pos
    when Map.find_multi state.visited next_pos |> List.exists ~f:(Dir.equal state.dir) ->
    `Loop
  | Some next_pos ->
    if
      can_change
      && (not (in_hashset state.possible_obstructions next_pos))
      && not (in_map state.visited next_pos)
    then (
      Matrix.set state.matrix next_pos '#';
      let () =
        match explore_graph state false with
        | `Loop -> Hashtbl.add_exn state.possible_obstructions ~key:next_pos ~data:()
        | `Not_loop _ -> ()
      in
      Matrix.set state.matrix next_pos '.');
    explore_graph
      { state with
        guard = next_pos
      ; visited = Map.add_multi state.visited ~key:next_pos ~data:state.dir
      }
      can_change
;;

let partx (lines : string list) can_change =
  let state = make lines in
  match explore_graph state can_change with
  | `Loop -> raise_s [%message "Found a loop in the main branch"]
  | `Not_loop state -> state
;;

let part1 (lines : string list) = Map.length (partx lines false).visited
let part2 (lines : string list) = Hashtbl.length (partx lines true).possible_obstructions

let%expect_test _ =
  let state = make sample_1 in
  print_s [%message (state : state)];
  let result1 = part1 sample_1 in
  let result2 = part2 sample_1 in
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
    (result2 6)
    |}]
;;
