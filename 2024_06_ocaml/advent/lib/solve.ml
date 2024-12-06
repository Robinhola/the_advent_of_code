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
  let visited = Coord.Hashtbl.of_alist_exn [ guard, [ Dir.Up ] ] in
  let possible_obstructions = Coord.Hash_set.create () in
  { matrix; guard; dir; visited; possible_obstructions }
;;

let rec move state =
  let next_move = Matrix.next state.matrix state.guard state.dir in
  match next_move with
  | None -> `Not_loop state
  | Some c when is_obstacle state.matrix c ->
    let dir = Dir.rotate state.dir in
    let guard = state.guard in
    move { state with dir; guard }
  | Some c ->
    if Hashtbl.find_multi state.visited c |> List.exists ~f:(Dir.equal state.dir)
    then `Loop
    else (
      Hashtbl.add_multi state.visited ~key:c ~data:state.dir;
      let guard = c in
      move { state with guard })
;;

let find_all_loops (state : state) (og_guard : Coord.t) =
  let would_create_a_loop coord =
    Matrix.set state.matrix coord '#';
    let visited = Coord.Hashtbl.of_alist_exn [ og_guard, [ Dir.Up ] ] in
    let () =
      match move { state with guard = og_guard; dir = Dir.Up; visited } with
      | `Not_loop _ -> ()
      | `Loop -> Hash_set.add state.possible_obstructions coord
    in
    Matrix.set state.matrix coord '.'
  in
  state.visited
  |> Hashtbl.keys
  |> List.filter ~f:(Fn.compose not (Coord.equal og_guard))
  |> List.iter ~f:would_create_a_loop
;;

let part1 (lines : string list) =
  let state = make lines in
  match move state with
  | `Loop -> raise_s [%message "Found a loop in part1"]
  | `Not_loop final_state -> Hashtbl.length final_state.visited
;;

let part2 (lines : string list) =
  let state = make lines in
  let final_state =
    match move state with
    | `Loop -> raise_s [%message "Found a loop in part2"]
    | `Not_loop final_state -> final_state
  in
  find_all_loops final_state state.guard;
  Hash_set.length final_state.possible_obstructions
;;

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
