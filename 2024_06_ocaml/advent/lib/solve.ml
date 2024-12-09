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
  let visited = Coord.Map.of_alist_exn [ guard, [ Dir.Up ] ] in
  let possible_obstructions = Coord.Hash_set.create () in
  { matrix; guard; dir; visited; possible_obstructions }
;;

let rec _move state =
  let next_move = Matrix.next state.matrix state.guard state.dir in
  match next_move with
  | None -> `Not_loop state.visited
  | Some c when is_obstacle state.matrix c ->
    let dir = Dir.rotate state.dir in
    _move { state with dir }
  | Some c ->
    if Map.find_multi state.visited c |> List.exists ~f:(Dir.equal state.dir)
    then `Loop
    else (
      let visited = Map.add_multi state.visited ~key:c ~data:state.dir in
      let guard = c in
      _move { state with guard; visited })
;;

let find_all_loops (state : state) (og_guard : Coord.t) =
  let would_create_a_loop coord =
    Matrix.set state.matrix coord '#';
    let visited = Coord.Map.of_alist_exn [ og_guard, [ Dir.Up ] ] in
    let () =
      match _move { state with guard = og_guard; dir = Dir.Up; visited } with
      | `Not_loop _ -> ()
      | `Loop -> Hash_set.add state.possible_obstructions coord
    in
    Matrix.set state.matrix coord '.'
  in
  state.visited
  |> Map.keys
  |> List.filter ~f:(Fn.compose not (Coord.equal og_guard))
  |> List.iter ~f:would_create_a_loop
;;

(* Some branches will loop, some branches (like the default one) will end outside *)
let rec explore_graph state can_change =
  match Matrix.next state.matrix state.guard state.dir with
  | None -> `Not_loop state
  | Some next_pos when is_obstacle state.matrix next_pos ->
    let dir = Dir.rotate state.dir in
    explore_graph { state with dir } can_change
  | Some next_pos
    when Map.find_multi state.visited next_pos |> List.exists ~f:(Dir.equal state.dir) ->
    `Loop
  | Some next_pos ->
    if can_change
    then (
      Matrix.set state.matrix next_pos '#';
      assert (is_obstacle state.matrix next_pos);
      let () =
        match explore_graph state false with
        | `Not_loop _ -> ()
        | `Loop -> Hash_set.add state.possible_obstructions next_pos
      in
      assert (is_obstacle state.matrix next_pos);
      Matrix.set state.matrix next_pos '.');
    let visited = Map.add_multi state.visited ~key:next_pos ~data:state.dir in
    let guard = next_pos in
    explore_graph { state with guard; visited } can_change
;;

let partx (lines : string list) can_change =
  let state = make lines in
  match explore_graph state can_change with
  | `Loop -> raise_s [%message "Found a loop in the main branch"]
  | `Not_loop state -> state
;;

let part2' (lines : string list) =
  let state = make lines in
  let visited =
    match _move state with
    | `Loop -> raise_s [%message "Found a loop in part2"]
    | `Not_loop visited -> visited
  in
  find_all_loops { state with visited } state.guard;
  state
;;

let part1 (lines : string list) = Map.length (partx lines false).visited

let part2 (lines : string list) =
  print_endline "######### PART2 ######";
  let state = partx lines true in
  print_endline "";
  print_endline "######### PART2' ######";
  print_s [%message (Hash_set.length (part2' lines).possible_obstructions : int)];
  print_endline "";
  Hash_set.length state.possible_obstructions
;;

let%expect_test _ =
  let state = make sample_1 in
  print_s [%message (state : state)];
  (* print_s [%message (partx sample_1 false : state)]; *)
  (* print_s [%message (partx sample_1 true : state)]; *)
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
    ######### PART2 ######

    ######### PART2' ######

    (result1 41)
    (result2 6)
    |}]
;;
