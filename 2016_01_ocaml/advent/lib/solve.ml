open! Base
open! Core
open! Robin_advent_lib

let sample_1 = {|R5, L5, R5, R3|} |> String.split_lines

let parse s =
  String.split s ~on:','
  |> List.map ~f:(fun line ->
    match String.to_list line with
    | ' ' :: 'R' :: rest | 'R' :: rest -> `R (rest |> String.of_list |> Int.of_string)
    | ' ' :: 'L' :: rest | 'L' :: rest -> `L (rest |> String.of_list |> Int.of_string)
    | s -> raise_s [%message "Wrong string" (s : char list)])
;;

type state =
  { dir : Dir.t
  ; pos : Coord.t
  ; seen : Coord.Hash_set.t
  }

let rotate' = Fn.apply_n_times ~n:3 Dir.rotate

let offset dir d =
  let open Coord in
  match dir with
  | Dir.Up -> { x = 0; y = d }
  | Dir.Left -> { x = -d; y = 0 }
  | Dir.Right -> { x = d; y = 0 }
  | Dir.Down -> { x = 0; y = -d }
  | d -> raise_s [%message (d : Dir.t)]
;;

let part1 (lines : string list) =
  let instructions = String.concat lines |> parse in
  let state =
    { dir = Dir.Up; pos = Coord.{ x = 0; y = 0 }; seen = Coord.Hash_set.create () }
  in
  let final_state =
    List.fold instructions ~init:state ~f:(fun state ins ->
      match ins with
      | `R d ->
        let dir = Dir.rotate state.dir in
        let pos = Coord.apply state.pos ~offset:(offset dir d) in
        { state with dir; pos }
      | `L d ->
        let dir = rotate' state.dir in
        let pos = Coord.apply state.pos ~offset:(offset dir d) in
        { state with dir; pos })
  in
  final_state.pos.x + final_state.pos.y
;;

let rec move state to_ =
  if Coord.equal state.pos to_
  then `Continue state
  else (
    match Coord.apply state.pos ~offset:(offset state.dir 1) with
    | pos when Hash_set.mem state.seen pos -> `Already_seen { state with pos }
    | pos ->
      Hash_set.add state.seen pos;
      move { state with pos } to_)
;;

let part2 (lines : string list) =
  let instructions = String.concat lines |> parse in
  let state =
    let pos = Coord.{ x = 0; y = 0 } in
    { dir = Dir.Up; pos; seen = Coord.Hash_set.of_list [ pos ] }
  in
  let final_state =
    List.fold_until
      instructions
      ~init:state
      ~f:(fun state ins ->
        let dir, d =
          match ins with
          | `R d ->
            let dir = Dir.rotate state.dir in
            dir, d
          | `L d ->
            let dir = rotate' state.dir in
            dir, d
        in
        let pos = Coord.apply state.pos ~offset:(offset dir d) in
        match move { state with dir } pos with
        | `Continue state -> Continue state
        | `Already_seen state -> Stop state)
      ~finish:Fn.id
  in
  final_state.pos.x + final_state.pos.y
;;

let%expect_test _ =
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 [ "R8, R4, R4, R8" ] : int)];
  [%expect
    {|
    ("part1 sample_1" 12)
    ("part2 [\"R8, R4, R4, R8\"]" 4)
    |}]
;;
