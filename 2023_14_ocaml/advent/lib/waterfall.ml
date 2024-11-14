open! Base
open! Core
open Dish

type state =
  { offsets : int array
  ; current : int
  }

let prepare t : state =
  let head = Array.get t 0 in
  let offsets = Array.map head ~f:(fun _ -> 0) in
  let current = 0 in
  { offsets; current }
;;

let update t state =
  let y = state.current in
  let line = Array.get t y in
  Array.iteri line ~f:(fun x c ->
    let o = Array.get state.offsets x in
    match c with
    | '.' -> Array.set state.offsets x (o + 1)
    | '#' -> Array.set state.offsets x 0
    | 'O' ->
      let y' = y - o in
      let line' = Array.get t y' in
      let () = Array.set line x '.' in
      let () = Array.set line' x 'O' in
      ()
    | c -> raise_s [%message "Unknown char??" (c : char)])
;;

let rec waterfall' t state =
  match state.current with
  | n when Int.equal n (Array.length t) -> t
  | i ->
    let () = update t state in
    waterfall' t { state with current = i + 1 }
;;

let waterfall t =
  let state = prepare t in
  waterfall' t state
;;

let%expect_test _ =
  let t =
    parse
      ("O....#....\n\
        O.OO#....#\n\
        .....##...\n\
        OO.#O....O\n\
        .O.....O#.\n\
        O.#..O.#.#\n\
        ..O..#O..O\n\
        .......O..\n\
        #....###..\n\
        #OO..#...."
       |> String.split_lines)
  in
  let t = waterfall t in
  print_s [%message (count t : int)];
  [%expect {| ("count t" 136) |}];
  Array.iter t ~f:(Fn.compose print_endline String.of_array);
  [%expect
    {|
    OOOO.#.O..
    OO..#....#
    OO..O##..O
    O..#.OO...
    ........#.
    ..#....#.#
    ..O..#.O.O
    ..O.......
    #....###..
    #....#....
    |}]
;;
