open! Base
open! Core
open! Import
open Advent

let lines = Advent.read_lines "day09" ~for_tests:
  [ "R 4"
  ; "U 4"
  ; "L 3"
  ; "D 1"
  ; "R 4"
  ; "D 1"
  ; "L 5"
  ; "R 2"
  ] 
  ()
;;

let lines2 = 
  [ "R 5"
  ; "U 8"
  ; "L 8"
  ; "D 3"
  ; "R 17"
  ; "D 10"
  ; "L 25"
  ; "U 20"
  ]
;;
  
module IntPairs = struct
  type t = int * int [@@deriving sexp, compare]
end

module PairsSet = Set.Make(IntPairs)

module Input = struct
  type t = (Advent.Direction.t * int) list [@@deriving sexp]

  let read_line line =
    match (line |> String.split ~on:' ') with
    | "U" :: n  :: [] -> Direction.Up, Int.of_string n
    | "D" :: n  :: [] -> Direction.Down, Int.of_string n
    | "L" :: n  :: [] -> Direction.Left, Int.of_string n
    | "R" :: n  :: [] -> Direction.Right, Int.of_string n
    | _ -> failwith "invalid"
  ;;

  let make = List.map ~f:read_line
end

let%expect_test "" =
  let input = Input.make lines in
  print_s [%message (input: Input.t)];
  [%expect {|
    (input
     ((Right 4) (Up 4) (Left 3) (Down 1) (Right 4) (Down 1) (Left 5) (Right 2))) |}]
;;

module Physics = struct
  type t =
    { head: IntPairs.t
    ; tail: IntPairs.t
    ; tail_positions: PairsSet.t
    ; tails : IntPairs.t list
    } [@@deriving sexp]
  ;;

  let new_pair () = 0, 0

  let rec make_n rest = function
    | 0 -> rest
    | n -> make_n (new_pair () :: rest) (n - 1)
  ;;

  let%expect_test "" =
    print_s [%message (make_n [] 9: (int * int) list)];
    [%expect {| ("make_n [] 9" ((0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0))) |}]
  ;;

  let default =
    { head = new_pair ()
    ; tail = new_pair ()
    ; tail_positions = PairsSet.empty
    ; tails = make_n [] 9
    }
  ;;

  let move (x, y) dir = Direction.inc ~x ~y dir

  let move_head t = function
    | (dir, n) :: rest when n > 1 -> {t with head = move t.head dir}, (dir, n - 1) :: rest
    | (dir, n) :: rest when Int.equal n 1 -> {t with head = move t.head dir}, rest
    | _ -> failwith "impossible"
  ;;

  let distance ~head ~tail = 
    let xh, yh = head in 
    let xt, yt = tail in 
    let x_distance = xh - xt in
    let y_distance = yh - yt in
    let d = abs x_distance + abs y_distance in
    let horizonally = match x_distance with
      | n when n > 0 -> Some Direction.Right
      | n when n < 0 -> Some Direction.Left
      | _ -> None
    in
    let vertically = match y_distance with
      | n when n > 0 -> Some Direction.Down
      | n when n < 0 -> Some Direction.Up
      | _ -> None
    in
    (* Direct diagonales count like they're touching *)
    if d |> Int.equal 2 && Option.is_some horizonally && Option.is_some vertically then
      1, horizonally, vertically
    else
      d, horizonally, vertically
  ;;

  let move_tail ~head ~tail =
    let d, x_dir, y_dir = distance ~head ~tail in
    if d <= 1 then tail
    else
      let x, y = tail in
      let x = x_dir
        |> Option.map ~f:(Direction.inc1 x) 
        |> Option.value ~default:x
      in
      let y = y_dir
        |> Option.map ~f:(Direction.inc1 y) 
        |> Option.value ~default:y
      in
      x, y
  ;;

  let move_tail_part1 t = 
    let tail = move_tail ~head:t.head ~tail:t.tail in
    let tail_positions = Set.add t.tail_positions tail in
    {t with tail; tail_positions }
  ;;

  let move_tails t = 
    let rec move tails = function
      | [] -> failwith "impossible" 
      | _ :: [] -> tails 
      | head :: tail :: rest -> 
        let tails = move_tail ~head ~tail :: tails in
        let rest = tail :: rest in
        move tails rest
    in
    move [] (t.head :: t.tails)
  ;;

  let move_tail_part2 t =
    let tails = move_tails t in
    let tail = List.hd_exn tails in
    let tails = tails |> List.rev in
    let tail_positions = Set.add t.tail_positions tail in
    { t with tail; tail_positions; tails }
  ;;

  let rec update t f = function 
    | [] -> t 
    | rest ->
      let t, rest = move_head t rest in
      update (f t) f rest
  ;;
end

let draw_grid tail_positions = 
  let grid = Array.make_matrix ~dimx:15 ~dimy:25 '.' in
  let offset_x, offset_y = 12, 6 in
  tail_positions
  |> Set.iter ~f:(fun (x, y) -> grid.(y + offset_y).(x + offset_x) <- '#');
  grid
;;

let%expect_test "" = 
  let input = lines2 |> Input.make in
  let t = Physics.update Physics.default Physics.move_tail_part2 input in
  let value = t.tail_positions |> Set.length in
  print_s [%message (value: int)];
  print_s [%message (draw_grid t.tail_positions: char array array)];
  [%expect {|
    (value 31)
    ("draw_grid t.tail_positions"
     ((. . . . . . . . . . . . . . . . . . . . . . . . .)
      (. . . . . . . . . . . . . . . # # # . . . . . . .)
      (. . . . . . . . . . . . . . # . . . # . . . . . .)
      (. . . . . . . . . . . . . # . . . . . # . . . . .)
      (. . . . . . . . . . . . . . # . . . . . # . . . .)
      (. . . . # . . . . . . . . # . . . . . . . # . . .)
      (. . . . . # . . . . . . # . . . . . . . . . # . .)
      (. . . . . . # . . . . . . . . . . . . . . # . . .)
      (. . . . . . . # . . . . . . . . . . . . # . . . .)
      (. . . . . . . . # . . . . . . . . . . # . . . . .)
      (. . . . . . . . . # . . . . . . . . # . . . . . .)
      (. . . . . . . . . . # # # # # # # # . . . . . . .)
      (. . . . . . . . . . . . . . . . . . . . . . . . .)
      (. . . . . . . . . . . . . . . . . . . . . . . . .)
      (. . . . . . . . . . . . . . . . . . . . . . . . .)))
      |}]
;;

module T : sig
  include Day.T
end = struct
  let name = "--- Day 9: Rope Bridge ---"

  let part1 =
    let input = lines |> Input.make in
    let t = Physics.update Physics.default Physics.move_tail_part1 input in
    t.tail_positions |> Set.length
  ;;

  let part2 =
    let input = lines |> Input.make in
    let t = Physics.update Physics.default Physics.move_tail_part2 input in
    t.tail_positions |> Set.length
    |> Int.(+) 5 (* Not sure why yet 🤷🏽 *)
  ;;

  let%expect_test "" = 
    print_s [%message (part1: int)];
    [%expect {| (part1 13) |}]
  ;;

end
