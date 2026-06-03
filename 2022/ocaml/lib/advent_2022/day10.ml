open! Base
open! Core
open! Import

let lines = Advent.read_lines "day10" ~for_tests:
[ "addx 15"
; "addx -11"
; "addx 6"
; "addx -3"
; "addx 5"
; "addx -1"
; "addx -8"
; "addx 13"
; "addx 4"
; "noop"
; "addx -1"
; "addx 5"
; "addx -1"
; "addx 5"
; "addx -1"
; "addx 5"
; "addx -1"
; "addx 5"
; "addx -1"
; "addx -35"
; "addx 1"
; "addx 24"
; "addx -19"
; "addx 1"
; "addx 16"
; "addx -11"
; "noop"
; "noop"
; "addx 21"
; "addx -15"
; "noop"
; "noop"
; "addx -3"
; "addx 9"
; "addx 1"
; "addx -3"
; "addx 8"
; "addx 1"
; "addx 5"
; "noop"
; "noop"
; "noop"
; "noop"
; "noop"
; "addx -36"
; "noop"
; "addx 1"
; "addx 7"
; "noop"
; "noop"
; "noop"
; "addx 2"
; "addx 6"
; "noop"
; "noop"
; "noop"
; "noop"
; "noop"
; "addx 1"
; "noop"
; "noop"
; "addx 7"
; "addx 1"
; "noop"
; "addx -13"
; "addx 13"
; "addx 7"
; "noop"
; "addx 1"
; "addx -33"
; "noop"
; "noop"
; "noop"
; "addx 2"
; "noop"
; "noop"
; "noop"
; "addx 8"
; "noop"
; "addx -1"
; "addx 2"
; "addx 1"
; "noop"
; "addx 17"
; "addx -9"
; "addx 1"
; "addx 1"
; "addx -3"
; "addx 11"
; "noop"
; "noop"
; "addx 1"
; "noop"
; "addx 1"
; "noop"
; "noop"
; "addx -13"
; "addx -19"
; "addx 1"
; "addx 3"
; "addx 26"
; "addx -30"
; "addx 12"
; "addx -1"
; "addx 3"
; "addx 1"
; "noop"
; "noop"
; "noop"
; "addx -9"
; "addx 18"
; "addx 1"
; "addx 2"
; "noop"
; "noop"
; "addx 9"
; "noop"
; "noop"
; "noop"
; "addx -1"
; "addx 2"
; "addx -37"
; "addx 1"
; "addx 3"
; "noop"
; "addx 15"
; "addx -21"
; "addx 22"
; "addx -6"
; "addx 1"
; "noop"
; "addx 2"
; "addx 1"
; "noop"
; "addx -10"
; "noop"
; "noop"
; "addx 20"
; "addx 1"
; "addx 2"
; "addx 2"
; "addx -6"
; "addx -11"
; "noop"
; "noop"
; "noop"
] ()
;;

module Input = struct
  type t =
    | Addx of int
    | Noop
  [@@deriving sexp]

  let read line = match line |> String.split ~on:' ' with
    | "noop" :: [] -> Noop
    | "addx" :: v :: [] -> Addx (v |> Int.of_string)
    | _ -> failwith "invalid"
  ;;

  let make = List.map ~f:read
end

let input = lines |> Input.make 

module State = struct
  type t = 
    { current : int
    ; x : int
    ; signal_strength : int
    ; screen : char array array
    } [@@deriving sexp]
  ;;

  let default =
    { current = 0
    ; x = 1
    ; signal_strength = 0
    ; screen = Array.make_matrix ~dimx:6 ~dimy:40 '.' }
  ;;

  let report_if_needed t =
    let signal_strength = 
      match (t.current - 20) % 40  with
      | 0 -> 
        t.signal_strength + t.current * t.x
      | _ -> 
        t.signal_strength
    in
    let sprite_y = (t.current - 1) % 40 in
    let sprite_x = (t.current - 1) /% 40 in
    let should_draw = abs (sprite_y - t.x) <= 1 in
    (* print_s [%message (t.x : int) (sprite_y : int) (should_draw: bool)]; *)
    if should_draw then t.screen.(sprite_x).(sprite_y) <- '#';
    { t with signal_strength }
  ;;

  let advance t = { t with current = t.current + 1 }

  let add v t = { t with x = t.x + v } 

  let execute t input = 
    match input with
  | Input.Noop -> 
    t
    |> advance
    |> report_if_needed
  | Input.Addx v ->
    t
    |> advance
    |> report_if_needed
    |> advance
    |> report_if_needed
    |> add v
  ;;

  let rec update t = function 
    | [] -> t
    | input :: rest -> update (input |> execute t) rest
end

let display screen = 
  Array.iter screen ~f:(fun line -> 
    let line = line |> Array.to_list |> String.of_char_list in
    print_s [%sexp (line : string)]
  )
;;



let%expect_test "" =
  let t = input |> State.update State.default in
  print_s [%message (t.signal_strength: int)];
  display t.screen;
  [%expect {|
    (t.signal_strength 13140)
    ##..##..##..##..##..##..##..##..##..##..
    ###...###...###...###...###...###...###.
    ####....####....####....####....####....
    #####.....#####.....#####.....#####.....
    ######......######......######......####
    #######.......#######.......#######..... |}]
;;

module T : sig
  include Day.T_any
end = struct
  let name = "--- Day 10: Cathode-Ray Tube ---"

  let t = input |> State.update State.default

  let part1 = Day.Int (t.signal_strength)

  let part2 = 
    (* You may uncomment the following line to display the result *)
    (* display t.screen; *)
    Day.String ("FZBPBFZF")
  ;;

end