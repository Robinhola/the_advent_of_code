open! Base
open! Core

let sample_1 =
  {|Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0|}
  |> String.split_lines
;;

type parsed = int list * int list [@@deriving sexp]

type instruction =
  | Adv of int
  | Bxl of int
  | Bst of int
  | Jnz of int
  | Bxc of int
  | Out of int
  | Bdv of int
  | Cdv of int
[@@deriving sexp]

let to_instruction = function
  | 0, v -> Adv v
  | 1, v -> Bxl v
  | 2, v -> Bst v
  | 3, v -> Jnz v
  | 4, v -> Bxc v
  | 5, v -> Out v
  | 6, v -> Bdv v
  | 7, v -> Cdv v
  | _ -> assert false
;;

type state =
  { a : int
  ; b : int
  ; c : int
  ; o : int list
  ; instructions : int array
  ; head : int
  }
[@@deriving sexp]

let read_operand state operand =
  match operand with
  | 0 | 1 | 2 | 3 -> operand
  | 4 -> state.a
  | 5 -> state.b
  | 6 -> state.c
  | _ -> assert false
;;

let%expect_test _ =
  [ 5, 5; 5, 8; 5, 0 ]
  |> List.iter ~f:(fun (a, b) ->
    let c = Int.(a lor b) in
    print_s [%message (a : int) (b : int) (c : int)]);
  [%expect
    {|
    ((a 5) (b 5) (c 5))
    ((a 5) (b 8) (c 13))
    ((a 5) (b 0) (c 5))
    |}]
;;

let%expect_test _ =
  [ 5, 5; 5, 8; 5, 0 ]
  |> List.iter ~f:(fun (a, b) ->
    let c = Int.(a lxor b) in
    print_s [%message (a : int) (b : int) (c : int)]);
  [%expect
    {|
    ((a 5) (b 5) (c 0))
    ((a 5) (b 8) (c 13))
    ((a 5) (b 0) (c 5))
    |}]
;;

let advance state = { state with head = state.head + 2 }

let rec execute state =
  let length = Array.length state.instructions in
  if state.head >= length
  then state, List.rev state.o |> List.map ~f:Int.to_string |> String.concat ~sep:","
  else (
    (* print_s [%message (length : int) (state.head : int)]; *)
    let instruction =
      let opcode = state.instructions.(state.head) in
      let operand = state.instructions.(state.head + 1) in
      to_instruction (opcode, operand)
    in
    (* print_s [%message (instruction : instruction) (state : state)]; *)
    let new_state =
      match instruction with
      | Adv x ->
        let combo = read_operand state x in
        let numerator = state.a in
        let denominator = Int.(2 ** combo) in
        { state with a = numerator / denominator }
      | Bxl x -> { state with b = Int.(bit_xor state.b x) }
      | Bst x ->
        let combo = read_operand state x in
        { state with b = combo % 8 }
      | Jnz _ when state.a = 0 -> state
      | Jnz x -> { state with head = x - 2 }
      | Bxc _ -> { state with b = Int.(bit_xor state.b state.c) }
      | Out x ->
        let combo = read_operand state x in
        { state with o = (combo % 8) :: state.o }
      | Bdv x ->
        let combo = read_operand state x in
        let numerator = state.a in
        let denominator = Int.(2 ** combo) in
        { state with b = numerator / denominator }
      | Cdv x ->
        let combo = read_operand state x in
        let numerator = state.a in
        let denominator = Int.(2 ** combo) in
        { state with c = numerator / denominator }
    in
    execute (advance new_state))
;;

let%expect_test _ =
  [ { a = 0; b = 0; c = 9; o = []; instructions = [| 2; 6 |]; head = 0 }
  ; { a = 10; b = 0; c = 0; o = []; instructions = [| 5; 0; 5; 1; 5; 4 |]; head = 0 }
  ; { a = 2024; b = 0; c = 0; o = []; instructions = [| 0; 1; 5; 4; 3; 0 |]; head = 0 }
  ; { a = 0; b = 29; c = 0; o = []; instructions = [| 1; 7 |]; head = 0 }
  ; { a = 0; b = 2024; c = 43690; o = []; instructions = [| 4; 0 |]; head = 0 }
  ]
  |> List.iter ~f:(fun state ->
    let state, out = execute state in
    print_s [%message (state : state) (out : string)]);
  [%expect
    {|
    ((state ((a 0) (b 1) (c 9) (o ()) (instructions (2 6)) (head 2))) (out ""))
    ((state
      ((a 10) (b 0) (c 0) (o (2 1 0)) (instructions (5 0 5 1 5 4)) (head 6)))
     (out 0,1,2))
    ((state
      ((a 0) (b 0) (c 0) (o (0 1 3 7 7 7 7 6 5 2 4)) (instructions (0 1 5 4 3 0))
       (head 6)))
     (out 4,2,5,6,7,7,7,7,3,1,0))
    ((state ((a 0) (b 26) (c 0) (o ()) (instructions (1 7)) (head 2))) (out ""))
    ((state ((a 0) (b 2024) (c 44354) (o ()) (instructions (4 0)) (head 2)))
     (out ""))
    |}]
;;

let to_state (parsed : parsed) =
  let regs, instructions = parsed in
  let a, b, c =
    match regs with
    | [ a; b; c ] -> a, b, c
    | _ -> assert false
  in
  let instructions = List.rev instructions |> Array.of_list in
  { a; b; c; o = []; instructions; head = 0 }
;;

let parse_reg s =
  match String.split s ~on:' ' with
  | [ _; _; v ] -> Int.of_string v
  | _ -> assert false
;;

let parse_regs = List.map ~f:parse_reg

let rec parse_program' current = function
  | [] -> current
  | opcode :: rest -> parse_program' (opcode :: current) rest
;;

let parse_program s =
  match String.split_on_chars s ~on:[ ' '; ',' ] with
  | _ :: rest -> parse_program' [] (List.map rest ~f:Int.of_string)
  | _ -> assert false
;;

let rec parse left = function
  | [ ""; right ] -> to_state (parse_regs (List.rev left), parse_program right)
  | a :: rest -> parse (a :: left) rest
  | _ -> assert false
;;

let part1 (lines : string list) =
  let state = parse [] lines in
  let final_state, out = execute state in
  print_s [%message (final_state : state)];
  print_endline out;
  0
;;

let part2 (lines : string list) =
  let _ = lines in
  0
;;

let%expect_test _ =
  print_s [%message (parse [] sample_1 : state)];
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    ("parse [] sample_1"
     ((a 729) (b 0) (c 0) (o ()) (instructions (0 1 5 4 3 0)) (head 0)))
    (final_state
     ((a 0) (b 0) (c 0) (o (0 1 2 5 3 6 5 3 6 4)) (instructions (0 1 5 4 3 0))
      (head 6)))
    4,6,3,5,6,3,5,2,1,0
    ("part1 sample_1" 0)
    ("part2 sample_1" 0)
    |}]
;;
