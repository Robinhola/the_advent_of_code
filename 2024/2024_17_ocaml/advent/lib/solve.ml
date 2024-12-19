open! Base
open! Core

let sample_1 =
  {|Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0|}
  |> String.split_lines
;;

let sample_2 =
  {|Register A: 729
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0|}
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

let advance state = { state with head = state.head + 2 }

let rec execute state mode =
  let length = Array.length state.instructions in
  if state.head >= length
  then (
    let output =
      List.rev state.o |> List.map ~f:Int.to_string |> String.concat ~sep:","
    in
    state, output)
  else (
    let instruction =
      let opcode = state.instructions.(state.head) in
      let operand = state.instructions.(state.head + 1) in
      to_instruction (opcode, operand)
    in
    let state =
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
    match mode, instruction with
    | `Strict, Out _ ->
      let i = List.length state.o - 1 in
      let last_out = List.hd_exn state.o in
      let corresponding = state.instructions.(i) in
      if last_out = corresponding
      then execute (advance state) mode
      else (
        let output =
          List.rev state.o |> List.map ~f:Int.to_string |> String.concat ~sep:","
        in
        state, output)
    | _, _ -> execute (advance state) mode)
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
  let final_state, out = execute state `Normal in
  print_s [%message (final_state : state)];
  print_endline out;
  0
;;

let part2 (lines : string list) =
  let state = parse [] lines in
  (* Use this to find values corresponding to your input *)
  [ "0"; "1"; "2"; "3"; "4"; "5,1,5,0,3"; "4,5,5,3,0"; "1,7" ]
  |> List.iter ~f:(fun pattern ->
    let rec look a =
      let _, o = execute { state with a } `Normal in
      if String.substr_index o ~pattern |> Option.is_some
      then print_s [%message "Found" (pattern : string) (o : string) (a : int)]
      else look (a + 1)
    in
    look 1);
  let open Int in
  (* Add and shift these values *)
  let instructions =
    state.instructions
    |> Array.to_list
    |> List.map ~f:Int.to_string
    |> String.concat ~sep:","
  in
  let a =
    [ 0
      (* did not manage to get closer *)
      (* ; 332 *)
    ; 27 * (8 ** 3)
    ; 61860 * (8 ** 5)
    ; 19152 * (8 ** 11)
    ]
    |> List.sum (module Int) ~f:Fn.id
  in
  let _, o = execute { state with a } `Normal in
  print_endline "--------------- HEHE ----------------";
  print_endline instructions;
  print_endline o;
  let rec find a =
    match execute { state with a } `Strict with
    | _, o when String.equal o instructions ->
      print_s [%message "Got a match!" (o : string) (a : int)];
      a
    | _ -> find (a + 1)
  in
  find a
;;

let%expect_test _ =
  let state = parse [] sample_2 in
  print_s [%message (execute { state with a = 117440 } `Normal : state * string)];
  print_s [%message (parse [] sample_1 : state)];
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (execute { state with a = 3 } `Normal : state * string)];
  print_s [%message (execute { state with a = (3 * 8) + 4 } `Normal : state * string)];
  print_s
    [%message
      (execute { state with a = (3 * 8 * 8) + (4 * 8) + 5 } `Normal : state * string)];
  [%expect
    {|
    ("execute { state with a = 117440 } `Normal"
     (((a 0) (b 0) (c 0) (o (0 3 4 5 3 0)) (instructions (0 3 5 4 3 0)) (head 6))
      0,3,5,4,3,0))
    ("parse [] sample_1"
     ((a 729) (b 0) (c 0) (o ()) (instructions (0 1 5 4 3 0)) (head 0)))
    (final_state
     ((a 0) (b 0) (c 0) (o (0 1 2 5 3 6 5 3 6 4)) (instructions (0 1 5 4 3 0))
      (head 6)))
    4,6,3,5,6,3,5,2,1,0
    ("part1 sample_1" 0)
    ("execute { state with a = 3 } `Normal"
     (((a 0) (b 0) (c 0) (o (0)) (instructions (0 3 5 4 3 0)) (head 6)) 0))
    ("execute { state with a = ((3 * 8) + 4) } `Normal"
     (((a 0) (b 0) (c 0) (o (0 3)) (instructions (0 3 5 4 3 0)) (head 6)) 3,0))
    ("execute { state with a = ((((3 * 8) * 8) + (4 * 8)) + 5) } `Normal"
     (((a 0) (b 0) (c 0) (o (0 3 4)) (instructions (0 3 5 4 3 0)) (head 6))
      4,3,0))
    |}]
;;
