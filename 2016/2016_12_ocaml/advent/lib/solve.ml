open! Base
open! Core

let sample_1 =
  {|cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a|}
  |> String.split_lines
;;

type instruction =
  | Cpy of string * char
  | Inc of char
  | Dec of char
  | Jnz of string * int
[@@deriving sexp]

module CharMap = Hashtbl.Make (Char)

type state =
  { registers : int CharMap.t
  ; instructions : instruction Array.t
  ; current : int
  }
[@@deriving sexp]

let parse l =
  let instructions =
    List.map l ~f:(fun line ->
      match String.split ~on:' ' line with
      | [ "cpy"; n; r ] -> Cpy (n, Char.of_string r)
      | [ "inc"; r ] -> Inc (Char.of_string r)
      | [ "dec"; r ] -> Dec (Char.of_string r)
      | [ "jnz"; r; n ] -> Jnz (r, Int.of_string n)
      | r -> raise_s [%message (r : string list)])
    |> Array.of_list
  in
  { instructions; registers = CharMap.create (); current = 0 }
;;

let next state = { state with current = state.current + 1 }

let rec read state =
  if state.current >= Array.length state.instructions
  then state
  else (
    match state.instructions.(state.current) with
    | Cpy (n, r) when Int.of_string_opt n |> Option.is_some ->
      Hashtbl.set ~key:r ~data:(Int.of_string n) state.registers;
      read (next state)
    | Cpy (s, d) ->
      Hashtbl.set
        ~key:d
        ~data:
          (Hashtbl.find_or_add state.registers (Char.of_string s) ~default:(Fn.const 0))
        state.registers;
      read (next state)
    | Inc r ->
      Hashtbl.set
        ~key:r
        ~data:(Hashtbl.find_or_add state.registers r ~default:(Fn.const 0) + 1)
        state.registers;
      read (next state)
    | Dec r ->
      Hashtbl.set
        ~key:r
        ~data:(Hashtbl.find_or_add state.registers r ~default:(Fn.const 0) - 1)
        state.registers;
      read (next state)
    | Jnz (r, n) when Int.of_string_opt r |> Option.is_some ->
      if Int.equal 0 (Int.of_string r)
      then read (next state)
      else read { state with current = state.current + n }
    | Jnz (r, n) ->
      if
        Hashtbl.find_or_add state.registers (Char.of_string r) ~default:(Fn.const 0)
        |> Int.equal 0
      then read (next state)
      else read { state with current = state.current + n })
;;

let part1 (lines : string list) =
  let state = parse lines in
  let state = read state in
  Hashtbl.find_exn state.registers 'a'
;;

let part2 (lines : string list) =
  let state = parse lines in
  Hashtbl.set state.registers ~key:'c' ~data:1;
  let state = read state in
  Hashtbl.find_exn state.registers 'a'
;;

let%expect_test _ =
  let state = parse sample_1 in
  print_s [%message (read state : state)];
  print_s [%message (part1 sample_1 : int)];
  [%expect
    {|
    ("read state"
     ((registers ((a 42)))
      (instructions ((Cpy 41 a) (Inc a) (Inc a) (Dec a) (Jnz a 2) (Dec a)))
      (current 6)))
    ("part1 sample_1" 42)
    |}]
;;
