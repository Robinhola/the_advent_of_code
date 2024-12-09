open! Base
open! Core
open! Robin_advent_lib

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
  | Out of string
[@@deriving sexp]

module CharMap = Hashtbl.Make (Char)

type state =
  { registers : int CharMap.t
  ; instructions : instruction Array.t
  ; current : int
  ; expecting : [ `ToStart | `Zero | `One ]
  ; seen : Int.Hash_set.t
  ; count : int
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
      | [ "out"; r ] -> Out r
      | r -> raise_s [%message (r : string list)])
    |> Array.of_list
  in
  { instructions
  ; registers = CharMap.create ()
  ; current = 0
  ; expecting = `ToStart
  ; seen = Int.Hash_set.create ()
  ; count = 0
  }
;;

let next state = { state with current = state.current + 1 }

let check_validity t =
  let key =
    t.registers |> CharMap.sexp_of_t Int.sexp_of_t |> Sexp.to_string |> hash_string
  in
  match Hash_set.strict_add t.seen key, t.expecting with
  | Ok (), _ -> `Keep_going t
  | Error _, `ToStart -> `Invalid t
  | (Error _, `One | Error _, `Zero) when t.count > 100 -> `Valid t
  | Error _, `One | Error _, `Zero -> `Keep_going t
;;

let is_invalid t =
  match check_validity t with
  | `Invalid _ -> true
  | _ -> false
;;

let is_valid t =
  match check_validity t with
  | `Valid _ -> true
  | _ -> false
;;

let get' state c = Hashtbl.find_or_add state.registers c ~default:(Fn.const 0)
let get state s = get' state (Char.of_string s)

let rec read i state =
  let out value =
    match value, state.expecting with
    | 0, `ToStart | 0, `Zero ->
      read (i + 1) (next { state with expecting = `One; count = state.count + 1 })
    | 1, `One -> read (i + 1) (next { state with expecting = `Zero })
    | _x, _expecting ->
      (* print_s [%message (_x : int) (_expecting : [ `Zero | `ToStart | `One ])]; *)
      `Invalid state
  in
  if state.current >= Array.length state.instructions
  then `Invalid state
  else if i % 181 |> Int.equal 0 && is_invalid state
  then `Invalid state
  else if i % 181 |> Int.equal 0 && is_valid state
  then `Valid state
  else (
    match state.instructions.(state.current) with
    | Cpy (n, r) when Int.of_string_opt n |> Option.is_some ->
      Hashtbl.set ~key:r ~data:(Int.of_string n) state.registers;
      read (i + 1) (next state)
    | Cpy (s, d) ->
      Hashtbl.set ~key:d ~data:(get state s) state.registers;
      read (i + 1) (next state)
    | Inc r ->
      Hashtbl.set ~key:r ~data:(get' state r + 1) state.registers;
      read (i + 1) (next state)
    | Dec r ->
      Hashtbl.set ~key:r ~data:(get' state r - 1) state.registers;
      read (i + 1) (next state)
    | Jnz (r, n) when Int.of_string_opt r |> Option.is_some ->
      let value = Int.of_string r in
      if Int.equal 0 value
      then read (i + 1) (next state)
      else read (i + 1) { state with current = state.current + n }
    | Jnz (r, n) ->
      let value = get state r in
      if Int.equal 0 value
      then read (i + 1) (next state)
      else read (i + 1) { state with current = state.current + n }
    | Out r when Int.of_string_opt r |> Option.is_some ->
      (* print_endline r; *)
      out (Int.of_string r)
    | Out r ->
      let value = get state r in
      (* print_endline (Int.to_string value); *)
      out value)
;;

let rec find_loop lines i =
  let state = parse lines in
  Hashtbl.add_exn state.registers ~key:'a' ~data:i;
  match read 1 state with
  | `Keep_going state -> raise_s [%message "Should not happen" (state : state)]
  | `Invalid _ -> find_loop lines (i + 1)
  | `Valid _state ->
    (* print_s [%message "valid" (_state : state)]; *)
    i
;;

let part1 (lines : string list) = find_loop lines 0
let part2 (_ : string list) = -1

let%expect_test _ =
  let state = parse sample_1 in
  print_s [%message (read 1 state : [ `Invalid of state | `Valid of state ])];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    ("read 1 state"
     (Invalid
      ((registers ((a 42)))
       (instructions ((Cpy 41 a) (Inc a) (Inc a) (Dec a) (Jnz a 2) (Dec a)))
       (current 6) (expecting ToStart) (seen ()) (count 0))))
    ("part2 sample_1" -1)
    |}]
;;
