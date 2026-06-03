open! Base
open! Core
open! Import

let lines = 
  Advent.read_lines "day05" ~for_tests:
  [ "    [D]    "
  ; "[N] [C]    "
  ; "[Z] [M] [P]"
  ; " 1   2   3 "
  ; ""
  ; "move 1 from 2 to 1"
  ; "move 3 from 1 to 3"
  ; "move 2 from 2 to 1"
  ; "move 1 from 1 to 2"
  ] ()
;;

module State = struct
  type t = char Stack.t Array.t [@@deriving sexp_of]

  let rec split_line crates = function
    | [] -> crates |> List.rev
    | _ :: c :: _ :: [] -> split_line (c :: crates) []
    | _ :: c :: _ :: _ :: rest -> split_line (c :: crates) rest
    | _ -> failwith "invalid format"
  ;;

  let%expect_test "" =
    let result =
      [ "    [D]    "
      ; "[N] [C]    "
      ; "[Z] [M] [P]"
      ]
      |> List.map ~f:String.to_list
      |> List.map ~f:(split_line [])
    in
    print_s [%message (result: char list list)];
    [%expect {| (result ((" " D " ") (N C " ") (Z M P))) |}]
  ;;

  let update t line =
    split_line [] line
    |> List.iteri ~f:(fun i -> function
      | ' ' -> ()
      | c ->
        let stack = Array.get t i in
        Stack.push stack c)
  ;;

  let get t offset_i =
    let i = offset_i - 1 in
    Array.get t i
  ;;

  let top_of_each t =
    t
    |> Array.map ~f:Stack.top_exn
    |> Array.to_list
    |> String.of_char_list
  ;;
end

module Instruction = struct
  type t =
  { how_many : int
  ; from : int
  ; to_ : int
  } [@@deriving sexp_of]

  let from_line line =
    match String.split line ~on:' ' with
    | "move" :: how_many :: "from" :: from :: "to" :: to_ :: [] ->
      let how_many = how_many |> Int.of_string in
      let from = from |> Int.of_string in
      let to_ = to_ |> Int.of_string in
      {how_many; from; to_}
    | _ -> failwith "invalid line"
  ;;

  let execute state t =
    let from = State.get state t.from in
    let to_ = State.get state t.to_ in
    let n () = t.how_many in
    for _i = 1 to n () do
      Stack.pop_exn from
      |> Stack.push to_
    done
  ;;

  let execute_9001 state t =
    let from = State.get state t.from in
    let to_ = State.get state t.to_ in
    let buffer = Stack.create () in
    let n () = t.how_many in
    for _i = 1 to n () do
      Stack.pop_exn from
      |> Stack.push buffer
    done;
    for _i = 1 to n () do
      Stack.pop_exn buffer
      |> Stack.push to_
    done
  ;;
end

module Input = struct
  type t =
    { starting_state : string list
    ; instructions : string list
    } [@@deriving sexp_of]

  let segregate_input lines =
    let rec starting_state t = function
    | "" :: rest -> t, rest
    | [] -> failwith "lines should not be empty"
    | l :: rest ->
      let t = { t with starting_state = l :: t.starting_state } in
      starting_state t rest
    in
    let rec instructions t = function
    | [] -> t, []
    | "" :: _ -> failwith "line should not be empty"
    | l :: rest ->
      let t = { t with instructions = l :: t.instructions } in
      instructions t rest
    in
    let t = {starting_state=[]; instructions=[]} in
    let t, rest = starting_state t lines in
    let t, _ = instructions t rest in
    t
  ;;

  let%expect_test "" =
    let t = segregate_input lines in
    print_s [%message (t: t)];
    [%expect {|
      (t
       ((starting_state (" 1   2   3 " "[Z] [M] [P]" "[N] [C]    " "    [D]    "))
        (instructions
         ("move 1 from 1 to 2" "move 2 from 2 to 1" "move 3 from 1 to 3"
          "move 1 from 2 to 1")))) |}]
  ;;

  let starting_state t =
    let how_many, rest = match t.starting_state with
      | [] -> failwith "cannot be empty"
      | head :: rest -> head
        |> String.split ~on:' '
        |> List.filter ~f:(fun s -> not (String.is_empty s))
        |> List.length
        , rest
    in
    let state = Array.init how_many ~f:(fun _ -> Stack.create ()) in
    rest
    |> List.map ~f:String.to_list
    |> List.iter ~f:(State.update state);
    state
  ;;

  let%expect_test "" =
    let t = segregate_input lines in
    let state = starting_state t in
    print_s [%message (state : State.t)];
    [%expect {| (state ((N Z) (D C M) (P))) |}]
  ;;

  let instructions t = t.instructions
    |> List.map ~f:Instruction.from_line
    |> List.rev
  ;;

  let%expect_test "" =
    let t = segregate_input lines in
    let instructions = instructions t in
    print_s [%sexp (instructions : Instruction.t list)];
    [%expect {|
      (((how_many 1) (from 2) (to_ 1)) ((how_many 3) (from 1) (to_ 3))
       ((how_many 2) (from 2) (to_ 1)) ((how_many 1) (from 1) (to_ 2))) |}]
  ;;

  let make_final_state execute lines =
    let t = segregate_input lines in
    let state = starting_state t in
    instructions t
    |> List.iter ~f:(execute state);
    state
  ;;

  let%expect_test "" =
    let state = lines |> make_final_state Instruction.execute in
    let result = State.top_of_each state in
    print_s [%message (state : State.t) (result : string)];
    [%expect {|
      ((state ((C) (M) (Z N D P))) (result CMZ)) |}]
  ;;

  let%expect_test "" =
    let state = lines |> make_final_state Instruction.execute_9001 in
    let result = State.top_of_each state in
    print_s [%message (state : State.t) (result : string)];
    [%expect {|
      ((state ((M) (C) (D N Z P))) (result MCD)) |}]
  ;;
end


module T : sig
  include Day.T_string
end = struct
  let name = "--- Day 5: Supply Stacks ---"
  let part1 = lines
    |> Input.make_final_state Instruction.execute
    |> State.top_of_each
  ;;
  let part2 = lines
    |> Input.make_final_state Instruction.execute_9001
    |> State.top_of_each
  ;;
end