open! Base
open! Core
open! Import

let lines = Advent.read_lines "day11" ~for_tests:
  [ "Monkey 0:"
  ; "  Starting items: 79, 98"
  ; "  Operation: new = old * 19"
  ; "  Test: divisible by 23"
  ; "    If true: throw to monkey 2"
  ; "    If false: throw to monkey 3"
  ; ""
  ; "Monkey 1:"
  ; "  Starting items: 54, 65, 75, 74"
  ; "  Operation: new = old + 6"
  ; "  Test: divisible by 19"
  ; "    If true: throw to monkey 2"
  ; "    If false: throw to monkey 0"
  ; ""
  ; "Monkey 2:"
  ; "  Starting items: 79, 60, 97"
  ; "  Operation: new = old * old"
  ; "  Test: divisible by 13"
  ; "    If true: throw to monkey 1"
  ; "    If false: throw to monkey 3"
  ; ""
  ; "Monkey 3:"
  ; "  Starting items: 74"
  ; "  Operation: new = old + 3"
  ; "  Test: divisible by 17"
  ; "    If true: throw to monkey 0"
  ; "    If false: throw to monkey 1"
  ] ()
;;

module Monkey = struct
  type t =
    { items : int Queue.t
    ; modulo : int
    ; operation : (int -> int)
    ; test: (int -> int)
    } [@@deriving sexp]
  ;;

  let read_starting_items line = match line |> String.split ~on:':' with
    | _ :: numbers :: [] -> numbers
      |> String.split ~on:','
      |> List.map ~f:String.strip
      |> List.map ~f:Int.of_string
      |> Queue.of_list
    | _ -> failwith "impossible"
  ;;

  let read_operation line = match line |> String.split ~on:'=' with
  | _ :: operation :: [] ->
      let op, sign =
        if String.exists operation ~f:(Char.equal '+') then
          (Int.(+), '+')
        else if String.exists operation ~f:(Char.equal '*') then
          (Int.( * ), '*')
        else failwith "impossible"
      in (
        match operation |> String.split ~on:sign with
        | " old " :: " old" :: [] ->
          (fun x -> op x x)
        | " old " :: n :: [] ->
          let n = n |> String.strip |> Int.of_string in
          (fun x -> op x n)
        | _ -> failwith "wrong format")
  | _ -> failwith "impossible"
  ;;

  let read_modulo test =
    List.nth_exn (test |> String.split ~on:' ') 5 |> Int.of_string
  ;;

  let read_test test ~if_true ~if_false =
    let divisible_by = read_modulo test in
    let throw_to line =
      List.nth_exn (line |> String.split ~on:' ') 9 |> Int.of_string
    in
    let if_true = throw_to if_true in
    let if_false = throw_to if_false in
    (fun x -> match (x % divisible_by) |> Int.equal 0 with
      | true -> if_true
      | false -> if_false
    )
  ;;

  let make ~starting_items ~operation ~test ~if_true ~if_false =
    { items = read_starting_items starting_items
    ; operation = read_operation operation
    ; test = read_test test ~if_true ~if_false
    ; modulo = read_modulo test
    }
  ;;
end

module State = struct
  type t =
    { array : Monkey.t array
    ; active : int array
    } [@@deriving sexp]
  ;;

  let rec read t = function
    | [] -> t |> List.rev |> Array.of_list
    | _ :: starting_items :: operation :: test :: if_true :: if_false :: "" :: rest
    | _ :: starting_items :: operation :: test :: if_true :: if_false :: rest ->
      read (Monkey.make ~starting_items ~operation ~test ~if_true ~if_false :: t) rest
    | _ -> failwith "impossible"
  ;;

  let make input =
    let array = read [] input in
    let active = Array.create ~len:(Array.length array) 0 in
    { array; active }
  ;;

  let throw (t : t) ~x ~to_ =
    let to_ = Array.get t.array to_ in
    Queue.enqueue to_.items x
  ;;

  let rec play_turn ~reduce_worry (t : t) (i : int) (monkey: Monkey.t) =
    match monkey.items |> Queue.dequeue with
    | None -> ()
    | Some x ->
      let x = x
        |> monkey.operation
        |> reduce_worry
      in
      throw t ~x ~to_:(monkey.test x);
      t.active.(i) <- t.active.(i) + 1;
      play_turn ~reduce_worry t i monkey;
  ;;

  let play_turn_part1 = play_turn ~reduce_worry:(fun x -> x / 3)

  let play_turn_part2 modulo = play_turn ~reduce_worry:(fun x -> x % modulo) 

  let play_round f t = Array.iteri ~f:(f t) t.array

  let rec play_n_rounds f t = function
    | 0 -> t
    | n ->
      play_round f t;
      play_n_rounds f t (n - 1)
  ;;
end

let rec multiply_first_two max1 max2 = function
  | [] -> max1 * max2
  | x :: rest when x >= max1 -> multiply_first_two x max1 rest
  | x :: rest when x >= max2 -> multiply_first_two max1 x rest
  | _ :: rest -> multiply_first_two max1 max2 rest
;;

let%expect_test "" =
  let t = State.make lines in
  let t = State.play_n_rounds State.play_turn_part1 t 20 in
  let result = multiply_first_two 0 0 (t.active |> Array.to_list) in
  print_s [%message (t.active: int array)];
  print_s [%message (result: int)];
  [%expect {|
    (t.active (101 95 7 105))
    (result 10605) |}]
;;

let super_modulo (t : State.t) = 
  t.array 
  |> Array.to_list
  |> List.map ~f:(fun m -> m.modulo)
  |> List.fold ~init:1 ~f:Int.( * )
;;

let part2_ () =
  let t = State.make lines in
  let super_modulo = super_modulo t in
  State.play_n_rounds (State.play_turn_part2 super_modulo) t 10_000
;;

let%expect_test "" =
  let t = State.make lines in
  let super_modulo = super_modulo t in
  let t = State.play_n_rounds (State.play_turn_part2 super_modulo) t 20 in 
  print_s [%message (t.active: int array)];
  [%expect {|
    (t.active (99 97 8 103)) |}]
;;

module T : sig
  include Day.T
end = struct
  let name = "--- Day 11: Monkey in the Middle ---"

  let part1 =
    let t = State.make lines in
    let t = State.play_n_rounds State.play_turn_part1 t 20 in
    let result = multiply_first_two 0 0 (t.active |> Array.to_list) in
    result
  ;;

  let part2 =
    let t = part2_ () in
    let result = multiply_first_two 0 0 (t.active |> Array.to_list) in
    result
  ;;

  let%expect_test "" =
    print_s [%message (part1 : int) (part2: int)];
    [%expect {| ((part1 10605) (part2 2713310158)) |}]
  ;;
end