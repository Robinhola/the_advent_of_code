open! Base
open! Core
open! Import

let lines = Advent.read_lines "day12" ~for_tests:
  [ "Sabqponm"
  ; "abcryxxl"
  ; "accszExk"
  ; "acctuvwj"
  ; "abdefghi"
  ] ()
;;

module IntPairs = struct
  type t = int * int [@@deriving sexp, compare]

  let equal left right = 
    let xl, yl = left in
    let xr, yr = right in
    Int.equal xl xr && Int.equal yl yr
  ;;
    
end

module PairsSet = Set.Make(IntPairs)

module Input = struct
  type t =
  { grid : int array array
  ; start : int * int
  ; end_ : int * int
  ; steps : int 
  ; current_minimum : int option ref
  } [@@deriving sexp_of]

  let lower_a = Char.to_int 'a'
  let from_a_to_n = function
    | 'S' -> 0
    | 'E' -> 25
    | c when Char.is_lowercase c -> (Char.to_int c) - lower_a
    | _ -> failwith "wrong format"
  ;;

  (* Move me to util  *)
  let empty_size_of lines =
    let x = List.hd_exn lines |> String.length in
    let y = List.length lines in
    Array.make_matrix ~dimx:y ~dimy:x 0
  ;;

  let make lines =
    let start = ref (0, 0) in
    let end_ = ref (0, 0) in
    let m = empty_size_of lines in
    lines
    |> List.iteri ~f:(fun y line ->
      line
      |> String.to_list
      |> List.iteri ~f:(
        fun x c ->
          m.(y).(x) <- from_a_to_n c;
          if Char.equal c 'S' then start := x, y;
          if Char.equal c 'E' then end_ := x, y;
      )
    );
    { grid = m
    ; start = !start
    ; end_ = !end_
    ; steps = 0
    ; current_minimum = ref None
    }
  ;;

  let%expect_test "" =
    print_s [%message (from_a_to_n 'a' : int) (from_a_to_n 'S' : int)];
    print_s [%message (from_a_to_n 'z' : int) (from_a_to_n 'E' : int)];
    print_s [%message (from_a_to_n 'c' : int) (from_a_to_n 'e' : int)];
    print_s [%message (make lines: t)];
    [%expect {|
      (("from_a_to_n 'a'" 0) ("from_a_to_n 'S'" 0))
      (("from_a_to_n 'z'" 25) ("from_a_to_n 'E'" 25))
      (("from_a_to_n 'c'" 2) ("from_a_to_n 'e'" 4))
      ("make lines"
       ((grid
         ((0 0 1 16 15 14 13 12) (0 1 2 17 24 23 23 11) (0 2 2 18 25 25 23 10)
          (0 2 2 19 20 21 22 9) (0 1 3 4 5 6 7 8)))
        (start (0 0)) (end_ (5 2)) (steps 0) (current_minimum ()))) |}]
  ;;


  let find_bounds grid =
    let max_y = Array.length grid in
    let max_x = Array.length grid.(0) in
    max_x, max_y
  ;;

  let part1_f t (x, y) (xn, yn) =
    t.grid.(yn).(xn) - t.grid.(y).(x) <= 1
  ;;

  let part2_f t (x, y) (xn, yn) =
    t.grid.(y).(x) - t.grid.(yn).(xn) <= 1
  ;;

  let get_unseen_next f t start seen = 
    let max_x, max_y = find_bounds t.grid in
    let x, y = start in
    [ x - 1, y
    ; x + 1, y
    ; x, y - 1
    ; x, y + 1 ]
    |> List.filter ~f:(fun (x, y) ->
      0 <= x
      && x < max_x
      && 0 <= y 
      && y < max_y)
    |> List.filter ~f:(f t (x, y))
    |> List.filter ~f:(fun p -> not (Set.exists seen ~f:(IntPairs.equal p)))
  ;;

  let bfs ~f ~is_done t =
    let rec search t to_visit seen =
      let p, steps = Queue.dequeue_exn to_visit in
      if is_done p then steps
      else
        let unseen = get_unseen_next f t p seen in
        let seen = Set.union seen (PairsSet.of_list unseen) in
        unseen
        |> List.map ~f:(fun new_p -> new_p, steps + 1)
        |> List.iter ~f:(Queue.enqueue to_visit);
        search t to_visit seen
    in
    search t (Queue.of_list [t.start, 0]) (PairsSet.of_list [t.start])
  ;;
end

module T : sig
  include Day.T
end = struct
  let name = "--- Day 12: ---"

  let input = Input.make lines 

  let part1 = Input.bfs
    input
    ~f:Input.part1_f
    ~is_done:(IntPairs.equal input.end_)
  ;;

  let part2 = 
    Input.bfs
      { input with start = input.end_ ; end_ = input.start }
      ~f:Input.part2_f
      ~is_done:(fun (x, y) -> input.grid.(y).(x) |> Int.equal 0)
  ;;

  let%expect_test "" =
    print_s [%message (part1 : int) (part2 : int)];
    [%expect {| ((part1 31) (part2 29)) |}]
  ;;
end