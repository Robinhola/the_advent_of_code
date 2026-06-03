open! Base
open! Core
open! Import

module Segment = struct
  type t =
    { start: int
    ; end_ : int
    }
  ;;

  let make = function
  | left :: right :: [] ->
    { start = Int.of_string left
    ; end_ = Int.of_string right
    }
  | _ -> failwith "unsupported line"

  let make_from_input input = input
    |> String.split ~on:'-'
    |> make
  ;;

  let does_fully_contain bigger smaller =
    bigger.start <= smaller.start && bigger.end_ >= smaller.end_
  ;;

  let does_partially_contain bigger smaller =
    let test x = bigger.start <= x && bigger.end_ >= x in
    test smaller.start || test smaller.end_
  ;;

end

let segments =
  Advent.read_lines "day04" ~for_tests:
  [ "2-4,6-8"
  ; "2-3,4-5"
  ; "5-7,7-9"
  ; "2-8,3-7"
  ; "6-6,4-6"
  ; "2-6,4-8"
  ] ()
  |> List.map ~f:(fun line -> line
    |> String.split ~on:','
    |> List.map ~f:Segment.make_from_input
  )
;;

let at_least_one f = function
  | left :: right :: [] -> if
    (f left right) ||
    (f right left) then
    1 else
    0
  | _ -> failwith "invalid pair"
;;

let how_many_are = List.sum (module Int)

let fully_overlapping = at_least_one Segment.does_fully_contain

let partially_overlapping = at_least_one Segment.does_partially_contain

module T : sig
  include Day.T
end = struct
  let name = "--- Day 4: Camp Cleanup ---"
  let part1 = segments |> how_many_are ~f:fully_overlapping
  let part2 = segments |> how_many_are ~f:partially_overlapping

  let%expect_test "" = 
    print_s [%message (part1: int) (part2: int)];
    [%expect {| ((part1 2) (part2 4)) |}]
  ;;
end