open! Base
open! Core

let lines = Advent.read_lines "day00" ~for_tests:[]

module T : sig
  include Day.T
end = struct
  let name = "--- Day 0: ---"
  let part1 = 0
  let part2 = 0

  let%expect_test "" = 
    print_s [%message (part1: int) (part2: int)];
    [%expect {| ((part1 0) (part2 0)) |}]
  ;;
end