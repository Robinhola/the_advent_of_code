open! Base
open! Core
open! Import

let lines = []
  (* try List.map (Stdio.In_channel.read_lines "input/day01.in") ~f:int_of_string with
  _ -> []
;; *)

module T : sig
  include Day.T
end = struct
  let name = "--- Day 21: ---"
  let part1 = 0
  let part2 = 0
end