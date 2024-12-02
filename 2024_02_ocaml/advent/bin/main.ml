open! Base
open! Core

let lines = In_channel.input_lines In_channel.stdin
let part1 = Advent.Solve.part1 lines
let part2 = Advent.Solve.part2 lines
let () = "Part1: " ^ Int.to_string part1 |> print_endline
let () = "Part2: " ^ Int.to_string part2 |> print_endline
