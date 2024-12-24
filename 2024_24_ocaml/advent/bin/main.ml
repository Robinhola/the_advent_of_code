open! Base
open! Core

let took started ended =
  let duration = Time_ns.(diff ended started) |> Time_ns.Span.to_ms in
  "took " ^ Float.to_string duration ^ "ms"
;;

let lines = In_channel.input_lines In_channel.stdin
let a = Time_ns.now ()
let () = "Part1: " ^ Int.to_string (Advent.Solve.part1 lines) |> print_endline
let b = Time_ns.now ()
let () = "Part2: " ^ Int.to_string (Advent.Solve.part2 lines) |> print_endline
let c = Time_ns.now ()
let () = "Part1 " ^ took a b |> print_endline
let () = "Part2 " ^ took b c |> print_endline
