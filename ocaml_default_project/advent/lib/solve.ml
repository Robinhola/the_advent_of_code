open! Base
open! Core

[@@@warning "-32"]

let should_print_debug = ref false
let debug sexp = if !should_print_debug then print_s sexp
let sample_1 = {||} |> String.split_lines

let part1 (lines : string list) =
  List.iteri lines ~f:(fun i line -> print_s [%message (i : int) (line : string)]);
  0
;;

let part2 (lines : string list) =
  let _ = lines in
  0
;;

let%expect_test _ =
  should_print_debug := true;
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    ("part1 sample_1" 0)
    ("part2 sample_1" 0)
    |}]
;;
