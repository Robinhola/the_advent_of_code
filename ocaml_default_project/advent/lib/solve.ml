open! Base
open! Core

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
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)]
;;
