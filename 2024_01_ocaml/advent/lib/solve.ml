open! Base
open! Core

let sample_1 = ""

let part1 (lines : string list) =
  List.iteri lines ~f:(fun i line -> print_s [%message (i : int) (line : string)]);
  0
;;

let part2 (_lines : string list) = 0

let%expect_test _ =
  let result = String.split_lines sample_1 |> part1 in
  print_s [%message (result : int)];
  [%expect {| (result 0) |}]
;;
