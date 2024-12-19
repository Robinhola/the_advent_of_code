open! Base
open! Core

let sample_1 =
  "#.##..##.\n\
   ..#.##.#.\n\
   ##......#\n\
   ##......#\n\
   ..#.##.#.\n\
   ..##..##.\n\
   #.#.##.#.\n\n\
   #...##..#\n\
   #....#..#\n\
   ..##..###\n\
   #####.##.\n\
   #####.##.\n\
   ..##..###\n\
   #....#..#"
  |> String.split_lines
;;

let rec parse_all (result : string list list) (current : string list) = function
  | [] -> List.rev current :: result
  | "" :: rest -> parse_all (List.rev current :: result) [] rest
  | line :: rest -> parse_all result (line :: current) rest
;;

let parse s = s |> parse_all [] [] |> List.rev

let%expect_test _ =
  print_s [%message (parse sample_1 : string list list)];
  [%expect
    {|
    ("parse sample_1"
     ((#.##..##. ..#.##.#. ##......# ##......# ..#.##.#. ..##..##. #.#.##.#.)
      (#...##..# #....#..# ..##..### #####.##. #####.##. ..##..### #....#..#)))
    |}]
;;
