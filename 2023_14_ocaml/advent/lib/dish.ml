open! Base
open! Core

type t = char array array [@@deriving sexp]

let parse (lines : string list) : t =
  List.map lines ~f:(fun l -> String.to_list l |> Array.of_list) |> Array.of_list
;;

let count t : int =
  let what_load line_number = Array.length t - line_number in
  Array.mapi t ~f:(fun i line -> what_load i * Array.count ~f:(Char.equal 'O') line)
  |> Array.sum (module Int) ~f:Fn.id
;;

let%expect_test _ =
  let t =
    parse
      ("OOOO.#.O..\n\
        OO..#....#\n\
        OO..O##..O\n\
        O..#.OO...\n\
        ........#.\n\
        ..#....#.#\n\
        ..O..#.O.O\n\
        ..O.......\n\
        #....###..\n\
        #....#...."
       |> String.split_lines)
  in
  print_s [%message (count t : int)];
  [%expect {| ("count t" 136) |}]
;;
