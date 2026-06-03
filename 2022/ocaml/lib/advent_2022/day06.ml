open! Base
open! Core
open! Import

let line =
  Advent.read_lines "day06" ~for_tests:["mjqjpqmgbljsphdztnvjfqwrcgsmlb"] ()
  |> List.hd_exn
  |> String.to_list
;;

let rec look_for_n_different ~n ?(pos = n) chars =
  match n
    |> List.take chars
    |> Char.Set.of_list
    |> Set.length
    |> phys_equal n
  with
  | true -> pos
  | false -> match chars with
    | _ :: rest -> look_for_n_different ~pos:(pos + 1) ~n rest
    | [] -> failwith "could not find"
;;

let look_for_4_different chars = look_for_n_different ~n:4 chars

let look_for_14_different chars = look_for_n_different ~n:14 chars

let run_test ~f = 
  [ "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
  ; "bvwbjplbgvbhsrlpgdmjqwftvncz"
  ; "nppdvjthqldpwncqszvftbrmjlhg"
  ; "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
  ; "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  ]
  |> List.map ~f:(fun l -> l, String.to_list l)
  |> List.map ~f:(fun (l, lc) -> l, f lc)
  |> List.iter ~f:(fun n -> print_s [%sexp (n : string * int)]);
;;

let%expect_test "" =
  run_test ~f:look_for_4_different;
  [%expect {|
    (mjqjpqmgbljsphdztnvjfqwrcgsmlb 7)
    (bvwbjplbgvbhsrlpgdmjqwftvncz 5)
    (nppdvjthqldpwncqszvftbrmjlhg 6)
    (nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg 10)
    (zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw 11) |}]
;;

let%expect_test "" =
  run_test ~f:look_for_14_different;
  [%expect {|
    (mjqjpqmgbljsphdztnvjfqwrcgsmlb 19)
    (bvwbjplbgvbhsrlpgdmjqwftvncz 23)
    (nppdvjthqldpwncqszvftbrmjlhg 23)
    (nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg 29)
    (zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw 26) |}]
;;

module T : sig
  include Day.T
end = struct
  let name = "--- Day 6: Tuning Trouble ---"
  let part1 = look_for_4_different line
  let part2 = look_for_14_different line
end