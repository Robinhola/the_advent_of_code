open! Base
open! Core

let sample_1 =
  {|L68
L30
R48
L5
R60
L55
L1
L99
R14
L82|}
  |> String.split_lines
;;

type t =
  { position : int
  ; count : int
  }
[@@deriving sexp]

let next_state t (mov : String.t) =
  match String.to_list mov with
  | 'R' :: value ->
    let value = value |> String.of_char_list |> Int.of_string in
    let number_of_times_over_100 = Int.((t.position + value) / 100) in
    { position = (t.position + value) % 100; count = t.count + number_of_times_over_100 }
  | 'L' :: value ->
    let value = Int.of_string (String.of_char_list value) in
    let position = if t.position = 0 then 100 else t.position in
    let number_of_times_over_0 =
      (-1 * Int.((position - value) / 100)) + if value >= position then 1 else 0
    in
    { position = (t.position - value) % 100; count = t.count + number_of_times_over_0 }
  | _ -> raise_s [%message "not matching" (mov : String.t)]
;;

let part1 (lines : string list) =
  let t = { position = 50; count = 0 } in
  let _, c =
    List.fold_left ~init:(t, 0) lines ~f:(fun (t, c) line ->
      let t = next_state t line in
      let c = if t.position = 0 then c + 1 else c in
      (*print_s [%sexp (t : t)];*)
      t, c)
  in
  c
;;

let part2 (lines : string list) =
  let init = { position = 50; count = 0 } in
  let t = List.fold_left ~init ~f:next_state lines in
  t.count
;;

let%expect_test _ =
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    ("part1 sample_1" 3)
    ("part2 sample_1" 6)
    |}]
;;
