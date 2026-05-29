open! Base
open! Core

[@@@warning "-32"]

let should_print_debug = ref false
let debug sexp = if !should_print_debug then print_s sexp
let sum = List.sum (module Int)

let sample_1 =
  {|987654321111111
811111111111119
234234234234278
818181911112111|}
  |> String.split_lines
;;

let maxv l =
  l |> List.max_elt ~compare:(fun (a, _) (b, _) -> Int.compare a b) |> Option.value_exn
;;

let rec best_of l i n number =
  let open Int in
  if n = 0
  then of_string number
  else (
    let n = n - 1 in
    let m, i = maxv List.(slice l i (length l - n)) in
    best_of l (i + 1) n (number ^ to_string m))
;;

let mapi s =
  s |> String.to_list |> List.mapi ~f:(fun i x -> Int.of_string (Char.to_string x), i)
;;

let part1 (lines : string list) =
  List.map lines ~f:mapi |> sum ~f:(fun l -> best_of l 0 2 "")
;;

let part2 (lines : string list) =
  List.map lines ~f:mapi |> sum ~f:(fun l -> best_of l 0 12 "")
;;

let%expect_test _ =
  should_print_debug := true;
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    ("part1 sample_1" 357)
    ("part2 sample_1" 3121910778619)
    |}]
;;
