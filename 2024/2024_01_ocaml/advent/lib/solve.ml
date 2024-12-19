open! Base
open! Core

let sample_1 = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3" |> String.split_lines

let rec parse a b = function
  | [] -> a, b
  | line :: rest ->
    let tokens = String.split line ~on:' ' in
    let hd = List.hd_exn tokens |> Int.of_string in
    let tl = List.rev tokens |> List.hd_exn |> Int.of_string in
    let a = hd :: a in
    let b = tl :: b in
    parse a b rest
;;

let sort = List.sort ~compare:Int.compare

let part1 (lines : string list) =
  let a, b = parse [] [] lines in
  let a = sort a in
  let b = sort b in
  List.zip_exn a b |> List.fold ~init:0 ~f:(fun s (a, b) -> s + abs (a - b))
;;

let count l =
  l |> List.map ~f:(fun k -> k, 1) |> Int.Map.of_alist_multi |> Map.map ~f:List.length
;;

let part2 (lines : string list) =
  let a, b = parse [] [] lines in
  let b = count b in
  List.map a ~f:(fun key ->
    let multi = Map.find b key |> Option.value ~default:0 in
    key * multi)
  |> List.sum (module Int) ~f:Fn.id
;;

let%expect_test _ =
  print_s [%message (parse [] [] sample_1 : int list * int list)];
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    ("parse [] [] sample_1" ((3 3 1 2 4 3) (3 9 3 5 3 4)))
    ("part1 sample_1" 11)
    ("part2 sample_1" 31)
    |}]
;;
