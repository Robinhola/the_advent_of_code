open! Base
open! Core
open Parse
open Diff
open Transpose

let rec does_reflect'' ~smudge_required a b =
  match a, b with
  | [], _ | _, [] -> Int.equal smudge_required 0
  | va :: resta, vb :: restb when String.equal va vb ->
    does_reflect'' ~smudge_required resta restb
  | va :: resta, vb :: restb when has_only_one_difference va vb && smudge_required > 0 ->
    does_reflect'' ~smudge_required:(smudge_required - 1) resta restb
  | _ -> false
;;

let%expect_test _ =
  let a = [ "1"; "2" ] in
  let b = [ "1" ] in
  let c = [ "1"; "3" ] in
  print_s [%message (does_reflect'' ~smudge_required:0 a b : bool)];
  print_s [%message (does_reflect'' ~smudge_required:0 a a : bool)];
  print_s [%message (does_reflect'' ~smudge_required:0 a c : bool)];
  [%expect
    {|
    ("does_reflect'' ~smudge_required:0 a b" true)
    ("does_reflect'' ~smudge_required:0 a a" true)
    ("does_reflect'' ~smudge_required:0 a c" false)
    |}]
;;

let does_reflect ~smudge_required l =
  let rec does_reflect' ~smudge_required a b =
    match a, b with
    | _, [] -> None
    | a, b' :: rest ->
      Option.first_some
        (Option.some_if (does_reflect'' ~smudge_required a b) (List.length a))
        (does_reflect' ~smudge_required (b' :: a) rest)
  in
  let head = [ List.hd_exn l ] in
  let tail = List.tl_exn l in
  does_reflect' ~smudge_required head tail
;;

let%expect_test _ =
  let a = [ "1"; "2"; "2"; "1" ] in
  let b = [ "1"; "2"; "3" ] in
  print_s [%message (does_reflect ~smudge_required:0 a : int option)];
  print_s [%message (does_reflect ~smudge_required:0 b : int option)];
  print_s [%message (does_reflect ~smudge_required:1 b : int option)];
  [%expect
    {|
    ("does_reflect ~smudge_required:0 a" (2))
    ("does_reflect ~smudge_required:0 b" ())
    ("does_reflect ~smudge_required:1 b" (1))
    |}]
;;

let part1'' ~smudge_required pattern =
  Option.first_some
    (does_reflect ~smudge_required pattern |> Option.map ~f:(Int.( * ) 100))
    (does_reflect ~smudge_required (turn_rows_to_columns pattern))
  |> Option.value_exn
;;

let sum = List.sum (module Int) ~f:Fn.id
let partx ~smudge_required lines = parse lines |> List.map ~f:(part1'' ~smudge_required)
let part1 = Fn.compose sum (partx ~smudge_required:0)
let part2 = Fn.compose sum (partx ~smudge_required:1)

let%expect_test _ =
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect {|
    ("part1 sample_1" 405)
    ("part2 sample_1" 400)
    |}]
;;
