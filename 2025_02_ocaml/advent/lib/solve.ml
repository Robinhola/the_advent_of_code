open! Base
open! Core

let sample_1 =
  {|11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124|}
  |> String.split_lines
;;

let should_print_debug = ref false
let debug sexp = if !should_print_debug then print_s sexp
let ranges l = String.split ~on:',' l

let range s =
  match String.split ~on:'-' s with
  | [ a; b ] -> Int.of_string a, Int.of_string b
  | _ -> raise_s [%message "hallo" (s : string)]
;;

let rec repeat s n = if n <= 0 then "" else s ^ repeat s (n - 1)

let brute_test_1 s =
  let n = String.length s in
  if n % 2 = 1
  then false
  else (
    let prefix = String.prefix s (n / 2) in
    let repeated = repeat prefix 2 in
    String.equal repeated s)
;;

let brute_test_2 s =
  let n = String.length s in
  List.range ~start:`inclusive ~stop:`inclusive 1 (n / 2)
  |> List.find ~f:(fun prefix_length ->
    let how_many_times = n / prefix_length in
    let prefix = String.prefix s prefix_length in
    let repeated = repeat prefix how_many_times in
    String.equal repeated s)
  |> Option.is_some
;;

let rec brute_test' ~f x b res =
  match x > b with
  | true -> res
  | false ->
    let res =
      match f (Int.to_string x) with
      | true -> x :: res
      | false -> res
    in
    brute_test' ~f (x + 1) b res
;;

let partx (lines : string list) ~f =
  let ranges = List.hd_exn lines |> ranges in
  let ranges = List.map ranges ~f:range in
  List.fold ~init:0 ranges ~f:(fun acc (a, b) ->
    let res = brute_test' ~f a b [] in
    debug [%message ((a, b) : int * int) (res : int list)];
    List.sum (module Int) ~f:(fun x -> x) res + acc)
;;

let part1 (lines : string list) = partx lines ~f:brute_test_1
let part2 (lines : string list) = partx lines ~f:brute_test_2

let%expect_test _ =
  should_print_debug := true;
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    (("(a, b)" (11 22)) (res (22 11)))
    (("(a, b)" (95 115)) (res (99)))
    (("(a, b)" (998 1012)) (res (1010)))
    (("(a, b)" (1188511880 1188511890)) (res (1188511885)))
    (("(a, b)" (222220 222224)) (res (222222)))
    (("(a, b)" (1698522 1698528)) (res ()))
    (("(a, b)" (446443 446449)) (res (446446)))
    (("(a, b)" (38593856 38593862)) (res (38593859)))
    (("(a, b)" (565653 565659)) (res ()))
    (("(a, b)" (824824821 824824827)) (res ()))
    (("(a, b)" (2121212118 2121212124)) (res ()))
    ("part1 sample_1" 1227775554)
    (("(a, b)" (11 22)) (res (22 11)))
    (("(a, b)" (95 115)) (res (111 99)))
    (("(a, b)" (998 1012)) (res (1010 999)))
    (("(a, b)" (1188511880 1188511890)) (res (1188511885)))
    (("(a, b)" (222220 222224)) (res (222222)))
    (("(a, b)" (1698522 1698528)) (res ()))
    (("(a, b)" (446443 446449)) (res (446446)))
    (("(a, b)" (38593856 38593862)) (res (38593859)))
    (("(a, b)" (565653 565659)) (res (565656)))
    (("(a, b)" (824824821 824824827)) (res (824824824)))
    (("(a, b)" (2121212118 2121212124)) (res (2121212121)))
    ("part2 sample_1" 4174379265)
    |}]
;;
