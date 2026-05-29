open! Base
open! Core

[@@@warning "-32"]
[@@@warning "-27"]

let should_print_debug = ref false
let debug sexp = if !should_print_debug then print_s sexp
let sum = List.sum (module Int)

let sample_1 =
  {|3-5
10-14
16-20
12-18

1
5
8
11
17
32|}
  |> String.split_lines
;;

type range =
  { a : int
  ; b : int
  }
[@@deriving sexp]

let parse_range s =
  match String.split ~on:'-' s with
  | [ a; b ] ->
    let a = Int.of_string a in
    let b = Int.of_string b in
    { a; b }
  | _ -> raise_s [%message "impossible"]
;;

let rec parse mode ranges lines =
  match mode, lines with
  | `Range, "" :: rest -> ranges, List.map rest ~f:Int.of_string
  | `Range, l :: rest -> parse `Range (parse_range l :: ranges) rest
  | _ -> raise_s [%message "impossible"]
;;

let check x range = range.a <= x && x <= range.b
let check' x ranges = List.find ranges ~f:(check x)

let part1 (lines : string list) =
  let ranges, values = parse `Range [] lines in
  debug [%message (ranges : range list) (values : int list)];
  sum values ~f:(fun x -> if check' x ranges |> Option.is_some then 1 else 0)
;;

let rec sanitize ranges range =
  let { a; b } = range in
  match check' a ranges, check' b ranges with
  | Some left, Some right ->
    let a = left.b + 1 in
    let b = right.a - 1 in
    if a > b then None else sanitize ranges { a; b }
  | Some left, None ->
    let a = left.b + 1 in
    if a > b then None else sanitize ranges { a; b }
  | None, Some right ->
    let b = right.a - 1 in
    if a > b then None else sanitize ranges { a; b }
  | None, None -> Some { a; b }
;;

let rec sanitize' before x after =
  let combined = before @ after in
  match after, sanitize combined x with
  | [], _ -> x :: combined
  | next :: rest, Some r -> sanitize' (r :: before) next rest
  | next :: rest, None -> sanitize' before next rest
;;

let distance range = range.b - range.a + 1

let part2 (lines : string list) =
  let ranges, _ = parse `Range [] lines in
  let sanitized = sanitize' [] (List.hd_exn ranges) (List.drop ranges 1) in
  debug [%message (sanitized : range list)];
  List.map sanitized ~f:distance |> sum ~f:Fn.id
;;

let%expect_test _ =
  should_print_debug := true;
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    ((ranges (((a 12) (b 18)) ((a 16) (b 20)) ((a 10) (b 14)) ((a 3) (b 5))))
     (values (1 5 8 11 17 32)))
    ("part1 sample_1" 3)
    (sanitized (((a 3) (b 5)) ((a 10) (b 14)) ((a 16) (b 20)) ((a 15) (b 15))))
    ("part2 sample_1" 14)
    |}]
;;
