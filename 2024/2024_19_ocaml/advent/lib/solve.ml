open! Base
open! Core

let sample_1 =
  {|r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb|}
  |> String.split_lines
;;

type t =
  { patterns : String.Set.t
  ; designs : string list
  }
[@@deriving sexp]

let parse' patterns =
  String.split_on_chars patterns ~on:[ ','; ' ' ]
  |> List.filter ~f:(Fn.compose not String.is_empty)
  |> String.Set.of_list
;;

let parse lines =
  match lines with
  | first_line :: "" :: designs ->
    let patterns = parse' first_line in
    { patterns; designs }
  | _ -> assert false
;;

let starts_with ~s start = String.prefix s (String.length start) |> String.equal start

let rec is_possible ~patterns s =
  Set.mem patterns s
  || Set.exists patterns ~f:(fun start ->
    starts_with ~s start
    && is_possible ~patterns (String.suffix s (String.length s - String.length start)))
;;

let rec is_possible' ~patterns ?(cache = String.Table.create ()) s =
  match Hashtbl.find cache s with
  | Some result -> result
  | None ->
    let result =
      Set.to_list patterns
      |> List.sum
           (module Int)
           ~f:(fun start ->
             if String.equal s start
             then 1
             else if starts_with ~s start
             then
               is_possible'
                 ~patterns
                 ~cache
                 (String.suffix s (String.length s - String.length start))
             else 0)
    in
    Hashtbl.add_exn cache ~key:s ~data:result;
    result
;;

let part1 (lines : string list) =
  let t = parse lines in
  List.count t.designs ~f:(is_possible ~patterns:t.patterns)
;;

let part2 (lines : string list) =
  let t = parse lines in
  List.sum (module Int) t.designs ~f:(is_possible' ~patterns:t.patterns)
;;

let%expect_test _ =
  let t = parse sample_1 in
  List.iter t.designs ~f:(fun design ->
    let is_possible = is_possible ~patterns:t.patterns design in
    let count = is_possible' ~patterns:t.patterns design in
    print_s [%message (design : string) (is_possible : bool) (count : int)]);
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    ((design brwrr) (is_possible true) (count 2))
    ((design bggr) (is_possible true) (count 1))
    ((design gbbr) (is_possible true) (count 4))
    ((design rrbgbr) (is_possible true) (count 6))
    ((design ubwu) (is_possible false) (count 0))
    ((design bwurrg) (is_possible true) (count 1))
    ((design brgr) (is_possible true) (count 2))
    ((design bbrgwb) (is_possible false) (count 0))
    ("part1 sample_1" 6)
    ("part2 sample_1" 16)
    |}]
;;
