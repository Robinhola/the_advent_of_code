open! Base
open! Core
open! Robin_advent_lib

let numeric_keypad =
  {|789
456
123
#0A|}
  |> String.split_lines
  |> Matrix.parse
;;

let directional_keypad =
  {|#^A
<v>|}
  |> String.split_lines
  |> Matrix.parse
;;

module Cache = Hashtbl.Make (struct
    type t = char * char [@@deriving sexp, compare, hash]
  end)

let sample_1 =
  {|029A
980A
179A
456A
379A|}
  |> String.split_lines
;;

let rec find ~cache m start end_ =
  match Hashtbl.find cache (start, end_) with
  | Some value -> value
  | None when Char.equal start end_ -> []
  | None ->
    let value =
      match start, end_ with
      | '0', 'A' -> [ Dir.Right ]
      | 'A', '0' -> [ Dir.Left ]
      | '0', _ -> Dir.Up :: find ~cache m '2' end_
      | 'A', _ -> Dir.Up :: find ~cache m '3' end_
      | _, _ -> ()
    in
    Hashtbl.add_exn cache ~key:(start, end_) ~data:value
;;

let part1 (lines : string list) =
  List.iteri lines ~f:(fun i line -> print_s [%message (i : int) (line : string)]);
  0
;;

let part2 (lines : string list) =
  let _ = lines in
  0
;;

let%expect_test _ =
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    ("part1 sample_1" 0)
    ("part2 sample_1" 0)
    |}]
;;
