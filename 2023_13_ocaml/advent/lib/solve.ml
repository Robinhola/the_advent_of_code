open! Base
open! Core

let sample_1 =
  "#.##..##.\n\
   ..#.##.#.\n\
   ##......#\n\
   ##......#\n\
   ..#.##.#.\n\
   ..##..##.\n\
   #.#.##.#.\n\n\
   #...##..#\n\
   #....#..#\n\
   ..##..###\n\
   #####.##.\n\
   #####.##.\n\
   ..##..###\n\
   #....#..#"
  |> String.split_lines
;;

let rec parse_all (result : string list list) (current : string list) = function
  | [] -> List.rev current :: result
  | "" :: rest -> parse_all (List.rev current :: result) [] rest
  | line :: rest -> parse_all result (line :: current) rest
;;

let parse s = s |> parse_all [] [] |> List.rev

let%expect_test _ =
  print_s [%message (parse sample_1 : string list list)];
  [%expect
    {|
    ("parse sample_1"
     ((#.##..##. ..#.##.#. ##......# ##......# ..#.##.#. ..##..##. #.#.##.#.)
      (#...##..# #....#..# ..##..### #####.##. #####.##. ..##..### #....#..#)))
    |}]
;;

let rec does_reflect a b =
  match a, b with
  | [], _ | _, [] -> true
  | va :: resta, vb :: restb when String.equal va vb -> does_reflect resta restb
  | _ -> false
;;

let%expect_test _ =
  let a = [ "1"; "2" ] in
  let b = [ "1" ] in
  let c = [ "1"; "3" ] in
  print_s [%message (does_reflect a b : bool)];
  print_s [%message (does_reflect a a : bool)];
  print_s [%message (does_reflect a c : bool)];
  [%expect
    {|
    ("does_reflect a b" true)
    ("does_reflect a a" true)
    ("does_reflect a c" false)
    |}]
;;

let rec does_reflect' a b =
  (* print_s [%message (a : string list) (b : string list)]; *)
  match a, b with
  | _, [] -> None
  | a, b' :: rest ->
    Option.first_some
      (Option.some_if (does_reflect a b) (List.length a))
      (does_reflect' (b' :: a) rest)
;;

let%expect_test _ =
  let a = [ "1" ] in
  let b = [ "2"; "2"; "1" ] in
  let c = [ "2"; "3" ] in
  print_s [%message (does_reflect' a b : int option)];
  print_s [%message (does_reflect' a a : int option)];
  print_s [%message (does_reflect' a c : int option)];
  [%expect
    {|
    ("does_reflect' a b" (2))
    ("does_reflect' a a" (1))
    ("does_reflect' a c" ())
    |}]
;;

let does_reflect'' = function
  | [] | _ :: [] -> None
  | a :: rest -> does_reflect' [ a ] rest
;;

let%expect_test _ =
  let a = [ "1"; "2"; "2"; "1" ] in
  let b = [ "1"; "2"; "3" ] in
  print_s [%message (does_reflect'' a : int option)];
  print_s [%message (does_reflect'' b : int option)];
  [%expect {|
    ("does_reflect'' a" (2))
    ("does_reflect'' b" ())
    |}]
;;

let%expect_test _ =
  let b = parse sample_1 |> List.rev |> List.hd_exn in
  print_s [%message (does_reflect'' b : int option)];
  [%expect {| ("does_reflect'' b" (4)) |}]
;;

let rec turn_rows_to_columns (cols : char list list) = function
  | [] -> cols |> List.map ~f:(Fn.compose String.of_list List.rev)
  | row :: rest ->
    let cols =
      match cols with
      | [] -> row |> List.map ~f:(fun c -> [ c ])
      | _ -> List.zip_exn row cols |> List.map ~f:(fun (c, col) -> c :: col)
    in
    turn_rows_to_columns cols rest
;;

let turn_rows_to_columns' (rows : string list) =
  turn_rows_to_columns [] (rows |> List.map ~f:String.to_list)
;;

let%expect_test _ =
  print_s [%message (turn_rows_to_columns' [ "abc"; "ABC" ] : string list)];
  [%expect {| ("turn_rows_to_columns' [\"abc\"; \"ABC\"]" (aA bB cC)) |}]
;;

let part1' lines =
  parse lines
  |> List.map ~f:(fun pattern ->
    Option.first_some
      (does_reflect'' pattern |> Option.map ~f:(Int.( * ) 100))
      (does_reflect'' (pattern |> turn_rows_to_columns'))
    |> Option.value_exn)
;;

let part1 (lines : string list) = lines |> part1' |> List.sum (module Int) ~f:Fn.id

let%expect_test _ =
  print_s [%message (part1 sample_1 : int)];
  [%expect {| ("part1 sample_1" 405) |}]
;;

let part2 (_lines : string list) = 0
