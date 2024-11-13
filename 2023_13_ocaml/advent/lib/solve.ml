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

let rec has_only_one_difference' a b current =
  match a, b, current with
  | _, _, f when f > 1 -> false
  | [], [], 0 -> false
  | [], [], 1 -> true
  | [], [], _ -> false
  | [], _, _ -> false
  | _, [], _ -> false
  | ca :: resta, cb :: restb, current when Char.equal ca cb ->
    has_only_one_difference' resta restb current
  | _ :: resta, _ :: restb, current -> has_only_one_difference' resta restb (current + 1)
;;

let has_only_one_difference a b =
  has_only_one_difference' (String.to_list a) (String.to_list b) 0
;;

let%expect_test _ =
  print_s [%message (has_only_one_difference "abc" "abd" : bool)];
  print_s [%message (has_only_one_difference "abc" "abc" : bool)];
  print_s [%message (has_only_one_difference "abc" "cbd" : bool)];
  [%expect
    {|
    ("has_only_one_difference \"abc\" \"abd\"" true)
    ("has_only_one_difference \"abc\" \"abc\"" false)
    ("has_only_one_difference \"abc\" \"cbd\"" false)
    |}]
;;

let rec does_reflect ?(mistakes_allowed = 0) a b =
  match a, b with
  | [], _ | _, [] -> Int.equal mistakes_allowed 0
  | va :: resta, vb :: restb when String.equal va vb ->
    does_reflect ~mistakes_allowed resta restb
  | va :: resta, vb :: restb when has_only_one_difference va vb && mistakes_allowed > 0 ->
    does_reflect ~mistakes_allowed:(mistakes_allowed - 1) resta restb
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

let rec does_reflect' ?(mistakes_allowed = 0) a b =
  (* print_s [%message (a : string list) (b : string list)]; *)
  match a, b with
  | _, [] -> None
  | a, b' :: rest ->
    Option.first_some
      (Option.some_if (does_reflect ~mistakes_allowed a b) (List.length a))
      (does_reflect' ~mistakes_allowed (b' :: a) rest)
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

let does_reflect'' ?(mistakes_allowed = 0) = function
  | [] | _ :: [] -> None
  | a :: rest -> does_reflect' ~mistakes_allowed [ a ] rest
;;

let%expect_test _ =
  let a = [ "1"; "2"; "2"; "1" ] in
  let b = [ "1"; "2"; "3" ] in
  print_s [%message (does_reflect'' a : int option)];
  print_s [%message (does_reflect'' b : int option)];
  print_s [%message (does_reflect'' ~mistakes_allowed:1 b : int option)];
  [%expect
    {|
    ("does_reflect'' a" (2))
    ("does_reflect'' b" ())
    ("does_reflect'' ~mistakes_allowed:1 b" (1))
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

let part1' ?(mistakes_allowed = 0) lines =
  parse lines
  |> List.map ~f:(fun pattern ->
    Option.first_some
      (does_reflect'' ~mistakes_allowed pattern |> Option.map ~f:(Int.( * ) 100))
      (does_reflect'' ~mistakes_allowed (pattern |> turn_rows_to_columns'))
    |> Option.value_exn)
;;

let part1 (lines : string list) = lines |> part1' |> List.sum (module Int) ~f:Fn.id

let part2 (lines : string list) =
  lines |> part1' ~mistakes_allowed:1 |> List.sum (module Int) ~f:Fn.id
;;

let%expect_test _ =
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect {|
    ("part1 sample_1" 405)
    ("part2 sample_1" 400)
    |}]
;;
