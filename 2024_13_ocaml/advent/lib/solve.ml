open! Base
open! Core
open! Robin_advent_lib

let sample_1 =
  {|Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279|}
  |> String.split_lines
;;

let parse_button s =
  match String.split_on_chars ~on:[ ' '; ','; '+' ] s with
  | [ _; _; "X"; x; ""; "Y"; y ] -> Coord.{ x = Int.of_string x; y = Int.of_string y }
  | rest -> raise_s [%message "button" (rest : string list)]
;;

let parse_prize s =
  match String.split_on_chars ~on:[ ' '; ','; '=' ] s with
  | [ _; "X"; x; ""; "Y"; y ] -> Coord.{ x = Int.of_string x; y = Int.of_string y }
  | rest -> raise_s [%message "prize" (rest : string list)]
;;

let find_best (a : Coord.t) (b : Coord.t) (p : Coord.t) =
  let nums = List.range 0 101 in
  List.cartesian_product nums nums
  |> List.find ~f:(fun (nb, na) ->
    p.x = (nb * b.x) + (na * a.x) && p.y = (nb * b.y) + (na * a.y))
;;

let rec parse current = function
  | [] -> List.rev current
  | a :: b :: c :: "" :: rest | a :: b :: c :: rest ->
    let a = parse_button a in
    let b = parse_button b in
    let c = parse_prize c in
    parse ((a, b, c) :: current) rest
  | rest -> raise_s [%message (rest : string list)]
;;

let part1 (lines : string list) =
  let l = parse [] lines in
  let l = List.filter_map l ~f:(fun (a, b, c) -> find_best a b c) in
  List.sum (module Int) l ~f:(fun (nb, na) -> (nb * 1) + (na * 3))
;;

let part2 (lines : string list) =
  let _ = lines in
  0
;;

let%expect_test _ =
  print_s [%message (parse [] sample_1 : (Coord.t * Coord.t * Coord.t) list)];
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    ("parse [] sample_1"
     ((((x 94) (y 34)) ((x 22) (y 67)) ((x 8400) (y 5400)))
      (((x 26) (y 66)) ((x 67) (y 21)) ((x 12748) (y 12176)))
      (((x 17) (y 86)) ((x 84) (y 37)) ((x 7870) (y 6450)))
      (((x 69) (y 23)) ((x 27) (y 71)) ((x 18641) (y 10279)))))
    (l
     ((((x 800000000000000) (y 400000000000000)))
      (((x -965565540984204) (y -1025658420241170)))
      (((x 380000000000000) (y -538540111729306)))
      (((x 289015419155031) (y 655698924731182)))))
    ("part1 sample_1" 480)
    ("part2 sample_1" 0)
    |}]
;;
