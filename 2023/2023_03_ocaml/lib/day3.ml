open! Base
open! Core

let example =
  [ "467..114.."
  ; "...*......"
  ; "..35..633."
  ; "......#..."
  ; "617*......"
  ; ".....+.58."
  ; "..592....."
  ; "......755."
  ; "...$.*...."
  ; ".664.598.."
  ]
;;

module Coord = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]

    let of_ y x : t = x, y
  end

  module Map = struct
    include Map.Make (T)
  end

  include T
end

module SymbolAndCoord = struct
  module T = struct
    type t = char * Coord.t [@@deriving compare, sexp]
  end

  module Set = struct
    include Set.Make (T)
  end
end

module Matrix = struct
  type 'a t = 'a array array

  let of_list (lists : 'a list list) : 'a t =
    lists |> Fn.compose List.to_array (List.map ~f:Array.of_list)
  ;;

  let range_of t =
    let leny = Array.length t in
    let lenx = Array.length t.(0) in
    let of_y y = Sequence.range 0 lenx |> Sequence.map ~f:(Coord.of_ y) in
    Sequence.range 0 leny |> Sequence.map ~f:of_y |> Sequence.concat
  ;;

  let%expect_test _ =
    let t = of_list [ [ 0; 0 ]; [ 1; 1 ] ] in
    let range = range_of t |> Sequence.to_list in
    print_s [%message (range : Coord.t list)];
    [%expect {| (range ((0 0) (1 0) (0 1) (1 1))) |}]
  ;;
end

let neighbours (x, y) =
  let distances = [ -1; 0; 1 ] in
  List.cartesian_product distances distances
  |> List.map ~f:(fun (dx, dy) -> Coord.of_ (y + dy) (x + dx))
;;

let%expect_test _ =
  print_s [%sexp (neighbours (5, 5) : (int * int) list)];
  [%expect {| ((4 4) (4 5) (4 6) (5 4) (5 5) (5 6) (6 4) (6 5) (6 6)) |}]
;;

let all_symbols lines =
  let matrix = Matrix.of_list (List.map lines ~f:String.to_list) in
  let range = Matrix.range_of matrix in
  Sequence.filter_map range ~f:(fun (x, y) ->
    match matrix.(y).(x) with
    | '.' -> None
    | c when Char.is_digit c -> None
    | symbol -> neighbours (x, y) |> List.map ~f:(fun n -> n, (symbol, (x, y))) |> Some)
  |> Sequence.to_list
  |> List.concat
  |> Coord.Map.of_alist_multi
  |> Map.map ~f:SymbolAndCoord.Set.of_list
;;

let numbers_of ~get_symbols (line : char array) =
  let lenx = Array.length line in
  let rec read current (value, symbols) x =
    match x < lenx with
    | false -> (value, symbols) :: current
    | true ->
      (match line.(x) with
       | c when Char.is_digit c ->
         let value = (value * 10) + Int.of_string (String.of_char c) in
         let symbols = Set.union (get_symbols x) symbols in
         read current (value, symbols) (x + 1)
       | _ -> read ((value, symbols) :: current) (0, SymbolAndCoord.Set.empty) (x + 1))
  in
  read [] (0, SymbolAndCoord.Set.empty) 0 |> List.filter ~f:(fun (x, _) -> x > 0)
;;

let get_symbols lines =
  let symbols = all_symbols lines in
  fun y x -> Map.find symbols (x, y) |> Option.value ~default:SymbolAndCoord.Set.empty
;;

let part1 (lines : string list) =
  let get_symbols = get_symbols lines in
  lines
  |> List.map ~f:String.to_array
  |> List.concat_mapi ~f:(fun y line -> numbers_of ~get_symbols:(get_symbols y) line)
  |> List.filter_map ~f:(fun (x, symbols) ->
    if (Fn.non Set.is_empty) symbols then Some x else None)
  |> List.sum (module Int) ~f:Fn.id
;;

let part2 (lines : string list) =
  let get_symbols = get_symbols lines in
  lines
  |> List.map ~f:String.to_array
  |> List.concat_mapi ~f:(fun y line -> numbers_of ~get_symbols:(get_symbols y) line)
  |> List.concat_map ~f:(fun (x, symbols) ->
    Set.to_list symbols
    |> List.filter_map ~f:(fun (symbol, coord) ->
      match symbol with
      | '*' -> Some (coord, x)
      | _ -> None))
  |> Coord.Map.of_alist_multi
  |> Map.filter_map ~f:(function
    | [ a; b ] -> Some (a * b)
    | _ -> None)
  |> Map.data
  |> List.sum (module Int) ~f:Fn.id
;;

let%expect_test _ =
  print_s [%message (part1 example : int)];
  print_s [%message (part2 example : int)];
  [%expect
    {|
    ("part1 example" 4361)
    ("part2 example" 467835) |}]
;;

let solve () =
  print_s [%message (part1 Input.lines : int)];
  print_s [%message (part2 Input.lines : int)]
;;
