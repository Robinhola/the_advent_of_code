open! Base
open! Core
open! Robin_advent_lib

[@@@warning "-32"]
[@@@warning "-27"]
[@@@warning "-26"]

let should_print_debug = ref false
let debug sexp = if !should_print_debug then print_s sexp
let sum = List.sum (module Int) ~f:Fn.id
let mul = List.fold ~init:1 ~f:(fun a b -> a * b)

let sample_1 =
  {|123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  |}
  |> String.split_lines
;;

let map_signs_to_cols signs cols =
  List.mapi signs ~f:(fun i sign ->
    match sign with
    | "*" -> Array.get cols i |> mul
    | "+" -> Array.get cols i |> sum
    | _ -> raise_s [%message "impossible"])
;;

let part1 (lines : string list) =
  let l =
    List.map lines ~f:(fun s ->
      String.split s ~on:' ' |> List.filter ~f:(fun s -> not (String.is_empty s)))
  in
  debug [%message (l : string list list)];
  let nums = List.take l (List.length l - 1) |> List.map ~f:(List.map ~f:Int.of_string) in
  let cols = List.hd_exn nums |> Array.of_list_map ~f:(fun n -> [ n ]) in
  let signs = List.rev l |> List.hd_exn in
  debug [%message (cols : int list array) (signs : string list)];
  List.drop nums 1
  |> List.iter ~f:(fun l ->
    List.iteri l ~f:(fun i x ->
      let col = Array.get cols i in
      let new_col = x :: col in
      Array.set cols i new_col));
  debug [%message (cols : int list array) (signs : string list)];
  let vals = map_signs_to_cols signs cols in
  sum vals
;;

let rec add_column t coord vals =
  let x = Matrix.get t coord in
  let vals = x :: vals in
  match Matrix.next t coord Dir.Down with
  | Some c ->
    let x = Matrix.get t c in
    add_column t c vals
  | None -> vals
;;

let col_to_num col =
  let s =
    List.drop col 1
    |> List.rev
    |> String.of_list
    |> String.substr_replace_all ~pattern:" " ~with_:""
  in
  if String.is_empty s then 0 else Int.of_string s
;;

let rec split_on_zero nums cur lists =
  match nums with
  | [] -> lists @ [ cur ]
  | 0 :: rest -> split_on_zero rest [] (lists @ [ cur ])
  | n :: rest -> split_on_zero rest (n :: cur) lists
;;

let part2 (lines : string list) =
  let t = Matrix.parse lines in
  let num_of_cols = List.hd_exn lines |> String.length in
  let cols = Array.init num_of_cols ~f:(fun _ -> []) in
  Array.iteri cols ~f:(fun i _ ->
    let col = add_column t (Coord.of_tuple (i, 0)) [] in
    Array.set cols i col);
  debug [%message (cols : char list array)];
  let nums = Array.map cols ~f:col_to_num |> Array.to_list in
  debug [%message (nums : int list)];
  let signs =
    List.map lines ~f:(fun s ->
      String.split s ~on:' ' |> List.filter ~f:(fun s -> not (String.is_empty s)))
    |> List.rev
    |> List.hd_exn
  in
  let cols = split_on_zero nums [] [] |> Array.of_list in
  debug [%message (cols : int list array) (signs : string list)];
  let vals = map_signs_to_cols signs cols in
  sum vals
;;

let%expect_test _ =
  should_print_debug := true;
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    (l ((123 328 51 64) (45 64 387 23) (6 98 215 314) (* + * +)))
    ((cols ((123) (328) (51) (64))) (signs (* + * +)))
    ((cols ((123 45 6) (328 64 98) (51 387 215) (64 23 314))) (signs (* + * +)))
    ("part1 sample_1" 4277556)
    (cols
     ((* " " " " 1) (" " " " 4 2) (" " 6 5 3) (" " " " " " " ") (+ 9 6 3)
      (" " 8 4 2) (" " " " " " 8) (" " " " " " " ") (* 2 3 " ") (" " 1 8 5)
      (" " 5 7 1) (" " " " " " " ") (+ 3 2 6) (" " 1 3 4) (" " 4 " " " ")))
    (nums (1 24 356 0 369 248 8 0 32 581 175 0 623 431 4))
    ((cols ((356 24 1) (8 248 369) (175 581 32) (4 431 623))) (signs (* + * +)))
    ("part2 sample_1" 3263827)
    |}]
;;
