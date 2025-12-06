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

let rec split_blocks blocks current results =
  match blocks with
  | [] -> current :: results
  | "" :: rest -> split_blocks rest [] (current :: results)
  | line :: rest -> split_blocks rest (line :: current) results
;;

let rec process_block vals block =
  match block with
  | [ last_value ] ->
    let value = String.drop_suffix last_value 1 |> Int.of_string in
    let sign = String.drop_prefix last_value (String.length last_value - 1) in
    let vals = value :: vals in
    (match sign with
     | "+" -> sum vals
     | "*" -> mul vals
     | _ -> raise_s [%message "impossible"])
  | value :: rest ->
    let vals = Int.of_string value :: vals in
    process_block vals rest
  | _ -> raise_s [%message "impossible"]
;;

let part2 (lines : string list) =
  let t = Matrix.parse lines in
  let t = Matrix.transpose t in
  let sanitized =
    Matrix.to_list t
    |> List.map ~f:String.of_list
    |> List.map ~f:(String.substr_replace_all ~pattern:" " ~with_:"")
  in
  let blocks = split_blocks sanitized [] [] in
  sum (List.map blocks ~f:(process_block []))
;;

let%expect_test _ =
  should_print_debug := true;
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    (l ((123 328 51 64) (45 64 387 23) (6 98 215 314) (* + * +)))
    ((cols ((123) (328) (51) (64))) (signs (* + * +)))
    ((cols ((6 45 123) (98 64 328) (215 387 51) (314 23 64))) (signs (* + * +)))
    ("part1 sample_1" 4277556)
    (blocks ((4 431 623+) (175 581 32*) (8 248 369+) (356 24 1*)))
    ("part2 sample_1" 3263827)
    |}]
;;
