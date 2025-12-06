open! Base
open! Core

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

let part1 (lines : string list) =
  let l =
    List.map lines ~f:(fun s ->
      String.split s ~on:' ' |> List.filter ~f:(fun s -> not (String.is_empty s)))
  in
  debug [%message (l : string list list)];
  let nums = List.take l (List.length l - 1) |> List.map ~f:(List.map ~f:Int.of_string) in
  let cols = List.hd_exn nums |> Array.of_list_map ~f:(fun n -> Array.of_list [ n ]) in
  let signs = List.rev l |> List.hd_exn in
  debug [%message (cols : int array array) (signs : string list)];
  List.drop nums 1
  |> List.iter ~f:(fun l ->
    List.iteri l ~f:(fun i x ->
      let col = Array.get cols i in
      let new_col = Array.append col (Array.of_list [ x ]) in
      Array.set cols i new_col));
  debug [%message (cols : int array array) (signs : string list)];
  let vals =
    List.mapi signs ~f:(fun i sign ->
      match sign with
      | "*" -> Array.get cols i |> Array.to_list |> mul
      | "+" -> Array.get cols i |> Array.to_list |> sum
      | _ -> raise_s [%message "impossible"])
  in
  sum vals
;;

let part2 (lines : string list) = 0

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
    ("part2 sample_1" 0)
    |}]
;;
