open! Base
open! Core
open! Robin_advent_lib

let sample_1 = {|125 17|} |> String.split_lines

let split' s =
  let length = String.length s in
  let half = length / 2 in
  [ String.drop_suffix s half; String.drop_prefix s half ] |> List.map ~f:Int.of_string
;;

let split = function
  | 0 -> [ 1 ]
  | n ->
    let n' = Int.to_string n in
    if String.length n' % 2 = 0 then split' n' else [ n * 2024 ]
;;

let parse lines = String.concat lines |> String.split ~on:' ' |> List.map ~f:Int.of_string

let%expect_test _ =
  let nums = parse sample_1 in
  print_s [%sexp (nums : int list)];
  let nums = List.concat_map nums ~f:split in
  print_s [%sexp (nums : int list)];
  let nums = List.concat_map nums ~f:split in
  print_s [%sexp (nums : int list)];
  let nums = List.concat_map nums ~f:split in
  print_s [%sexp (nums : int list)];
  let nums = List.concat_map nums ~f:split in
  print_s [%sexp (nums : int list)];
  let nums = List.concat_map nums ~f:split in
  print_s [%sexp (nums : int list)];
  let nums = List.concat_map nums ~f:split in
  print_s [%sexp (nums : int list)];
  [%expect
    {|
    (125 17)
    (253000 1 7)
    (253 0 2024 14168)
    (512072 1 20 24 28676032)
    (512 72 2024 2 0 2 4 2867 6032)
    (1036288 7 2 20 24 4048 1 4048 8096 28 67 60 32)
    (2097446912 14168 4048 2 0 2 4 40 48 2024 40 48 80 96 2 8 6 7 6 0 3 2)
    |}]
;;

let partx ~total_iterations ~batch_size (nums : int list) =
  assert (total_iterations % batch_size = 0);
  let goal = total_iterations / batch_size in
  let cache = Coord.Hashtbl.create () in
  let rec partx' ~goal i x =
    let iterations_so_far = i * batch_size in
    let key = Coord.of_tuple (x, iterations_so_far) in
    match Hashtbl.find cache key with
    | Some value -> value
    | None ->
      let value =
        match i = goal with
        | true ->
          Fn.apply_n_times ~n:batch_size (List.concat_map ~f:split) [ x ] |> List.length
        | false ->
          let next_batch =
            Fn.apply_n_times ~n:batch_size (List.concat_map ~f:split) [ x ]
          in
          List.sum (module Int) ~f:(partx' ~goal (i + 1)) next_batch
      in
      Hashtbl.add_exn cache ~key ~data:value;
      value
  in
  List.sum (module Int) ~f:(partx' ~goal 1) nums
;;

let part1 (lines : string list) = parse lines |> partx ~total_iterations:25 ~batch_size:5
let part2 (lines : string list) = parse lines |> partx ~total_iterations:75 ~batch_size:3

let%expect_test _ =
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    ("part1 sample_1" 55312)
    ("part2 sample_1" 65601038650482)
    |}]
;;
