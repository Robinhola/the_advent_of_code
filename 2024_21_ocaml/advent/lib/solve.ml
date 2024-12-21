open! Base
open! Core
open! Robin_advent_lib

let _numeric_keypad =
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

let from_dir_to_dir = function
  | Dir.Up -> '^'
  | Dir.Right -> '>'
  | Dir.Left -> '<'
  | Dir.Down -> 'v'
  | _ -> assert false
;;

let min l = List.min_elt ~compare:Int.compare l |> Option.value_exn
let min' t = Hashtbl.keys t |> min

let rec find ~m ~seen ~bests to_ queue =
  if Queue.is_empty queue
  then bests
  else (
    let pos, i, path = Queue.dequeue_exn queue in
    if Matrix.get m pos |> Char.equal to_
    then (
      Hashtbl.add_multi bests ~key:i ~data:path;
      find ~m ~seen ~bests to_ queue)
    else if Matrix.get m pos |> Char.equal '#'
    then find ~m ~seen ~bests to_ queue
    else if
      (Hashtbl.mem seen pos && Hashtbl.find_exn seen pos < i)
      || (Hashtbl.length bests > 0 && min' bests < i)
    then find ~m ~seen ~bests to_ queue
    else (
      Hashtbl.set seen ~key:pos ~data:i;
      [ Dir.Up; Dir.Right; Dir.Down; Dir.Left ]
      |> List.filter_map ~f:(fun dir ->
        Matrix.next m pos dir
        |> Option.map ~f:(fun c -> c, i + 1, from_dir_to_dir dir :: path))
      |> Queue.enqueue_all queue;
      find ~m ~seen ~bests to_ queue))
;;

let part1 (lines : string list) =
  List.iteri lines ~f:(fun i line -> print_s [%message (i : int) (line : string)]);
  0
;;

let part2 (lines : string list) =
  let _ = lines in
  0
;;

let partx from to_ =
  let m = directional_keypad in
  let seen = Coord.Hashtbl.create () in
  let start = Matrix.find_exn m from in
  let queue = Queue.of_list [ start, 0, [] ] in
  find ~m ~seen ~bests:(Int.Table.create ()) to_ queue
;;

let%expect_test _ =
  print_s [%message (partx 'A' '^' : char list list Int.Table.t)];
  print_s [%message (partx 'A' '<' : char list list Int.Table.t)];
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    ("partx 'A' '^'" ((1 ((<)))))
    ("partx 'A' '<'" ((3 ((< v <) (< < v)))))
    ((i 0) (line 029A))
    ((i 1) (line 980A))
    ((i 2) (line 179A))
    ((i 3) (line 456A))
    ((i 4) (line 379A))
    ("part1 sample_1" 0)
    ("part2 sample_1" 0)
    |}]
;;
