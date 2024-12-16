open! Base
open! Core
open! Robin_advent_lib

let sample_1 =
  {|###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############|}
  |> String.split_lines
;;

let sample_2 =
  {|#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################|}
  |> String.split_lines
;;

let rec find ~m ~seen ~bests queue =
  if Queue.is_empty queue
  then bests
  else (
    let pos, dir, i, path = Queue.dequeue_exn queue in
    let rdir = Dir.rotate dir in
    let ldir = Dir.rotate_left dir in
    if Matrix.get m pos |> Char.equal 'E'
    then (
      Hashtbl.add_multi bests ~key:i ~data:path;
      find ~m ~seen ~bests queue)
    else if Matrix.get m pos |> Char.equal '#'
    then find ~m ~seen ~bests queue
    else if Hashtbl.mem seen (pos, dir) && Hashtbl.find_exn seen (pos, dir) < i
    then find ~m ~seen ~bests queue
    else (
      Hashtbl.set seen ~key:(pos, dir) ~data:i;
      [ Matrix.next m pos dir
        |> Option.map ~f:(fun pos -> pos, dir, i + 1, Set.add path pos)
      ; Some (pos, rdir, i + 1000, path)
      ; Some (pos, ldir, i + 1000, path)
      ]
      |> List.filter_opt
      |> Queue.enqueue_all queue;
      find ~m ~seen ~bests queue))
;;

let find_start m =
  Matrix.all_indices m |> List.find_exn ~f:(fun c -> Matrix.get m c |> Char.equal 'S')
;;

let part1 (lines : string list) =
  let m = Matrix.parse lines in
  let seen = Side.Hashtbl.create () in
  let queue =
    Queue.of_list [ find_start m, Dir.Right, 0, Coord.Set.of_list [ find_start m ] ]
  in
  let bests = find ~m ~seen ~bests:(Int.Table.create ()) queue in
  bests |> Hashtbl.keys |> List.min_elt ~compare:Int.compare |> Option.value_exn
;;

let part2 (lines : string list) =
  let m = Matrix.parse lines in
  let seen = Side.Hashtbl.create () in
  let queue =
    Queue.of_list [ find_start m, Dir.Right, 0, Coord.Set.of_list [ find_start m ] ]
  in
  let bests = find ~m ~seen ~bests:(Int.Table.create ()) queue in
  let best_score =
    bests |> Hashtbl.keys |> List.min_elt ~compare:Int.compare |> Option.value_exn
  in
  Hashtbl.find_exn bests best_score |> Coord.Set.union_list |> Set.length
;;

let%expect_test _ =
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part1 sample_2 : int)];
  print_s [%message (part2 sample_1 : int)];
  print_s [%message (part2 sample_2 : int)];
  [%expect
    {|
    ("part1 sample_1" 7036)
    ("part1 sample_2" 11048)
    ("part2 sample_1" 45)
    ("part2 sample_2" 64)
    |}]
;;
