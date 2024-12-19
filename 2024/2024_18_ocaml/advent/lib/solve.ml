open! Base
open! Core
open! Robin_advent_lib

let sample_1 =
  {|5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0|}
  |> String.split_lines
;;

let small =
  let m = Matrix.make (Coord.of_tuple (7, 7)) '.' in
  Matrix.set m (Coord.of_tuple (0, 0)) 'S';
  Matrix.set m (Coord.of_tuple (6, 6)) 'E';
  m
;;

let big =
  let m = Matrix.make (Coord.of_tuple (71, 71)) '.' in
  Matrix.set m (Coord.of_tuple (0, 0)) 'S';
  Matrix.set m (Coord.of_tuple (70, 70)) 'E';
  m
;;

type t =
  { bytes : Coord.t list
  ; matrix : Matrix.t
  }
[@@deriving sexp]

let parse lines matrix =
  let bytes =
    List.map lines ~f:(fun s ->
      match String.split s ~on:',' with
      | [ x; y ] -> Coord.of_tuple (Int.of_string x, Int.of_string y)
      | _ -> assert false)
  in
  { bytes; matrix }
;;

let fall' t c = Matrix.set t.matrix c '#'

let fall t i =
  List.take t.bytes i |> List.iter ~f:(fall' t);
  t
;;

let find_path t =
  let seen = Coord.Hash_set.create () in
  let neigbhours c =
    List.filter_map [ Dir.Up; Dir.Right; Dir.Down; Dir.Left ] ~f:(fun dir ->
      Matrix.next t.matrix c dir)
  in
  let start = Coord.of_tuple (0, 0) in
  let rec find candidates =
    match candidates with
    | [] -> None
    | candidates ->
      (match
         List.find candidates ~f:(fun (c, _path) ->
           Matrix.get t.matrix c |> Char.equal 'E')
       with
       | Some (_, path) -> Some path
       | None ->
         let candidates =
           List.concat_map candidates ~f:(fun (c, path) ->
             if Hash_set.mem seen c
             then []
             else if Matrix.get t.matrix c |> Char.equal '#'
             then []
             else (
               Hash_set.add seen c;
               List.map (neigbhours c) ~f:(fun n -> n, Set.add path n)))
         in
         find candidates)
  in
  (* Don't count the start in the path *)
  find [ start, Coord.Set.empty ]
;;

let part1 (lines : string list) =
  let t = parse lines big in
  let t = fall t 1024 in
  find_path t |> Option.value_exn |> Set.length
;;

let part2' (lines : string list) m i =
  let t = parse lines m in
  let t = fall t i in
  let _, rest = List.split_n t.bytes i in
  let first =
    List.find_exn rest ~f:(fun c ->
      fall' t c;
      find_path t |> Option.is_none)
  in
  print_s [%message (first : Coord.t)]
;;

let part2 (lines : string list) =
  let t = parse lines big in
  let t = fall t 1024 in
  let _, rest = List.split_n t.bytes 1024 in
  let valid_path = find_path t |> Option.value_exn in
  let last_path = ref valid_path in
  let first =
    List.find_exn rest ~f:(fun c ->
      fall' t c;
      Set.mem !last_path c
      && find_path t |> Option.map ~f:(fun p -> last_path := p) |> Option.is_none)
  in
  print_s [%message (first : Coord.t)];
  0
;;

let%expect_test _ =
  let t = parse sample_1 small in
  let t = fall t 12 in
  Matrix.print t.matrix;
  print_s [%message (find_path t |> Option.map ~f:Set.length : int option)];
  part2' sample_1 small 12;
  [%expect
    {|
    S..#...
    ..#..#.
    ....#..
    ...#..#
    ..#..#.
    .#..#..
    #.#...E
    ("(find_path t) |> (Option.map ~f:Set.length)" (22))
    (first ((x 6) (y 1)))
    |}]
;;
