open! Base
open! Core
open! Robin_advent_lib

let sample_1 =
  {|########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<|}
  |> String.split_lines
;;

let sample_2 =
  {|##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^|}
  |> String.split_lines
;;

let rec parse left = function
  | [] -> assert false
  | "" :: right -> Matrix.parse (List.rev left), String.concat right |> String.to_list
  | a :: rest -> parse (a :: left) rest
;;

let rec next_free m dir pos =
  let candidate = Matrix.next m pos dir in
  match Option.map candidate ~f:(Matrix.get m) with
  | None | Some '#' -> None
  | Some 'O' -> next_free m dir (Option.value_exn candidate)
  | Some '.' -> Some (Option.value_exn candidate)
  | Some c -> raise_s [%message (c : char)]
;;

let move m pos c =
  let dir =
    match c with
    | '<' -> Dir.Left
    | '>' -> Dir.Right
    | '^' -> Dir.Up
    | 'v' -> Dir.Down
    | _ -> assert false
  in
  let n = Matrix.next m pos dir |> Option.value_exn in
  match next_free m dir pos with
  | None -> pos
  | Some c ->
    Matrix.set m pos '.';
    Matrix.set m c 'O';
    Matrix.set m n '@';
    n
;;

let calculate m =
  Matrix.all_indices m
  |> List.filter_map ~f:(fun c ->
    match Matrix.get m c with
    | 'O' -> Some ((100 * c.y) + c.x)
    | _ -> None)
  |> List.sum (module Int) ~f:Fn.id
;;

let find_start m =
  Matrix.all_indices m |> List.find_exn ~f:(fun c -> Matrix.get m c |> Char.equal '@')
;;

let part1 (lines : string list) =
  let m, directions = parse [] lines in
  let pos = find_start m in
  let _ = List.fold directions ~init:pos ~f:(move m) in
  Matrix.print m;
  calculate m
;;

let increase_line l =
  l
  |> String.to_list
  |> List.concat_map ~f:(function
    | '#' -> [ '#'; '#' ]
    | 'O' -> [ '['; ']' ]
    | '.' -> [ '.'; '.' ]
    | '@' -> [ '@'; '.' ]
    | c -> raise_s [%message (c : char)])
  |> String.of_list
;;

let rec parse' left = function
  | [] -> assert false
  | "" :: right ->
    let extended = List.rev left |> List.map ~f:increase_line in
    Matrix.parse extended, String.concat right |> String.to_list
  | a :: rest -> parse' (a :: left) rest
;;

let rec can_move m pos dir =
  let n = Matrix.next m pos dir in
  match Matrix.get m pos, dir with
  | '#', _ -> false
  | '.', _ -> true
  | _, Dir.Left | _, Dir.Right -> can_move m (Option.value_exn n) dir
  | '[', Dir.Up ->
    let next = Matrix.next m pos Dir.Up |> Option.value_exn in
    let other = Matrix.next m pos Dir.Up_right |> Option.value_exn in
    can_move m next dir && can_move m other dir
  | '[', Dir.Down ->
    let next = Matrix.next m pos Dir.Down |> Option.value_exn in
    let other = Matrix.next m pos Dir.Down_right |> Option.value_exn in
    can_move m next dir && can_move m other dir
  | ']', Dir.Up ->
    let next = Matrix.next m pos Dir.Up |> Option.value_exn in
    let other = Matrix.next m pos Dir.Up_left |> Option.value_exn in
    can_move m next dir && can_move m other dir
  | ']', Dir.Down ->
    let next = Matrix.next m pos Dir.Down |> Option.value_exn in
    let other = Matrix.next m pos Dir.Down_left |> Option.value_exn in
    can_move m next dir && can_move m other dir
  | c, d -> raise_s [%message "Forgot this" (c : char) (d : Dir.t)]
;;

let rec move' ~seen m pos dir =
  let move' = move' ~seen in
  if Hash_set.mem seen pos
  then
    (* print_string "nice"; *)
    ()
  else (
    Hash_set.add seen pos;
    let vertical_move c same_box neighbour_dir =
      let same = Matrix.next m pos same_box |> Option.value_exn in
      let next = Matrix.next m pos dir |> Option.value_exn in
      let other = Matrix.next m pos neighbour_dir |> Option.value_exn in
      move' m same dir;
      move' m next dir;
      move' m other dir;
      Matrix.set m next c;
      Matrix.set m pos '.'
    in
    let n = Matrix.next m pos dir |> Option.value_exn in
    match Matrix.get m pos, dir with
    | '#', _ -> assert false
    | '.', _ -> ()
    | c, Dir.Left | c, Dir.Right ->
      move' m n dir;
      Matrix.set m n c;
      Matrix.set m pos '.'
    | '[', Dir.Up -> vertical_move '[' Right Up_right
    | '[', Dir.Down -> vertical_move '[' Right Down_right
    | ']', Dir.Up -> vertical_move ']' Left Up_left
    | ']', Dir.Down -> vertical_move ']' Left Down_left
    | c, d -> raise_s [%message "Forgot this too" (c : char) (d : Dir.t)])
;;

let move2 m pos dir =
  let n = Matrix.next m pos dir |> Option.value_exn in
  if can_move m n dir
  then (
    move' ~seen:(Coord.Hash_set.create ()) m n dir;
    Matrix.set m n '@';
    Matrix.set m pos '.';
    n)
  else pos
;;

let calculate' m =
  Matrix.all_indices m
  |> List.filter_map ~f:(fun c ->
    match Matrix.get m c with
    | '[' -> Some ((100 * c.y) + c.x)
    | _ -> None)
  |> List.sum (module Int) ~f:Fn.id
;;

let part2 (lines : string list) =
  let m, directions = parse' [] lines in
  let pos = find_start m in
  let _ =
    List.map directions ~f:(function
      | '<' -> Dir.Left
      | '>' -> Dir.Right
      | '^' -> Dir.Up
      | 'v' -> Dir.Down
      | _ -> assert false)
    |> List.fold ~init:pos ~f:(move2 m)
  in
  Matrix.print m;
  calculate' m
;;

let%expect_test _ =
  print_s [%message (parse [] sample_1 : Matrix.t * char list)];
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part1 sample_2 : int)];
  print_s [%message (parse' [] sample_1 : Matrix.t * char list)];
  print_s [%message (parse' [] sample_2 : Matrix.t * char list)];
  print_s [%message (part2 sample_1 : int)];
  print_s [%message (part2 sample_2 : int)];
  [%expect
    {|
    ("parse [] sample_1"
     (((words
        ((# # # # # # # #) (# . . O . O . #) (# # @ . O . . #) (# . . . O . . #)
         (# . # . O . . #) (# . . . O . . #) (# . . . . . . #) (# # # # # # # #)))
       (dims ((x 8) (y 8))))
      (< ^ ^ > > > v v < v > > v < <)))
    ########
    #....OO#
    ##.....#
    #.....O#
    #.#O@..#
    #...O..#
    #...O..#
    ########
    ("part1 sample_1" 2028)
    ##########
    #.O.O.OOO#
    #........#
    #OO......#
    #OO@.....#
    #O#.....O#
    #O.....OO#
    #O.....OO#
    #OO....OO#
    ##########
    ("part1 sample_2" 10092)
    ("parse' [] sample_1"
     (((words
        ((# # # # # # # # # # # # # # # #) (# # . . . . [ ] . . [ ] . . # #)
         (# # # # @ . . . [ ] . . . . # #) (# # . . . . . . [ ] . . . . # #)
         (# # . . # # . . [ ] . . . . # #) (# # . . . . . . [ ] . . . . # #)
         (# # . . . . . . . . . . . . # #) (# # # # # # # # # # # # # # # #)))
       (dims ((x 16) (y 8))))
      (< ^ ^ > > > v v < v > > v < <)))
    ("parse' [] sample_2"
     (((words
        ((# # # # # # # # # # # # # # # # # # # #)
         (# # . . . . [ ] . . . . [ ] . . [ ] # #)
         (# # . . . . . . . . . . . . [ ] . . # #)
         (# # . . [ ] [ ] . . . . [ ] . . [ ] # #)
         (# # . . . . [ ] @ . . . . . [ ] . . # #)
         (# # [ ] # # . . . . [ ] . . . . . . # #)
         (# # [ ] . . . . [ ] . . . . [ ] . . # #)
         (# # . . [ ] [ ] . . [ ] . . [ ] [ ] # #)
         (# # . . . . . . . . [ ] . . . . . . # #)
         (# # # # # # # # # # # # # # # # # # # #)))
       (dims ((x 20) (y 10))))
      (< v v > ^ < v ^ > v > ^ v v ^ v > v < > v ^ v < v < ^ v v < < < ^ > < < >
       < > > v < v v v < > ^ v ^ > ^ < < < > < < v < < < v ^ v v ^ v > ^ v v v <
       < ^ > ^ v ^ ^ > < < > > > < > ^ < < > < ^ v v ^ ^ < > v v v < > > < ^ ^ v
       > ^ > v v < > v < < < < v < ^ v > ^ < ^ ^ > > > ^ < v < v > < > v v > v ^
       v ^ < > > < > > > > < ^ ^ > v v > v < ^ ^ ^ > > v ^ v ^ < ^ ^ > v ^ ^ > v
       ^ < ^ v > v < > > v ^ v ^ < v > v ^ ^ < ^ ^ v v < < < v < ^ > > ^ ^ ^ ^ >
       > > v ^ < > v v v ^ > < v < < < > ^ ^ ^ v v ^ < v v v > ^ > v < ^ ^ ^ ^ v
       < > ^ > v v v v > < > > v ^ < < ^ ^ ^ ^ ^ ^ > < ^ > < > > > < > ^ ^ < < ^
       ^ v > > > < ^ < v > ^ < v v > > v > > > ^ v > < > ^ v > < < < < v > > v <
       v < v > v v v > ^ < > < < > ^ > < ^ > > < > ^ v < > < ^ v v v < ^ ^ < > <
       v < < < < < > < ^ v < < < > < < < ^ ^ < v < ^ ^ ^ > < ^ > > ^ < v ^ > < <
       < ^ > > ^ v < v ^ v < v ^ > ^ > > ^ v > v v > ^ < < ^ v < > > < < > < < v
       < < v > < > v < ^ v v < < < > ^ ^ v ^ > ^ ^ > > > < < ^ v > > v ^ v > < ^
       ^ > > ^ < > v v ^ < > < ^ ^ > ^ ^ ^ < > < v v v v v ^ v < v < < > ^ v < v
       > v < < ^ > < < > < < > < < < ^ ^ < < < ^ < < > > < < > < ^ ^ ^ > ^ ^ < >
       ^ > v < > ^ ^ > v v < ^ v ^ v < v v > ^ < > < v < ^ v > ^ ^ ^ > > > ^ ^ v
       v v ^ > v v v < > > > ^ < ^ > > > > > ^ < < ^ v > ^ v v v < > ^ < > < < v
       > v ^ ^ > > > < < ^ ^ < > > ^ v ^ < v ^ v v < > v ^ < < > ^ < ^ v ^ v > <
       ^ < < < > < < ^ < v > < v < > v v > > v > < v ^ < v v < > v ^ < < ^)))
    ################
    ##......[][]..##
    ####....[]....##
    ##......[]....##
    ##..##...[]...##
    ##....@.......##
    ##......[]....##
    ################
    ("part2 sample_1" 1751)
    ####################
    ##[].......[].[][]##
    ##[]...........[].##
    ##[]........[][][]##
    ##[]......[]....[]##
    ##..##......[]....##
    ##..[]............##
    ##..@......[].[][]##
    ##......[][]..[]..##
    ####################
    ("part2 sample_2" 9021)
    |}]
;;
