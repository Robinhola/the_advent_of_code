open! Base
open! Core
open! Robin_advent_lib

let numeric_keypad =
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

let sample_1 =
  {|029A
980A
179A
456A
379A|}
  |> String.split_lines
;;

let deux_a_deux code =
  let rec deux_a_deux current = function
    | [] -> assert false
    | _ :: [] -> List.rev current
    | a :: b :: rest -> deux_a_deux ((a, b) :: current) (b :: rest)
  in
  deux_a_deux [] (String.to_list ("A" ^ code))
;;

let path m start end_ =
  let from = Matrix.find_exn m start in
  let to_ = Matrix.find_exn m end_ in
  let offset = Coord.offset ~from ~to_ in
  let horizontal =
    if offset.x > 0 then String.make offset.x '>' else String.make (-offset.x) '<'
  in
  let vertical =
    if offset.y > 0 then String.make offset.y 'v' else String.make (-offset.y) '^'
  in
  let prefer_vertical =
    let bad = Coord.offset ~from ~to_:(Matrix.find_exn m '#') in
    (* This is the bit that I couldn't come up with *)
    offset.x > 0
    || (Coord.equal bad { offset with y = 0 }
        && not (Coord.equal bad { offset with x = 0 }))
    (* List.exists [ '^'; 'A'; '0' ] ~f:(Char.equal start) *)
    (* && List.exists [ '1'; '4'; '7'; '<' ] ~f:(Char.equal end_) *)
  in
  String.concat
    (if prefer_vertical
     then [ vertical; horizontal; "A" ]
     else [ horizontal; vertical; "A" ])
;;

let%expect_test _ =
  print_s [%message (path numeric_keypad '0' '9' : string)];
  print_s [%message (path numeric_keypad '0' '0' : string)];
  print_s [%message (path numeric_keypad '0' '1' : string)];
  print_s [%message (path numeric_keypad '9' '0' : string)];
  print_s [%message (path numeric_keypad '9' '1' : string)];
  print_s [%message (path directional_keypad 'A' '<' : string)];
  print_s [%message (path directional_keypad '<' 'A' : string)];
  [%expect
    {|
    ("path numeric_keypad '0' '9'" ^^^>A)
    ("path numeric_keypad '0' '0'" A)
    ("path numeric_keypad '0' '1'" ^<A)
    ("path numeric_keypad '9' '0'" <vvvA)
    ("path numeric_keypad '9' '1'" <<vvA)
    ("path directional_keypad 'A' '<'" v<<A)
    ("path directional_keypad '<' 'A'" ^>>A)
    |}]
;;

let from_code_to_direction code =
  deux_a_deux code
  |> List.map ~f:(fun (start, end_) -> path numeric_keypad start end_)
  |> String.concat
;;

let%expect_test _ =
  List.iter sample_1 ~f:(fun code ->
    print_s [%message (code : string) ~path:(from_code_to_direction code : string)]);
  [%expect
    {|
    ((code 029A) (path <A^A^^>AvvvA))
    ((code 980A) (path ^^^A<AvvvA>A))
    ((code 179A) (path ^<<A^^A>>AvvvA))
    ((code 456A) (path ^^<<A>A>AvvA))
    ((code 379A) (path ^A<<^^A>>AvvvA))
    |}]
;;

let rec from_direction_to_more_direction current_path depth =
  if depth = 0
  then current_path
  else
    deux_a_deux current_path
    |> List.map ~f:(fun (a, b) ->
      let current_path = path directional_keypad a b in
      from_direction_to_more_direction current_path (depth - 1))
    |> String.concat
;;

module Cache = Hashtbl.Make (struct
    type t = string * int [@@deriving sexp, hash, compare, equal]
  end)

let from_direction_to_more_direction' =
  let cache = Cache.create () in
  let rec from_direction_to_more_direction' current_path depth =
    let key = current_path, depth in
    match Hashtbl.find cache key with
    | Some value -> value
    | None ->
      let value =
        if depth = 0
        then current_path |> String.length
        else
          deux_a_deux current_path
          |> List.sum
               (module Int)
               ~f:(fun (a, b) ->
                 let current_path = path directional_keypad a b in
                 from_direction_to_more_direction' current_path (depth - 1))
      in
      Hashtbl.add_exn cache ~key ~data:value;
      value
  in
  from_direction_to_more_direction'
;;

let%expect_test _ =
  List.iter sample_1 ~f:(fun code ->
    let first_level = from_code_to_direction code in
    print_s
      [%message
        (code : string)
          ~path:(first_level |> fun d -> from_direction_to_more_direction d 1 : string)]);
  [%expect
    {|
    ((code 029A) (path v<<A^>>A<A>A<AAv>A^A<vAAA^>A))
    ((code 980A) (path <AAA>Av<<A^>>A<vAAA^>AvA^A))
    ((code 179A) (path <Av<AA^>>A<AA>AvAA^A<vAAA^>A))
    ((code 456A) (path <AAv<AA^>>AvA^AvA^A<vAA^>A))
    ((code 379A) (path <A>Av<<AA^>AA>AvAA^A<vAAA^>A))
    |}]
;;

let num_part code = String.prefix code (String.length code - 1) |> Int.of_string

let part1 lines =
  List.sum
    (module Int)
    lines
    ~f:(fun code ->
      let n = num_part code in
      let first_level = from_code_to_direction code in
      let l = from_direction_to_more_direction first_level 2 |> String.length in
      n * l)
;;

let part2 =
  List.sum
    (module Int)
    ~f:(fun code ->
      let n = num_part code in
      let first_level = from_code_to_direction code in
      let l = from_direction_to_more_direction' first_level 25 in
      n * l)
;;

let%expect_test _ =
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    ("part1 sample_1" 126384)
    ("part2 sample_1" 132680405989994)
    |}]
;;
