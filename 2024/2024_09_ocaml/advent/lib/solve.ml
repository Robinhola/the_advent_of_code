open! Base
open! Core

let sample_1 = {|2333133121414131402|} |> String.split_lines

type value =
  | Free
  | File of int
[@@deriving equal, sexp]

type 'a range =
  { value : 'a
  ; start : int
  ; end_ : int
  }
[@@deriving sexp]

type state =
  { file_blocks : value range List.t
  ; free_blocks : value range List.t
  }
[@@deriving sexp]

let length range = range.end_ - range.start

let rec repeat ~p s = function
  | 0 -> s
  | i -> repeat ~p (p ^ s) (i - 1)
;;

let rec repeat' ~v l = function
  | 0 -> l
  | i -> repeat' ~v (v :: l) (i - 1)
;;

let to_string' range =
  let value =
    match range.value with
    | Free -> "."
    | File i -> Int.to_string i
  in
  repeat ~p:value "" (length range)
;;

let to_val' range =
  let value =
    match range.value with
    | Free -> 0
    | File i -> i
  in
  repeat' ~v:value [] (length range)
;;

let is_free a = equal_value Free a.value

let combine a b =
  assert (is_free a && is_free b && a.end_ = b.start);
  { value = Free; start = a.start; end_ = b.end_ }
;;

let%expect_test _ =
  let a = { value = Free; start = 0; end_ = 3 } in
  let b = { value = Free; start = 3; end_ = 5 } in
  print_s [%message (combine a b : value range)];
  [%expect {| ("combine a b" ((value Free) (start 0) (end_ 5))) |}]
;;

let sort = List.sort ~compare:(fun left right -> left.start - right.start)

let rec compact left = function
  | [] -> raise_s [%message "cannot be empty"]
  | a :: b :: rest when a.end_ = b.start -> compact left (combine a b :: rest)
  | a :: b :: rest -> compact (a :: left) (b :: rest)
  | b :: [] -> List.rev (b :: left)
;;

let sort_and_compact l = sort l |> compact []

let to_string state =
  List.concat [ state.file_blocks; state.free_blocks ]
  |> sort
  |> List.map ~f:to_string'
  |> String.concat
;;

let check_sum state =
  List.concat [ state.file_blocks; state.free_blocks ]
  |> sort
  |> List.concat_map ~f:to_val'
  |> List.mapi ~f:Int.( * )
  |> List.sum (module Int) ~f:Fn.id
;;

let range ~i ~length ~value = { value; start = i; end_ = i + length }

let rec parse' state ~i ~last_id = function
  | [] -> raise_s [%message "Should not happen"]
  | length :: [] ->
    let range = range ~i ~length ~value:(File last_id) in
    { state with file_blocks = range :: state.file_blocks }
  | file_length :: free_length :: rest ->
    let file_range = range ~i ~length:file_length ~value:(File last_id) in
    let free_range = range ~i:(i + file_length) ~length:free_length ~value:Free in
    parse'
      { file_blocks = file_range :: state.file_blocks
      ; free_blocks = free_range :: state.free_blocks
      }
      ~i:(i + file_length + free_length)
      ~last_id:(last_id + 1)
      rest
;;

let parse l =
  let state =
    String.concat l
    |> String.to_list
    |> List.map ~f:(Fn.compose Int.of_string Char.to_string)
    |> parse' { file_blocks = []; free_blocks = [] } ~i:0 ~last_id:0
  in
  { state with free_blocks = List.filter state.free_blocks ~f:(fun r -> length r > 0) }
;;

let split' range size end_ =
  assert (size < length range);
  let a = { range with start = range.start; end_ } in
  let b = { range with start = a.end_; end_ = range.end_ } in
  a, b
;;

(* sized part on the right *)
let split_right range size =
  let a, b = split' range size (range.end_ - size) in
  assert (length b = size);
  a, b
;;

(* sized part on the left *)
let split_left range size =
  let a, b = split' range size (range.start + size) in
  assert (length a = size);
  a, b
;;

let move source ~to_ =
  assert (length source = length to_);
  { source with start = to_.start; end_ = to_.end_ }
;;

let defrag state =
  let rec defrag' defragmented file_blocks free_blocks =
    match file_blocks, free_blocks with
    | [], _ -> raise_s [%message "Ran out of file blocks"]
    | _, [] -> { file_blocks = List.concat [ file_blocks; defragmented ]; free_blocks }
    | file :: _, free :: _ when file.end_ < free.start ->
      { file_blocks = List.concat [ file_blocks; defragmented ]; free_blocks }
    | file :: rest_file_blocks, free :: rest_free_blocks when length file < length free ->
      let a, b = split_left free (length file) in
      let file = move file ~to_:a in
      defrag' (file :: defragmented) rest_file_blocks (b :: rest_free_blocks)
    | file :: rest_file_blocks, free :: rest_free_blocks when length file > length free ->
      let a, b = split_right file (length free) in
      let b = move b ~to_:free in
      defrag' (b :: defragmented) (a :: rest_file_blocks) rest_free_blocks
    | file :: rest_file_blocks, free :: rest_free_blocks when length file = length free ->
      let file = move file ~to_:free in
      defrag' (file :: defragmented) rest_file_blocks rest_free_blocks
    | _ -> raise_s [%message "forgot a case"]
  in
  defrag' [] state.file_blocks (List.rev state.free_blocks)
;;

let rec can_move file unused = function
  | [] -> `Cannot_move
  | free :: rest ->
    if file.end_ <= free.start
    then `Cannot_move
    else (
      match length file with
      | x when x = length free -> `Can_move_exactly (free, List.concat [ unused; rest ])
      | x when x < length free -> `Can_move (free, unused, rest)
      | _ -> can_move file (free :: unused) rest)
;;

let continuous_defrag state =
  let file_blocks, free_blocks =
    List.fold
      state.file_blocks
      ~init:([], List.rev state.free_blocks)
      ~f:(fun (defragmented, free_blocks) file ->
        match can_move file [] free_blocks with
        | `Cannot_move -> file :: defragmented, free_blocks
        | `Can_move_exactly (free, unused) ->
          let moved_file = move file ~to_:free in
          let freed_room = move free ~to_:file in
          moved_file :: defragmented, sort_and_compact (freed_room :: unused)
        | `Can_move (free, left, right) ->
          let a, b = split_left free (length file) in
          let moved_file = move file ~to_:a in
          let freed_room = move a ~to_:file in
          ( moved_file :: defragmented
          , sort_and_compact (List.concat [ left; [ freed_room; b ]; right ]) ))
  in
  { file_blocks; free_blocks }
;;

let part1 (lines : string list) = lines |> parse |> defrag |> check_sum
let part2 (lines : string list) = lines |> parse |> continuous_defrag |> check_sum

let%expect_test _ =
  let state = parse sample_1 in
  print_s [%sexp (to_string state : string)];
  print_s [%sexp (to_string (defrag state) : string)];
  print_s [%sexp (to_string (continuous_defrag state) : string)];
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    00...111...2...333.44.5555.6666.777.888899
    0099811188827773336446555566..
    00992111777.44.333....5555.6666.....8888..
    ("part1 sample_1" 1928)
    ("part2 sample_1" 2858)
    |}]
;;
