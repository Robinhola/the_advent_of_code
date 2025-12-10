open! Base
open! Core

[@@@warning "-32"]
[@@@warning "-27"]

let should_print_debug = ref false
let debug sexp = if !should_print_debug then print_s sexp
let impossible () = raise_s [%message "impossible"]
let sum' = List.sum (module Int)
let sum = List.sum (module Int) ~f:Fn.id
let mul = List.fold ~init:1 ~f:(fun a b -> a * b)

let sample_1 =
  {|[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}|}
  |> String.split_lines
;;

type t =
  { goal : string
  ; buttons : int list list
  ; costs : int array
  }
[@@deriving sexp]

let remove_surroundings s = String.drop_suffix (String.drop_prefix s 1) 1
let parse' s = remove_surroundings s

let parse'' l =
  List.drop_last_exn l
  |> List.map ~f:remove_surroundings
  |> List.map ~f:(fun s -> String.split s ~on:',' |> List.map ~f:Int.of_string)
;;

let parse''' l =
  List.rev l
  |> List.hd_exn
  |> remove_surroundings
  |> String.split ~on:','
  |> List.map ~f:Int.of_string
  |> List.to_array
;;

let parse line =
  match String.split line ~on:' ' with
  | goal :: rest ->
    let goal = parse' goal in
    let buttons = parse'' rest in
    let costs = parse''' rest in
    { goal; buttons; costs }
  | [] -> impossible ()
;;

let switch = function
  | '.' -> '#'
  | '#' -> '.'
  | _ -> impossible ()
;;

let press input button =
  let input = String.to_array input in
  List.iter button ~f:(fun i ->
    let c = Array.get input i in
    Array.set input i (switch c));
  String.of_array input
;;

let rec find t i ~seen states =
  match List.find states ~f:(String.equal t.goal) with
  | Some goal -> i
  | None ->
    let i = i + 1 in
    let states =
      List.map states ~f:(fun s ->
        List.map t.buttons ~f:(press s)
        |> List.filter ~f:(fun s -> Hash_set.mem seen s |> not))
      |> List.concat
    in
    List.iter states ~f:(Hash_set.add seen);
    find t i ~seen states
;;

let reset goal = String.make (String.length goal) '.'

let part1 (lines : string list) =
  let l = List.map lines ~f:parse in
  debug [%message (l : t list)];
  let l =
    List.map l ~f:(fun t ->
      let start = reset t.goal in
      let seen = String.Hash_set.of_list [ start ] in
      find t 0 ~seen [ start ])
  in
  debug [%message (l : int list)];
  sum l
;;

let part2 (lines : string list) = 0

let%expect_test _ =
  should_print_debug := true;
  print_s [%message (press "...." [ 3 ] : string)];
  print_s [%message (press "...." [ 1; 3 ] : string)];
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect {|
    ("press \"....\" [3]" ...#)
    ("press \"....\" [1; 3]" .#.#)
    (l
     (((goal .##.) (buttons ((3) (1 3) (2) (2 3) (0 2) (0 1))) (costs (3 5 4 7)))
      ((goal ...#.) (buttons ((0 2 3 4) (2 3) (0 4) (0 1 2) (1 2 3 4)))
       (costs (7 5 12 7 2)))
      ((goal .###.#) (buttons ((0 1 2 3 4) (0 3 4) (0 1 2 4 5) (1 2)))
       (costs (10 11 11 5 10 5)))))
    (l (2 3 2))
    ("part1 sample_1" 7)
    ("part2 sample_1" 0)
    |}]
;;
