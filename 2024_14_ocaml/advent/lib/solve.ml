open! Base
open! Core
open! Robin_advent_lib

let sample_1 =
  {|p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3|}
  |> String.split_lines
;;

type t =
  { pos : Coord.t
  ; vel : Coord.t
  }
[@@deriving sexp]

let parse_c s =
  match String.split_on_chars s ~on:[ '='; ',' ] with
  | [ _type; x; y ] -> Coord.{ x = Int.of_string x; y = Int.of_string y }
  | l -> raise_s [%message (l : string list)]
;;

let parse' line =
  match String.split line ~on:' ' with
  | [ pos; vel ] -> { pos = parse_c pos; vel = parse_c vel }
  | l -> raise_s [%message (l : string list)]
;;

let parse = List.map ~f:parse'

let pos_to_quadrant ~(lims : Coord.t) (pos : Coord.t) =
  let x, y = Coord.to_tuple pos in
  let w, h = Coord.to_tuple lims in
  let hw, hh = w / 2, h / 2 in
  if x = hw || y = hh
  then None
  else
    Some Coord.{ x = (if pos.x >= hw then 1 else 0); y = (if pos.y >= hh then 1 else 0) }
;;

let%expect_test _ =
  let open Coord in
  let lims = { x = 11; y = 7 } in
  let l =
    List.filter_map
      ~f:(pos_to_quadrant ~lims)
      [ { x = 0; y = 0 }
      ; { x = 5; y = 3 }
      ; { x = 6; y = 4 }
      ; { x = 10; y = 6 }
      ; { x = 10; y = 0 }
      ]
  in
  print_s [%message (l : Coord.t list)];
  [%expect {| (l (((x 0) (y 0)) ((x 1) (y 1)) ((x 1) (y 1)) ((x 1) (y 0)))) |}]
;;

let move ~(lims : Coord.t) t ~time =
  let pos = t.pos in
  let vel = t.vel in
  let x = (pos.x + (vel.x * time)) % lims.x in
  let y = (pos.y + (vel.y * time)) % lims.y in
  let pos' = Coord.{ x; y } in
  { t with pos = pos' }
;;

let part1' (lims : Coord.t) lines =
  let count = Coord.Hashtbl.create () in
  let l = parse lines in
  List.iter l ~f:(fun t ->
    let pos = (move ~lims t ~time:100).pos in
    match pos_to_quadrant ~lims pos with
    | None -> ()
    | Some q ->
      let c = Hashtbl.find_or_add count q ~default:(Fn.const 0) in
      Hashtbl.set count ~key:q ~data:(c + 1));
  count
  |> Hashtbl.to_alist
  |> List.fold ~init:1 ~f:(fun total (_, count) -> total * count)
;;

let part1 (lines : string list) =
  let lims = Coord.{ x = 101; y = 103 } in
  part1' lims lines
;;

let part2 (lines : string list) =
  let lims = Coord.{ x = 101; y = 103 } in
  let m = Matrix.make lims '.' in
  let l = parse lines in
  let l = List.map l ~f:(move ~lims ~time:6516) in
  List.iter l ~f:(fun t -> Matrix.set m t.pos 'x');
  print_endline ("------------ " ^ Int.to_string 6516 ^ " ------------");
  Matrix.print m;
  6516
;;

let%expect_test _ =
  let lims = Coord.{ x = 11; y = 7 } in
  print_s [%message (part1' lims sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    ("part1' lims sample_1" 12)
    ("part2 sample_1" 0)
    |}]
;;
