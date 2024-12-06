open! Base
open! Core

type t =
  | Up
  | Down
  | Left
  | Right
  | Up_right
  | Up_left
  | Down_right
  | Down_left
[@@deriving sexp, equal, enumerate]

let for_all ~f = List.map ~f all

let other_way = function
  | Up -> Down
  | Down -> Up
  | Left -> Right
  | Right -> Left
  | Up_right -> Down_left
  | Up_left -> Down_right
  | Down_right -> Up_left
  | Down_left -> Up_right
;;

let rotate = function
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up
  | Up_right -> Down_right
  | Down_right -> Down_left
  | Down_left -> Up_left
  | Up_left -> Up_right
;;

let%expect_test "Invariant" =
  List.iter all ~f:(fun dir ->
    let rotated2 = dir |> rotate |> rotate in
    let other_way = dir |> other_way in
    if not (equal rotated2 other_way)
    then print_s [%message (dir : t) (rotated2 : t) (other_way : t)])
;;
