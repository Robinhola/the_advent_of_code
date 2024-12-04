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
[@@deriving sexp, enumerate]

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
