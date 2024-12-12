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
[@@deriving sexp, compare, equal, hash, enumerate]

val for_all : f:(t -> 'a) -> 'a list
val other_way : t -> t
val rotate : t -> t
val rotate_left : t -> t
