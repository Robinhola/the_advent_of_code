open! Base
open! Core

type t =
  { words : char array array
  ; dims : Coord.t
  }
[@@deriving sexp]

(* Creators *)
val parse : string list -> t
val make : Coord.t -> char -> t

(* With Coords as input/ouput *)
val all_indices : t -> Coord.t list
val next : t -> Coord.t -> Dir.t -> Coord.t option

(* Get value in or out of the matrix *)
val get : t -> Coord.t -> char
val set : t -> Coord.t -> char -> unit

(* Quality Of Life utils *)
val print : t -> unit
val within_bounds : t -> Coord.t -> bool
