open! Base
open! Core

type t =
  { words : char array array
  ; dims : Coord.t
  }
[@@deriving sexp]

val parse : string list -> t
val next : t -> Coord.t -> Dir.t -> Coord.t option
val all_indices : t -> Coord.t list
val get : t -> Coord.t -> char
val set : t -> Coord.t -> char -> unit
