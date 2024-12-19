open! Base
open! Core

type t = char array array [@@deriving sexp]

val parse : string list -> t
val count : t -> int
