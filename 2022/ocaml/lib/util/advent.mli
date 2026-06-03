open! Base
open! Core

val read_lines : string -> year:string -> ?for_tests:string list -> unit -> string list

val solve : (module Day.T) -> unit
val solve_ : (module Day.T_any) -> unit
val solve_string : (module Day.T_string) -> unit

module Direction : sig
  type t =
    | Right
    | Left
    | Up
    | Down
    [@@deriving sexp]
  ;;

  val inc : x:int -> y:int -> t -> int * int

  val inc1 : int -> t -> int
end