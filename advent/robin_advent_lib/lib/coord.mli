open! Base
open! Core

type t =
  { x : int
  ; y : int
  }
[@@deriving sexp, compare, equal, hash]

module Hash_set : sig
  include Hash_set.S
end

module Map : sig
  include Map.S
end

module Set : sig
  include Set.S
end

val to_tuple : t -> int * int
val of_tuple : int * int -> t
