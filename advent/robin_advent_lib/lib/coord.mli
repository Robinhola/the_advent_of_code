open! Base
open! Core

type t =
  { x : int
  ; y : int
  }
[@@deriving sexp, compare, equal, hash]

module Hash_set : sig
  include Hash_set.S with type elt := t
end

module Hashtbl : sig
  include Hashtbl.S with type key := t
end

module Map : sig
  include Map.S with type Key.t := t
end

module Set : sig
  include Set.S with type Elt.t := t
end

val to_tuple : t -> int * int
val of_tuple : int * int -> t
