open! Base
open! Core

type t = Coord.t * Dir.t [@@deriving compare, equal, hash, sexp]

module Hash_set : sig
  include Hash_set.S with type elt := t
end
