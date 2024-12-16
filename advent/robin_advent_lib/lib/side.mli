open! Base
open! Core

type t = Coord.t * Dir.t [@@deriving compare, equal, hash, sexp]

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
