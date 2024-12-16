open! Base
open! Core

module T = struct
  type t = Coord.t * Dir.t [@@deriving compare, equal, hash, sexp]
end

include T
module Hash_set = Hash_set.Make (T)
module Set = Set.Make (T)
module Map = Map.Make (T)
module Hashtbl = Hashtbl.Make (T)
