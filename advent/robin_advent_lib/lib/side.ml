open! Base
open! Core

module T = struct
  type t = Coord.t * Dir.t [@@deriving compare, equal, hash, sexp]
end

include T
module Hash_set = Hash_set.Make (T)
