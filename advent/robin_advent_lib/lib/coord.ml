open! Base
open! Core

module T = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving sexp, compare, equal, hash]
end

include T
module Hash_set = Hash_set.Make (T)
module Hashtbl = Hashtbl.Make (T)
module Map = Map.Make (T)
module Set = Set.Make (T)

let to_tuple t = t.x, t.y
let of_tuple (x, y) = { x; y }

let offset ~from:a ~to_:b =
  let x = b.x - a.x in
  let y = b.y - a.y in
  { x; y }
;;

let apply from ~offset =
  let x = from.x + offset.x in
  let y = from.y + offset.y in
  { x; y }
;;
