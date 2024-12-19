open! Base
open! Core
open Dish
open Waterfall

let part1 (lines : string list) =
  let t = parse lines in
  let t = waterfall t in
  count t
;;

let part2 (_lines : string list) = 0
