open! Base
open! Core

type ouput =
  | String of string
  | Int of int
;;

module type T = sig
  val name : string
  val part1 : int
  val part2 : int
end

module type T_string = sig
  val name : string
  val part1 : string
  val part2 : string
end

module type T_any = sig
  val name : string
  val part1 : ouput
  val part2 : ouput
end