open! Base
open! Core
open Import

open Advent

let all () =
  print_endline "Welcome to 2019! Let the advent of code... begin!!!";
  solve (module Day01.T);
  solve (module Day02.T);
  solve (module Day03.T);
  solve (module Day04.T);
  solve (module Day05.T);
  solve (module Day06.T);
;;
