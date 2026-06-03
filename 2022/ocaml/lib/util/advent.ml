open! Base
open! Core

let read_lines name ~year ?for_tests () =
  let path = "input/advent_" ^ year ^ "/" ^ name ^ ".in" in
  try Stdio.In_channel.read_lines path with
  _ -> for_tests |> Option.value ~default:[]
;;

let solve (module M: Day.T) =
  print_endline M.name;
  printf "part1:\t%i\n" M.part1;
  printf "part2:\t%i\n" M.part2;
;;

let solve_string (module M: Day.T_string) =
  print_endline M.name;
  printf "part1:\t%s\n" M.part1;
  printf "part2:\t%s\n" M.part2;
;;

let solve_ (module M: Day.T_any) = 
  let to_string = function 
    | Day.Int x -> Int.to_string x
    | Day.String x -> x
  in
  print_endline M.name;
  printf "part1:\t%s\n" (to_string M.part1);
  printf "part2:\t%s\n" (to_string M.part2);
;;

module Direction = struct
  type t =
    | Right
    | Left
    | Up
    | Down
    [@@deriving sexp]
  ;;

  let inc1 x = function
    | Right -> x + 1
    | Left -> x - 1
    | Up -> x - 1
    | Down -> x + 1
  ;;

  let inc ~x ~y = function
    | Right -> x + 1, y
    | Left -> x - 1, y
    | Up -> x, y - 1
    | Down -> x, y + 1
  ;;
end
