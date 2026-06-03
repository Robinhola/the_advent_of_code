open! Base
open! Core
open! Import

(*
--- Day 3: Crossed Wires ---

The gravity assist was successful, and you're well on your way to the Venus
refuelling station. During the rush back on Earth, the fuel management system
wasn't completely installed, so that's next on the priority list.

Opening the front panel reveals a jumble of wires. Specifically, two wires are
connected to a central port and extend outward on a grid. You trace the path
each wire takes as it leaves the central port, one wire per line of text (your
puzzle input).

The wires twist and turn, but the two wires occasionally cross paths. To fix
the circuit, you need to find the intersection point closest to the central
port. Because the wires are on a grid, use the Manhattan distance for this
measurement. While the wires do technically cross right at the central port
where they both start, this point does not count, nor does a wire count as
crossing with itself.

For example, if the first wire's path is R8,U5,L5,D3, then starting from the
central port (o), it goes right 8, up 5, left 5, and finally down 3:

...........
...........
...........
....+----+.
....|....|.
....|....|.
....|....|.
.........|.
.o-------+.
...........
Then, if the second wire's path is U7,R6,D4,L4, it goes up 7, right 6, down 4,
and left 4:

...........
.+-----+...
.|.....|...
.|..+--X-+.
.|..|..|.|.
.|.-X--+.|.
.|..|....|.
.|.......|.
.o-------+.
...........
These wires cross at two locations (marked X), but the lower-left one is closer
to the central port: its distance is 3 + 3 = 6.

Here are a few more examples:

R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83 = distance 159
R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = distance 135
What is the Manhattan distance from the central port to the closest intersection?
*)

let lines =
  Advent.read_lines "day03" ~for_tests:
    [ "R75,D30,R83,U83,L12,D49,R71,U7,L72"
    ; "U62,R66,U55,R34,D71,R55,D58,R83"
    ] ()
  |> List.map  ~f:(fun line -> String.split ~on:',' line)
;;

(* let lines = [["R8";"U5";"L5";"D3"]; ["U7";"R6";"D4";"L4"]] in  *)

module T : sig
  include Day.T

end = struct
  let name = "--- Day 3: Crossed Wires ---"

  module Coord = struct 
    type t = int * int [@@deriving sexp, compare]
    let from_char c = match c with
    | 'U' -> (0, 1)
    | 'R' -> (1, 0)
    | 'D' -> (0, -1)
    | 'L' -> (-1, 0)
    | _ -> Error.raise_s [%message (c: char)]

    let distance t = 
      let x ,y = t in 
      abs x + abs y 
    ;;
  end

  module CoordMap = Map.Make(Coord) [@@deriving sexp]

  let%expect_test "distance" =
  let t1 = (5, 5) in
  let t2 = (-5, 5) in
  printf "%d %d" (Coord.distance t1) (Coord.distance t2);
  [%expect {| 10 10 |}]

  let rec make_move history position ~direction ~move ~count =
    let px, py = position in
    let dx, dy = direction in 
    match move with 
    | 0 -> history, position, count
    | _ -> (
      let move = move - 1 in 
      let count = count + 1 in 
      let position = px + dx, py + dy in 
      let history = Map.add_multi history ~key:position ~data:count in
      make_move history position ~direction ~move ~count
    )
  ;;

  let rec move_wire history position moves count =
    match moves with 
    | [] -> history
    | m :: moves -> 
      let direction = Coord.from_char (Char.of_string(String.sub m ~pos:0 ~len:1)) in
      let move = Int.of_string (String.sub m ~pos:1 ~len:((String.length m) - 1)) in 
      let history, position, count = make_move history position ~direction ~move ~count in
      move_wire history position moves count;
  ;;

    let find_wire_positions id = 
      let wire_moves = List.nth_exn lines id in 
      let wire_positions  = Map.key_set (move_wire CoordMap.empty (0, 0) wire_moves 0) in
      wire_positions
    ;;
    let intersections = Set.to_list (Set.inter (find_wire_positions 0) (find_wire_positions 1));;
    let closest_intersection = List.min_elt intersections ~compare:(fun a b -> Coord.distance a - Coord.distance b);;

  let part1 = (Option.value_exn closest_intersection |> Coord.distance)

  (* --- Part Two ---
  It turns out that this circuit is very timing-sensitive; you actually need to
  minimize the signal delay.
  
  To do this, calculate the number of steps each wire takes to reach each
  intersection; choose the intersection where the sum of both wires' steps is
  lowest. If a wire visits a position on the grid multiple times, use the steps
  value from the first time it visits that position when calculating the total
  value of a specific intersection.
  
  The number of steps a wire takes is the total number of grid squares the wire
  has entered to get to that location, including the intersection being
  considered. Again consider the example from above:

  ...........
  .+-----+...
  .|.....|...
  .|..+--X-+.
  .|..|..|.|.
  .|.-X--+.|.
  .|..|....|.
  .|.......|.
  .o-------+.
  ...........
  In the above example, the intersection closest to the central port is reached
  after 8+5+5+2 = 20 steps by the first wire and 7+6+4+3 = 20 steps by the
  second wire for a total of 20+20 = 40 steps.
  
  However, the top-right intersection is better: the first wire takes only 8+5+2
  = 15 and the second wire takes only 7+6+2 = 15, a total of 15+15 = 30 steps.
  
  Here are the best steps for the extra examples from above:
  
  R75,D30,R83,U83,L12,D49,R71,U7,L72
  U62,R66,U55,R34,D71,R55,D58,R83 = 610 steps
  R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
  U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = 410 steps

  What is the fewest combined steps the wires must take to reach an
  intersection?
  *)

  let solve_part2 lines = 
    let find_wire_positions id = 
      let wire_moves = List.nth_exn lines id in 
      Map.map
        (move_wire CoordMap.empty (0, 0) wire_moves 0)
        ~f:(fun l -> List.min_elt l ~compare:Int.compare |> Option.value_exn)
    in
    let first = find_wire_positions 0 in 
    let second = find_wire_positions 1 in
    let intersections = Set.to_list (Set.inter (Map.key_set first) (Map.key_set second)) in
    let distance c = (Map.find_exn first c) + (Map.find_exn second c) in
    let compare a b = distance a - distance b in 
    let closest_intersection = List.min_elt intersections ~compare |> Option.value_exn in
    distance closest_intersection
  ;;

  let%expect_test "distance" =
    let lines1 = [
      ["R75";"D30";"R83";"U83";"L12";"D49";"R71";"U7";"L72"];
      ["U62";"R66";"U55";"R34";"D71";"R55";"D58";"R83"]
    ] in
    let lines2 = [
      ["R98";"U47";"R26";"D63";"R33";"U87";"L62";"D20";"R33";"U53";"R51"];
      ["U98";"R91";"D20";"R16";"D67";"R40";"U7";"R15";"U6";"R7"]
    ] in
    printf "%d %d" (solve_part2 lines1) (solve_part2 lines2);
    [%expect {| 610 410 |}]

  let part2 = (solve_part2 lines)

end
