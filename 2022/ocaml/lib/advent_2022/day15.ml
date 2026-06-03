open! Base
open! Core
open! Import

module IntPairs = struct
  type t = int * int [@@deriving sexp, compare]

  let equal left right = 
    let xl, yl = left in
    let xr, yr = right in
    Int.equal xl xr && Int.equal yl yr
  ;;
    
end

module PairsSet = Set.Make(IntPairs)
module PairsMap = Map.Make(IntPairs)

let lines = Advent.read_lines "day15" ~for_tests:
  [ "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
  ; "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
  ; "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
  ; "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
  ; "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
  ; "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
  ; "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
  ; "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
  ; "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
  ; "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
  ; "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
  ; "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
  ; "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
  ; "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
  ] ()
;;

module Input = struct 
  type t = 
    { sensors_to_closest_beacon : int PairsMap.t
    ; beacons : PairsSet.t }
    [@@deriving sexp_of]
  ;;

  let default_t = { sensors_to_closest_beacon = PairsMap.empty; beacons = PairsSet.empty }

  let read_number number remove_last =
    let last_n = match remove_last with
      | true -> String.length number - 1
      | false -> String.length number
    in
    let number = String.slice number 2 last_n in
    Int.of_string number
  ;;

  let compute_distance (x, y) (xb, yb) =
    abs (yb - y) + abs (xb - x)
  ;;

  let read_line line = 
    let line = line |> String.split ~on:' ' |> Array.of_list in
    let x = read_number line.(2) true in
    let y = read_number line.(3) true in
    let xb = read_number line.(8) true in
    let yb = read_number line.(9) false in
    let distance = compute_distance (x, y) (xb, yb) in
    (x, y, xb, yb, distance)
  ;;

  let rec make t = function
    | [] -> t
    | line :: rest ->
      let x, y, xb, yb, distance = read_line line in
      let sensors_to_closest_beacon = Map.set
        t.sensors_to_closest_beacon
        ~key:(x, y)
        ~data:distance
      in
      let beacons = PairsSet.add
        t.beacons
        (xb, yb)
      in
      make { sensors_to_closest_beacon; beacons } rest
  ;;

  (* let find_x_bounds t =
    let all_x = t.sensors_to_closest_beacon
      |> Map.keys 
      |> List.map ~f:(fun (x, _) -> x)
    in
    let smallest = List.max_elt all_x ~compare:(fun a b -> a - b) in 
    let biggest = List.max_elt all_x ~compare:(fun a b -> b - a) in 
    smallest, biggest
  ;; *)

  let how_many_at (_, y) max_distance goaly = 
    let distance_from_closest = abs (goaly - y) in
    let over_the_distance = (max_distance - distance_from_closest) in
    over_the_distance
  ;;

  let rec make_line l ~x ~n ~current = 
    match n - current with
    | 0 -> l
    | d when Int.is_negative d -> failwith "impossible"
    | _ -> make_line ( x + current :: x - current :: l) ~x ~n ~current:(current + 1)
  ;;

  let just_all_t t goaly = 
    t.sensors_to_closest_beacon
    |> Map.to_alist
    |> List.map ~f:(
      fun ((x, y), distance) ->
        let n = how_many_at (x, y) distance goaly in
        if n < 0 then Int.Set.empty
        else
          let n = n + 1 in
          make_line [ x ] ~x ~n ~current:0
          |> Int.Set.of_list)
    |> Int.Set.union_list
  ;;

  let for_all_t t goaly =
    let all_t = just_all_t t goaly in 
    let all_sensors =
      t.beacons
      |> Set.to_list
      |> List.filter ~f:(fun (_, y) -> y |> Int.equal goaly)
      |> List.map ~f:(fun (x, _) -> x)
      |> Int.Set.of_list
    in
    Int.Set.diff all_t all_sensors
  ;;

  let%expect_test "" =
    let t = make default_t lines in
    let all_t = for_all_t t 10 in 
    (* print_s [%message (all_t : Int.Set.t)]; *)
    print_s [%message (Set.length all_t : int)];
    [%expect {|
      ("Set.length all_t" 26) |}]
  ;;

  let find_middle (x, y) (xb, yb) =
    abs (x - xb) / 2, abs (y - yb) / 2
  ;;

  let%expect_test "" = 
    print_s [%message (find_middle (0, 0) (6, 0): IntPairs.t)];
    print_s [%message (find_middle (0, 6) (0, 0): IntPairs.t)];
    print_s [%message (find_middle (0, 6) (6, 0): IntPairs.t)];
    print_s [%message (find_middle (0, 0) (3, 6): IntPairs.t)];
    [%expect {|
      ("find_middle (0, 0) (6, 0)" (3 0))
      ("find_middle (0, 6) (0, 0)" (0 3))
      ("find_middle (0, 6) (6, 0)" (3 3))
      ("find_middle (0, 0) (3, 6)" (1 3)) |}]
  ;;

  let rec find_almost_touching t results = function
    | _ :: []
    | [] -> results
    | reference :: rest ->
        let max_d = Map.find_exn t.sensors_to_closest_beacon reference in
        let touch_distances = rest
        |> List.map ~f:(fun right -> 
          let right_d = Map.find_exn t.sensors_to_closest_beacon right in
          let distance = compute_distance reference right in
          let touch_distance = distance - (right_d + max_d) in 
          right, touch_distance)
        in
        let result = 
          touch_distances
          |> List.find ~f:(fun (_, d) -> Int.equal d 1)
          |> Option.map ~f:(fun (right, _) -> reference, right)
        in
        let results = 
          if Option.is_some result then (Option.value_exn result) :: results
          else results
        in find_almost_touching t results rest
  ;;

  let rec find_surrounding_points_ (x, y) distance current result = 
    let xp = x + distance - current in
    let yp = y + current in
    let xp2 = x - distance - current in
    let yp2 = y - current in
    let result = (xp2, yp2) :: (xp, yp) :: result in
    if Int.equal current distance then PairsSet.of_list result
    else find_surrounding_points_ (x, y) distance (current + 1) result
  ;;


  let find_surrounding_points (x, y) t = 
    let distance = Map.find_exn t.sensors_to_closest_beacon (x, y) + 1 in
    find_surrounding_points_ (x, y) distance 0 []
  ;;

  let%expect_test "" = 
    print_s [%message (find_surrounding_points_ (5, 5) 2 0 [] : PairsSet.t)];
    [%expect {|
      ("find_surrounding_points_ (5, 5) 2 0 []"
       ((1 3) (2 4) (3 5) (5 7) (6 6) (7 5))) |}]
  ;;

  let rec is_not_seen (x, y) = function
    | [] -> true
    | ((xb, yb), d) :: rest -> 
      let distance = compute_distance (x, y) (xb, yb) in
      if distance <= d then false
      else is_not_seen (x, y) rest
  ;;

  let%expect_test "" = 
    let t = make default_t lines in
    let points = find_almost_touching t [] (Map.keys t.sensors_to_closest_beacon) in
    let surrounding = List.map points ~f:(
      fun (a, b) ->
        [ find_surrounding_points a t
        ; find_surrounding_points b t ]
        |> PairsSet.union_list)
      |> PairsSet.union_list
      |> PairsSet.to_list
      |> List.filter ~f:(fun (x, y) -> 
        0 < x && x < 20 &&
        0 < y && y < 20
        )
    in
    let sensors = Map.to_alist t.sensors_to_closest_beacon in
    let is_not_seen = surrounding
      |> List.filter ~f:(fun (x, y) -> is_not_seen (x, y) sensors)
    in
    print_s [%message (points : ((IntPairs.t) * (IntPairs.t)) list )];
    print_s [%message (is_not_seen : IntPairs.t list )];
    [%expect {|
      (points (((12 14) (17 20)) ((2 18) (8 7)) ((2 0) (2 18))))
      (is_not_seen ((14 11))) |}]
  ;;

  let is_seen_by (xs, ys) max_distance (x, y) =
    let distance = compute_distance (x, y) (xs, ys) in
    distance <= max_distance 
  ;;

  let%expect_test "" = 
    print_s [%message (is_seen_by (5, 5) 5 (0, 5): bool)];
    print_s [%message (is_seen_by (5, 5) 4 (0, 5): bool)];
    [%expect {|
      ("is_seen_by (5, 5) 5 (0, 5)" true)
      ("is_seen_by (5, 5) 4 (0, 5)" false) |}]
  ;;

  let rec all_true = function
    | [] -> true 
    | x :: rest -> x && all_true rest
  ;;

  let%expect_test "" = 
    print_s [%message (all_true [true; true; true]: bool)];
    print_s [%message (all_true [false; true; true]: bool)];
    print_s [%message (all_true [true; true; false]: bool)];
    [%expect {|
      ("all_true [true; true; true]" true)
      ("all_true [false; true; true]" false)
      ("all_true [true; true; false]" false) |}]
  ;;

  let rec is_rectangle_fully_included top_left size = function 
    | [] -> false
    | (sensor, max_distance) :: rest -> 
      let x, y = top_left in
      let is_included =
        [ x, y
        ; x + size - 1, y
        ; x + size - 1, y + size - 1
        ; x, y + size - 1]
        |> List.map ~f:(is_seen_by sensor max_distance)
        |> all_true
      in
      if is_included then (
        (* print_s [%message (top_left : int * int)];
        print_s [%message (size : int)];
        print_s [%message (sensor : int * int)];
        print_s [%message (max_distance : int)]; *)
        true)
      else is_rectangle_fully_included top_left size rest
  ;;

  let%expect_test "" =
    let t = make default_t lines in
    let rest = Map.to_alist t.sensors_to_closest_beacon in
    print_s [%message (is_rectangle_fully_included (4, 2) 5 rest : bool)];
    print_s [%message (is_rectangle_fully_included (4, 2) 15 rest : bool)];
    print_s [%message (is_rectangle_fully_included (14, 11) 1 rest : bool)];
    print_s [%message (is_rectangle_fully_included (13, 11) 1 rest : bool)];
    print_s [%message (is_rectangle_fully_included (14, 12) 1 rest : bool)];
    [%expect {|
      ("is_rectangle_fully_included (4, 2) 5 rest" true)
      ("is_rectangle_fully_included (4, 2) 15 rest" false)
      ("is_rectangle_fully_included (14, 11) 1 rest" false)
      ("is_rectangle_fully_included (13, 11) 1 rest" true)
      ("is_rectangle_fully_included (14, 12) 1 rest" true) |}]
  ;;

  let rec find_the_bad_spot t size top_left = 
    (* print_s [%message (top_left: IntPairs.t) (size : int)]; *)
    if Int.equal size 0 ||
      is_rectangle_fully_included top_left size (Map.to_alist t.sensors_to_closest_beacon)
    then (
      (* print_s [%message (top_left: IntPairs.t) (size : int) (false : bool)]; *)
      None)
    else (
      let size = max (size / 2) 1 in
      let x, y = top_left in
      let top_middle = x + size - 1, y in
      let bot_middle = x + size - 1, y + size - 1 in
      let bot_left = x, y + size - 1 in
      [ top_left; top_middle; bot_left; bot_middle ] 
      |> List.map ~f:(find_the_bad_spot t size)
      |> List.filter ~f:Option.is_some
      |> List.map ~f:(fun x -> Option.value_exn x)
      |> List.hd)
  ;;
end

module T : sig
  include Day.T
end = struct
  let name = "--- Day 15: Beacon Exclusion Zone ---"

  let part1_ goaly = 
    let open Input in 
    let t = make default_t lines in
    let all_t = for_all_t t goaly in 
    Set.length all_t 
  ;;

  let part1 = 
    (* Not optimal 😔 *)
    (* part1_ 2000000 *)
    5688618
  ;;

  let part2_ tuning_offset =
    let open Input in
    let t = make default_t lines in
    let from = (Map.to_alist t.sensors_to_closest_beacon) in
    let rec test_me = function
      | [] -> failwith "didnt work"
      | p :: rest -> 
        let points = find_surrounding_points p t in
        let unseen = Set.to_list points
          |> List.filter ~f:(fun (x, y) -> 0 <= x && x < tuning_offset && 0 <= y && y <= tuning_offset)
          |> List.filter ~f:(fun p -> is_not_seen p from)
        in
        match List.hd unseen with
          | Some match_ -> match_
          | None -> test_me rest
    in
    
    (* sort by size  *)
    (* look at extremeties *)
    (* test  *)
    let is_not_seen = test_me (Map.keys t.sensors_to_closest_beacon) in
    let x, y = is_not_seen in
    x * 4000000 + y
  ;;

  let part2 = 
    (* 0 *)
    (* Not optimal 😔 *)
    (* part2_ 4000000 *)
    12625383204261
  ;;

  let%expect_test "" =
    print_s [%message (part1_ 10 : int)];
    print_s [%message (part2_ 20 : int)];
    [%expect{|
      ("part1_ 10" 26)
      ("part2_ 20" 56000011) |}]
  ;;
end