open! Base
open! Core

let example =
  {|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4|}
;;

type range' =
  { start : int
  ; end_ : int
  }
[@@deriving sexp]

type range =
  { source : range'
  ; destination : range'
  }
[@@deriving sexp]

let to_range' start length : range' = { start; end_ = start + length }

type t =
  { seeds : int list
  ; seed_range : range list
  ; seed_to_soil : range list
  ; soil_to_fertilizer : range list
  ; fertilizer_to_water : range list
  ; water_to_light : range list
  ; light_to_temperature : range list
  ; temperature_to_humidity : range list
  ; humidity_to_location : range list
  }
[@@deriving sexp]

module Parse = struct
  let parse_range = function
    | [ destination; source; length ] ->
      { source = to_range' source length; destination = to_range' destination length }
    | _ -> failwith "Invalid range"
  ;;

  let parse_raw_range raw_range =
    raw_range |> String.split ~on:' ' |> List.map ~f:Int.of_string |> parse_range
  ;;

  let parse_ranges = function
    | _name :: ranges -> List.map ranges ~f:parse_raw_range
    | _ -> failwith "Invalid ranges"
  ;;

  let parse_seeds raw_seeds =
    let pattern = String.Search_pattern.create ": " in
    match String.Search_pattern.split_on pattern raw_seeds with
    | [ _; seed_numbers ] -> List.map (String.split ~on:' ' seed_numbers) ~f:Int.of_string
    | _ -> failwith "Invalid seeds"
  ;;

  let rec parse_seed_range' (current : range List.t) = function
    | [] -> current
    | source :: length :: rest ->
      parse_seed_range'
        ({ source = to_range' source length; destination = to_range' source length }
         :: current)
        rest
    | _ -> failwith "Bad seeds"
  ;;

  let sort_ranges_by_destination =
    List.sort ~compare:(fun left right ->
      Int.compare left.destination.start right.destination.start)
  ;;

  (* let sort_ranges_by_source = *)
  (*   List.sort ~compare:(fun left right -> Int.compare left.source right.source) *)
  (* ;; *)

  let make_continuous (ranges : range list) =
    let sorted_by_source =
      ranges
      |> List.sort ~compare:(fun left right ->
        Int.compare left.source.start right.source.start)
    in
    let _, ranges =
      List.fold sorted_by_source ~init:(0, []) ~f:(fun (current_start, current) range ->
        let new_start = range.source.end_ + 1 in
        let current =
          let current = range :: current in
          match current_start >= range.source.start with
          | true -> current
          | false ->
            let length = range.source.start - 1 - current_start in
            { source = to_range' current_start length
            ; destination = to_range' current_start length
            }
            :: current
        in
        new_start, current)
    in
    let latest_element =
      List.fold ranges ~init:0 ~f:(fun acc { source; _ } -> max acc source.end_)
    in
    { source = to_range' latest_element 1000000
    ; destination = to_range' latest_element 1000000
    }
    :: ranges
  ;;

  let parse_seed_range seeds = parse_seed_range' [] seeds

  let parse data =
    let pattern = String.Search_pattern.create "\n\n" in
    match String.Search_pattern.split_on pattern data with
    | [ seeds
      ; seed_to_soil
      ; soil_to_fertilizer
      ; fertilizer_to_water
      ; water_to_light
      ; light_to_temperature
      ; temperature_to_humidity
      ; humidity_to_location
      ] ->
      let s = Fn.compose sort_ranges_by_destination make_continuous in
      let seeds = parse_seeds seeds in
      { seeds
      ; seed_range = parse_seed_range seeds
      ; seed_to_soil = parse_ranges (String.split_lines seed_to_soil) |> s
      ; soil_to_fertilizer = parse_ranges (String.split_lines soil_to_fertilizer) |> s
      ; fertilizer_to_water = parse_ranges (String.split_lines fertilizer_to_water) |> s
      ; water_to_light = parse_ranges (String.split_lines water_to_light) |> s
      ; light_to_temperature = parse_ranges (String.split_lines light_to_temperature) |> s
      ; temperature_to_humidity =
          parse_ranges (String.split_lines temperature_to_humidity) |> s
      ; humidity_to_location = parse_ranges (String.split_lines humidity_to_location) |> s
      }
    | _ -> failwith "Invalid input"
  ;;

  let%expect_test _ =
    let t = parse example in
    print_s [%message (t : t)];
    [%expect
      {|
    (t
     ((seeds (79 14 55 13))
      (seed_range
       (((source ((start 55) (end_ 68))) (destination ((start 55) (end_ 68))))
        ((source ((start 79) (end_ 93))) (destination ((start 79) (end_ 93))))))
      (seed_to_soil
       (((source ((start 0) (end_ 49))) (destination ((start 0) (end_ 49))))
        ((source ((start 98) (end_ 100))) (destination ((start 50) (end_ 52))))
        ((source ((start 50) (end_ 98))) (destination ((start 52) (end_ 100))))
        ((source ((start 100) (end_ 1000100)))
         (destination ((start 100) (end_ 1000100))))))
      (soil_to_fertilizer
       (((source ((start 15) (end_ 52))) (destination ((start 0) (end_ 37))))
        ((source ((start 52) (end_ 54))) (destination ((start 37) (end_ 39))))
        ((source ((start 0) (end_ 15))) (destination ((start 39) (end_ 54))))
        ((source ((start 54) (end_ 1000054)))
         (destination ((start 54) (end_ 1000054))))))
      (fertilizer_to_water
       (((source ((start 11) (end_ 53))) (destination ((start 0) (end_ 42))))
        ((source ((start 0) (end_ 7))) (destination ((start 42) (end_ 49))))
        ((source ((start 53) (end_ 61))) (destination ((start 49) (end_ 57))))
        ((source ((start 7) (end_ 11))) (destination ((start 57) (end_ 61))))
        ((source ((start 61) (end_ 1000061)))
         (destination ((start 61) (end_ 1000061))))))
      (water_to_light
       (((source ((start 0) (end_ 17))) (destination ((start 0) (end_ 17))))
        ((source ((start 25) (end_ 95))) (destination ((start 18) (end_ 88))))
        ((source ((start 18) (end_ 25))) (destination ((start 88) (end_ 95))))
        ((source ((start 95) (end_ 1000095)))
         (destination ((start 95) (end_ 1000095))))))
      (light_to_temperature
       (((source ((start 0) (end_ 44))) (destination ((start 0) (end_ 44))))
        ((source ((start 77) (end_ 100))) (destination ((start 45) (end_ 68))))
        ((source ((start 64) (end_ 77))) (destination ((start 68) (end_ 81))))
        ((source ((start 45) (end_ 64))) (destination ((start 81) (end_ 100))))
        ((source ((start 100) (end_ 1000100)))
         (destination ((start 100) (end_ 1000100))))))
      (temperature_to_humidity
       (((source ((start 69) (end_ 70))) (destination ((start 0) (end_ 1))))
        ((source ((start 0) (end_ 69))) (destination ((start 1) (end_ 70))))
        ((source ((start 70) (end_ 1000070)))
         (destination ((start 70) (end_ 1000070))))))
      (humidity_to_location
       (((source ((start 0) (end_ 55))) (destination ((start 0) (end_ 55))))
        ((source ((start 93) (end_ 97))) (destination ((start 56) (end_ 60))))
        ((source ((start 56) (end_ 93))) (destination ((start 60) (end_ 97))))
        ((source ((start 97) (end_ 1000097)))
         (destination ((start 97) (end_ 1000097)))))))) |}]
  ;;
end

let translate (ranges : range list) number : int =
  List.fold_until
    ranges
    ~init:()
    ~f:(fun _ range ->
      let start = range.source.start in
      let end_ = range.source.end_ in
      match start <= number, number <= end_ with
      | true, true ->
        let offset = number - range.source.start in
        Stop (range.destination.start + offset)
      | _ -> Continue ())
    ~finish:(Fn.const number)
;;

let solve1' t seed =
  [ t.seed_to_soil
  ; t.soil_to_fertilizer
  ; t.fertilizer_to_water
  ; t.water_to_light
  ; t.light_to_temperature
  ; t.temperature_to_humidity
  ; t.humidity_to_location
  ]
  |> List.fold ~init:seed ~f:(fun number ranges -> translate ranges number)
;;

let part1 data =
  let t = Parse.parse data in
  t.seeds
  |> List.map ~f:(solve1' t)
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn
;;

let how_to_reach_arrival (arrival : range) (ranges : range list) : range list =
  let ranges = Parse.sort_ranges_by_destination ranges in
  let target = arrival.source in
  List.fold ~init:[] ranges ~f:(fun current range ->
    let range' = range.destination in
    let start = max target.start range'.start in
    let end_ = min target.end_ range'.end_ in
    if start < end_
    then (
      let offset = start - range.destination.start in
      let length = end_ - start in
      let source = to_range' (range.source.start + offset) length in
      let destination = to_range' start length in
      { source; destination } :: current)
    else current)
  |> Parse.sort_ranges_by_destination
;;

let rec depth_first_search (range : range) = function
  | [] -> Some range
  | next_step :: steps ->
    let how_to_reach =
      how_to_reach_arrival range next_step |> Parse.sort_ranges_by_destination
    in
    List.fold_until
      how_to_reach
      ~init:None
      ~f:(fun _ range ->
        match depth_first_search range steps with
        | Some result -> Continue_or_stop.Stop (Some result)
        | None -> Continue_or_stop.Continue None)
      ~finish:(Fn.const None)
;;

let part2 data =
  let t = Parse.parse data in
  let steps =
    [ t.temperature_to_humidity
    ; t.light_to_temperature
    ; t.water_to_light
    ; t.fertilizer_to_water
    ; t.soil_to_fertilizer
    ; t.seed_to_soil
    ; t.seed_range
    ]
  in
  let result = depth_first_search (List.hd_exn t.humidity_to_location) steps in
  let range = result |> Option.value_exn in
  solve1' t range.source.start
;;

let%expect_test _ =
  let t = Parse.parse example in
  let arrival = List.hd_exn t.humidity_to_location in
  let result = part2 example in
  print_s
    [%message
      (t.seed_range : range list) (arrival : range) (result : int) (solve1' t 82 : int)];
  [%expect
    {|
    ((t.seed_range
      (((source ((start 55) (end_ 68))) (destination ((start 55) (end_ 68))))
       ((source ((start 79) (end_ 93))) (destination ((start 79) (end_ 93))))))
     (arrival
      ((source ((start 0) (end_ 55))) (destination ((start 0) (end_ 55)))))
     (result 46) ("solve1' t 82" 46)) |}]
;;

let%expect_test _ =
  print_s [%message (part1 example : int)];
  print_s [%message (part1 Input.data : int)];
  print_s [%message (part2 example : int)];
  [%expect
    {|
    ("part1 example" 35)
    ("part1 Input.data" 313045984)
    ("part2 example" 46) |}]
;;

let solve () =
  print_endline [%string "part | example"];
  print_endline [%string "1    | %{part1 example#Int}"];
  print_endline [%string "2    | %{part2 example#Int}"];
  print_endline [%string "---------------"];
  print_endline [%string "part | solution"];
  print_endline [%string "1    | %{part1 Input.data#Int}"];
  print_endline [%string "2    | %{part2 Input.data#Int}"]
;;

(* Part 2 ->

   make all ranges continous
   Depth First Search

   0 -> N
*)
let%expect_test _ =
  let t = Parse.parse example in
  let range = { source = to_range' 45 2; destination = to_range' 45 2 } in
  let how_to_reach =
    how_to_reach_arrival range t.humidity_to_location |> Parse.sort_ranges_by_destination
  in
  print_s [%message (range : range) (how_to_reach : range list)];
  [%expect
    {|
    ((range
      ((source ((start 45) (end_ 47))) (destination ((start 45) (end_ 47)))))
     (how_to_reach
      (((source ((start 45) (end_ 47))) (destination ((start 45) (end_ 47))))))) |}]
;;
