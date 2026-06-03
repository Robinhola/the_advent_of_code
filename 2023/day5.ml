open! Base
open! Core

(* CR relrharbi-fleury: remove me *)
[@@@disable_unused_warnings]

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

type range =
  { source : int
  ; destination : int
  ; length : int
  }
[@@deriving sexp]

type t =
  { seeds : int list
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
    | [ destination; source; length ] -> { source; destination; length }
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
      { seeds = parse_seeds seeds
      ; seed_to_soil = parse_ranges (String.split_lines seed_to_soil)
      ; soil_to_fertilizer = parse_ranges (String.split_lines soil_to_fertilizer)
      ; fertilizer_to_water = parse_ranges (String.split_lines fertilizer_to_water)
      ; water_to_light = parse_ranges (String.split_lines water_to_light)
      ; light_to_temperature = parse_ranges (String.split_lines light_to_temperature)
      ; temperature_to_humidity =
          parse_ranges (String.split_lines temperature_to_humidity)
      ; humidity_to_location = parse_ranges (String.split_lines humidity_to_location)
      }
    | _ -> failwith "Invalid input"
  ;;

  let%expect_test _ =
    print_s [%message (parse example : t)];
    [%expect
      {|
    ("parse example"
     ((seeds (79 14 55 13))
      (seed_to_soil
       (((source 98) (destination 50) (length 2))
        ((source 50) (destination 52) (length 48))))
      (soil_to_fertilizer
       (((source 15) (destination 0) (length 37))
        ((source 52) (destination 37) (length 2))
        ((source 0) (destination 39) (length 15))))
      (fertilizer_to_water
       (((source 53) (destination 49) (length 8))
        ((source 11) (destination 0) (length 42))
        ((source 0) (destination 42) (length 7))
        ((source 7) (destination 57) (length 4))))
      (water_to_light
       (((source 18) (destination 88) (length 7))
        ((source 25) (destination 18) (length 70))))
      (light_to_temperature
       (((source 77) (destination 45) (length 23))
        ((source 45) (destination 81) (length 19))
        ((source 64) (destination 68) (length 13))))
      (temperature_to_humidity
       (((source 69) (destination 0) (length 1))
        ((source 0) (destination 1) (length 69))))
      (humidity_to_location
       (((source 56) (destination 60) (length 37))
        ((source 93) (destination 56) (length 4)))))) |}]
  ;;
end

let translate (ranges : range list) number : int =
  List.fold_until
    ranges
    ~init:()
    ~f:(fun _ range ->
      let start = range.source in
      let end_ = range.source + range.length in
      match start <= number, number <= end_ with
      | true, true ->
        let offset = number - range.source in
        Stop (range.destination + offset)
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

let solve1 data =
  let t = Parse.parse data in
  t.seeds
  |> List.map ~f:(solve1' t)
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn
;;

let%expect_test _ =
  print_s [%message (solve1 example : int)];
  print_s [%message (solve1 Advent_2023_04_input.data : int)];
  [%expect
    {|
    ("solve1 example" 35)
    ("solve1 Advent_2023_04_input.data" 313045984) |}]
;;

let solve =
  Command.basic
    ~summary:"Solve 2023-12-03"
    (let%map_open.Command () = return () in
     fun () ->
       let part1 = 1 in
       Ascii_table.simple_list_table
         [ "part"; "solution" ]
         [ [ "1"; Int.to_string part1 ] ])
;;
