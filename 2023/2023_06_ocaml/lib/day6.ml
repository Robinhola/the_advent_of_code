open! Base
open! Core

[@@@disable_unused_warning]

let example = {|Time:      7  15   30
Distance:  9  40  200|}

let data = {|Time:        61     67     75     71
Distance:   430   1036   1307   1150|}

type race =
  { time : int
  ; previous_record : int
  }
[@@deriving sexp]

module Parse = struct
  let sanitize line =
    match String.split ~on:':' line with
    | [ _name; number_part ] ->
      String.split ~on:' ' number_part |> List.filter ~f:(Fn.non String.is_empty)
    | _ -> failwith "bad line"
  ;;

  let parse data : race list =
    match
      String.split_lines data
      |> List.map ~f:sanitize
      |> List.map ~f:(List.map ~f:Int.of_string)
    with
    | [ time; distance ] ->
      List.map2_exn time distance ~f:(fun time previous_record ->
        { time; previous_record })
    | _ -> failwith "wrong data"
  ;;

  let parse' data : race =
    match
      String.split_lines data
      |> List.map ~f:sanitize
      |> List.map ~f:String.concat
      |> List.map ~f:Int.of_string
    with
    | [ time; previous_record ] -> { time; previous_record }
    | _ -> failwith "wrong data"
  ;;

  let%expect_test _ =
    print_s [%message (parse example : race list)];
    print_s [%message (parse' example : race)];
    [%expect
      {|
      ("parse example"
       (((time 7) (previous_record 9)) ((time 15) (previous_record 40))
        ((time 30) (previous_record 200))))
      ("parse' example" ((time 71530) (previous_record 940200))) |}]
  ;;
end

let compute ~total_time time =
  let acceleration = 1 in
  let speed = acceleration * time in
  let time_left = total_time - time in
  speed * time_left
;;

let solve1' race =
  Sequence.range 1 (race.time - 1)
  |> Sequence.map ~f:(compute ~total_time:race.time)
  |> Sequence.filter ~f:(fun distance -> distance > race.previous_record)
  |> Sequence.length
;;

let solve1 data =
  let races = Parse.parse data in
  let numbers_of_ways_to_win = List.map races ~f:solve1' in
  List.fold numbers_of_ways_to_win ~init:1 ~f:Int.( * )
;;

let solve2 data =
  let race = Parse.parse' data in
  solve1' race
;;

let%expect_test _ =
  print_s [%message (solve1 example : int)];
  print_s [%message (solve1 data : int)];
  print_s [%message (solve2 example : int)];
  print_s [%message (solve2 data : int)];
  [%expect
    {|
    ("solve1 example" 288)
    ("solve1 data" 316800)
    ("solve2 example" 71503)
    ("solve2 data" 45647654) |}]
;;

let solve () = print_endline "Oops"
