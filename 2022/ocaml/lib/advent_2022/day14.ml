open! Base
open! Core
open! Import

let max_x_cst = 1000
let max_y_cst = 500

module IntPairs = struct
  type t = int * int [@@deriving sexp, compare]

  let equal left right = 
    let xl, yl = left in
    let xr, yr = right in
    Int.equal xl xr && Int.equal yl yr
  ;;
    
end

module PairsSet = Set.Make(IntPairs)

let lines = Advent.read_lines "day14" ~for_tests:
  [ "498,4 -> 498,6 -> 496,6"
  ; "503,4 -> 502,4 -> 502,9 -> 494,9" ] ()
;;

module Path_coordinates = struct
  type t =
    { x : int
    ; y : int }
    [@@deriving sexp_of]
  ;;

  let make line = 
    match String.split line ~on:',' with
    | x :: y :: [] ->
      { x = (Int.of_string x)
      ; y = (Int.of_string y)
      }
    | _ -> failwith "wrong format"
  ;;

  let equal left right = 
    Int.equal left.x right.x && Int.equal left.y right.y
  ;;
end

module Input = struct
  type t =
    { grid : char array array 
    ; end_ : int
    } [@@deriving sexp_of]
  ;;

  let rec draw_single_path grid (start: Path_coordinates.t) (end_: Path_coordinates.t) = 
    grid.(start.y).(start.x) <- '#';
    match Path_coordinates.equal start end_ with
      | true -> ()
      | false ->
        let x = 
          match end_.x - start.x with
          | n when Int.is_positive n -> start.x + 1
          | n when Int.is_negative n -> start.x - 1
          | _ -> start.x
        in
        let y = 
          match end_.y - start.y with
          | n when Int.is_positive n -> start.y + 1
          | n when Int.is_negative n -> start.y - 1
          | _ -> start.y
        in
        draw_single_path grid { x ; y } end_
  ;;

  let rec draw grid = function
    | _ :: [] -> ()
    | start :: end_ :: rest ->
      draw_single_path grid start end_;
      draw grid (end_ :: rest)
    | _ -> failwith "impossible"
  ;;

  let make lines = 
    let grid = Array.make_matrix ~dimy:max_x_cst ~dimx:max_y_cst '.' in
    let max_y = ref 0 in
    lines
    |> List.iter ~f:(fun line ->
      let paths = 
        line
        |> String.split ~on:' ' 
        |> List.filter ~f:(fun l -> not (String.equal "->" l))
        |> List.map ~f:Path_coordinates.make
      in
      let local_max = paths
        |> List.max_elt ~compare:(fun left right -> left.y - right.y)
        |> Option.map ~f:(fun p -> p.y)
        |> Option.value_exn
      in
      max_y := max local_max !max_y;
      paths |> draw grid
    );
    { grid; end_ = !max_y + 1 }
  ;;

  let add_floor t =
    let y = t.end_ + 1 in
    draw t.grid [ { x = 0; y }; {x = max_x_cst - 1; y }]
  ;;

  let display screen = 
    Array.iter screen ~f:(fun line -> 
      let line = line |> Array.to_list |> String.of_char_list in
      print_s [%sexp (line : string)]
    )
  ;;

  let rec update t (sand: Path_coordinates.t) = 
    let grid = t.grid in
    let x, y = sand.x, sand.y in
    if Int.equal y (t.end_ - 1) then Path_coordinates.{ x ; y = t.end_ }
    else
    match grid.(y + 1).(x), grid.(y + 1).(x - 1), grid.(y + 1).(x + 1) with
      | '.', _, _ -> update t { y = y + 1; x }
      | _, '.', _ -> update t { y = y + 1; x = x - 1 }
      | _, _, '.' -> update t { y = y + 1; x = x + 1 }
      | _ -> sand
  ;;
    
  let rec update_part2 t (sand: Path_coordinates.t) = 
    let grid = t.grid in
    let x, y = sand.x, sand.y in
    match grid.(y + 1).(x), grid.(y + 1).(x - 1), grid.(y + 1).(x + 1) with
      | '.', _, _ -> update_part2 t { y = y + 1; x }
      | _, '.', _ -> update_part2 t { y = y + 1; x = x - 1 }
      | _, _, '.' -> update_part2 t { y = y + 1; x = x + 1 }
      | _ -> sand
  ;;
    
  let rec make_sand_fall t count =
    let sand = update t { x = 500; y = 0} in
    match sand.y with
      | n when Int.equal n t.end_ -> count
      | _ ->
        t.grid.(sand.y).(sand.x) <- 'o';
        make_sand_fall t (count + 1)
    ;;

  let%expect_test "" =
    let t = make lines in
    let count = make_sand_fall t 0 in
    print_s [%message (count : int )];
    [%expect {| (count 24) |}]
  ;;

  let rec make_sand_fall_part2 t count =
    let sand = update_part2 t { x = 500; y = 0} in
    match sand.x, sand.y with
      | 500, 0 -> count
      | _ ->
        t.grid.(sand.y).(sand.x) <- 'o';
        make_sand_fall_part2 t (count + 1)
    ;;


end


module T : sig
  include Day.T
end = struct
  let name = "--- Day 14: Regolith Reservoir ---"

  let input = Input.make lines

  let part1 = Input.make_sand_fall input 0

  let part2_ () = 
    let input_part2 = Input.make lines in 
    Input.add_floor input_part2;
    (Input.make_sand_fall_part2 input_part2 0) + 1
  ;;

  let part2 = part2_ ()

  let%expect_test "" = 
    print_s [%message (part1 : int) (part2_ () : int)];
    [%expect {| ((part1 24) ("part2_ ()" 93)) |}]
  ;;
end