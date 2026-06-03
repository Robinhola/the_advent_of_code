open! Base
open! Core
open! Import

module Orbiting_object = struct
  type t =
  { name : string
  ; is_orbited_by : string list
  ; orbit_around : string list
  } [@@deriving sexp_of]

  let default name = { name; is_orbited_by = []; orbit_around = [] }

  let get_or_create orbiting_objects name =
    (* print_s [%message (name: string) (orbiting_objects: t String.Map.t)]; *)
    Map.find orbiting_objects name
    |> Option.value ~default:(default name)
  ;;

  let rec add_to orbiting_objects = function
    | [] -> orbiting_objects
    | o :: rest ->
      let orbiting_objects = Map.set orbiting_objects ~key:o.name ~data:o in
      add_to orbiting_objects rest
  ;;

  let add_to_is_orbited_by orbiting_objects ~name ~by =
    let t = get_or_create orbiting_objects name in
    let t = { t with is_orbited_by = by :: t.is_orbited_by} in
    let by = get_or_create orbiting_objects by in
    let by = { by with orbit_around = name :: by.is_orbited_by} in
    add_to orbiting_objects [ t; by ]
  ;;

  let add_to_orbit_around orbiting_objects ~name ~around =
    let t = get_or_create orbiting_objects name in
    let t = { t with orbit_around = around :: t.orbit_around} in
    let around = get_or_create orbiting_objects around in
    let around = { around with orbit_around = name :: around.orbit_around} in
    add_to orbiting_objects [ t; around ]
  ;;

  let update orbiting_objects line =
    match line |> String.split ~on:')' with
    | heavy :: light :: [] ->
      orbiting_objects
      |> add_to_is_orbited_by ~name:heavy ~by:light
      (* |> add_to_orbit_around ~name:light ~around:heavy *)
    | _ -> failwith "unsupported"
  ;;

  let rec count orbiting_objects distance name =
    (* print_s [%message (name : string)]; *)
    let t = get_or_create orbiting_objects name in
    match t.is_orbited_by with
    | [] -> distance
    | satellites -> satellites |>
      List.fold ~init:distance ~f:(fun accum s ->
        let distance = distance + 1 in
        accum + count orbiting_objects distance s
      )
  ;;

  let breath_first_search orbiting_objects ~start ~goal =
    let next_to_visit element =
      let element = Map.find_exn orbiting_objects element in
      element.orbit_around @ element.is_orbited_by
    in
    let was_not_in seen element = seen
      |> Set.exists ~f:(String.equal element)
      |> not
    in
    let to_visit = next_to_visit start
      |> List.map ~f:(fun e -> e, 0)
      |> Queue.of_list
    in
    let update_to_visit distance next_elements =
      next_elements
      |> List.map ~f:(fun e -> e, distance + 1)
      |> List.iter ~f:(Queue.enqueue to_visit);
    in
    let rec look_for ~seen =
      let element = Queue.dequeue to_visit in
      match element with
      | None -> failwith "could not find"
      | Some (element, distance) when String.equal element goal -> distance - 1
      | Some (element, distance) ->
        let next_elements = next_to_visit element
          |> List.filter ~f:(was_not_in seen)
        in
        next_elements |> update_to_visit distance;
        look_for ~seen:(Set.union seen (String.Set.of_list next_elements))
    in
    look_for ~seen:(String.Set.of_list ["YOU"])
  ;;
end

let lines = Advent.read_lines "day06" ~for_tests:
  [ "COM)B"
  ; "B)C"
  ; "C)D"
  ; "D)E"
  ; "E)F"
  ; "B)G"
  ; "G)H"
  ; "D)I"
  ; "E)J"
  ; "J)K"
  ; "K)L"
  ; "K)YOU"
  ; "I)SAN"
  ] ()

let rec read_lines orbiting_objects = function
  | [] -> orbiting_objects
  | line :: rest ->
    let orbiting_objects = Orbiting_object.update orbiting_objects line in
    rest |> read_lines orbiting_objects
;;

module T : sig
  include Day.T
end = struct
  let name = "--- Day 6: Universal Orbit Map ---"

  let orbiting_objects = read_lines String.Map.empty lines

  let part1 = (Orbiting_object.count orbiting_objects 0 "COM")

  let part2 = Orbiting_object.breath_first_search
    orbiting_objects
    ~start:"YOU"
    ~goal:"SAN"
  ;;

end