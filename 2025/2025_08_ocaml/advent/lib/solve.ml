open! Base
open! Core

[@@@warning "-32"]
[@@@warning "-27"]
[@@@warning "-26"]

let should_print_debug = ref false
let debug sexp = if !should_print_debug then print_s sexp
let impossible () = raise_s [%message "impossible"]
let sum' = List.sum (module Int)
let sum = List.sum (module Int) ~f:Fn.id
let mul = List.fold ~init:1 ~f:(fun a b -> a * b)

let sample_1 =
  {|162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689|}
  |> String.split_lines
;;

module T = struct
  type t =
    { x : int
    ; y : int
    ; z : int
    }
  [@@deriving sexp, compare, equal, hash]
end

include T
module Table = Hashtbl.Make (T)

let parse s =
  match String.split s ~on:',' with
  | [ x; y; z ] -> { x = Int.of_string x; y = Int.of_string y; z = Int.of_string z }
  | _ -> impossible ()
;;

let distance a b = sum' [ b.x - a.x; b.y - a.y; b.z - a.z ] ~f:(fun x -> Int.pow x 2)
let equal a b = a.x = b.x && a.y = b.y && a.z = b.z

let print_d circuits =
  debug [%message "ROBIN----------"];
  Array.iter circuits ~f:(fun points ->
    debug [%message "circuit"];
    List.iter points ~f:(fun t -> debug [%sexp (t : t)]));
  debug [%message "---------------"]
;;

let number = ref 0

let rec connect'
          ~(nodes_to_circuits : string Table.t)
          ~(circuits_to_nodes : t list String.Table.t)
          (index : int)
          (connections : (t * t) list)
  =
  if List.length connections % 1000 = 0
  then print_s [%message (List.length connections : int)];
  if index = 0
  then circuits_to_nodes, nodes_to_circuits
  else (
    match connections with
    | [] -> circuits_to_nodes, nodes_to_circuits
    | (a, b) :: rest ->
      let is_in_circuit node = Hashtbl.find nodes_to_circuits node in
      let add_to_new_group a b =
        let group = Int.to_string !number in
        let () = number := !number + 1 in
        Hashtbl.add_exn nodes_to_circuits ~key:a ~data:group;
        Hashtbl.add_exn nodes_to_circuits ~key:b ~data:group;
        Hashtbl.add_exn circuits_to_nodes ~key:group ~data:[ a; b ];
        debug [%message "New group" (group : string)]
      in
      let add_to_group (a : t) (group : string) =
        let nodes = Hashtbl.find_exn circuits_to_nodes group in
        Hashtbl.set nodes_to_circuits ~key:a ~data:group;
        Hashtbl.set circuits_to_nodes ~key:group ~data:(a :: nodes);
        debug [%message "Add to group" (group : string)]
      in
      let merge_groups a b =
        let group = Hashtbl.find_exn nodes_to_circuits a in
        let group' = Hashtbl.find_exn nodes_to_circuits b in
        let nodes =
          [ a; b ]
          |> List.map ~f:(Hashtbl.find_exn nodes_to_circuits)
          |> List.map ~f:(Hashtbl.find_exn circuits_to_nodes)
          |> List.concat
        in
        Hashtbl.set nodes_to_circuits ~key:a ~data:group;
        Hashtbl.set nodes_to_circuits ~key:b ~data:group;
        Hashtbl.set circuits_to_nodes ~key:group ~data:nodes;
        Hashtbl.set circuits_to_nodes ~key:group' ~data:[];
        debug [%message "Merge groups" (group : string) (group' : string)]
      in
      let something_happened =
        match is_in_circuit a, is_in_circuit b with
        | None, None ->
          add_to_new_group a b;
          `Yes
        | Some group, None ->
          add_to_group b group;
          `Yes
        | None, Some group ->
          add_to_group a group;
          `Yes
        | Some group, Some group' when String.equal group group' ->
          debug [%message "Nothing to do"];
          `Yes
        | Some group, Some group' ->
          merge_groups a b;
          `Yes
      in
      (match something_happened with
       | `Yes -> connect' ~nodes_to_circuits ~circuits_to_nodes (index - 1) rest
       | `No -> connect' ~nodes_to_circuits ~circuits_to_nodes index rest))
;;

let part1 (lines : string list) =
  let coords = List.map lines ~f:parse in
  let sorted_coords =
    List.cartesian_product coords coords
    |> List.filter ~f:(fun (a, b) -> distance a b = 0 |> not)
    |> List.dedup_and_sort ~compare:(fun (la, lb) (ra, rb) ->
      let ld = distance la lb in
      let rd = distance ra rb in
      Int.compare ld rd)
  in
  print_s [%message (List.length sorted_coords : int)];
  let index = if !should_print_debug then 10 else 1000 in
  let circuits_to_nodes = String.Table.of_alist_exn [] in
  let nodes_to_circuits = Table.of_alist_exn [] in
  let circuits = connect' ~nodes_to_circuits ~circuits_to_nodes index sorted_coords in
  let circuits_to_nodes, nodes_to_circuits = circuits in
  (*debug [%message (nodes_to_circuits : string Table.t)];*)
  (*debug [%message (circuits_to_nodes : t list String.Table.t)];*)
  let lengths =
    Hashtbl.data circuits_to_nodes
    |> List.map ~f:List.length
    |> List.sort ~compare:Int.compare
    |> List.rev
  in
  let top_3 = List.take lengths 3 in
  print_s [%message (top_3 : int list)];
  mul top_3
;;

let part2 (lines : string list) = 0

let%expect_test _ =
  should_print_debug := true;
  print_s [%message (List.cartesian_product [ 0 ] [ 1; 2; 3 ] : (int * int) list)];
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    ("List.cartesian_product [0] [1; 2; 3]" ((0 1) (0 2) (0 3)))
    ("List.length sorted_coords" 190)
    ("New group" (group 0))
    ("Add to group" (group 0))
    ("New group" (group 1))
    "Nothing to do"
    ("New group" (group 2))
    ("New group" (group 3))
    ("New group" (group 4))
    ("Add to group" (group 1))
    ("Add to group" (group 0))
    ("Merge groups" (group 2) (group' 1))
    (top_3 (5 4 2))
    ("part1 sample_1" 40)
    ("part2 sample_1" 0)
    |}]
;;
