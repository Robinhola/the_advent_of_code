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

type t =
  { x : int
  ; y : int
  ; z : int
  }
[@@deriving sexp]

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

let rec connect' circuits index connections =
  (*print_d circuits;*)
  let n = List.length connections in
  if n % 1_000 = 0 then print_s [%message (n : int)];
  if index = 0
  then circuits
  else (
    match connections with
    | [] -> circuits
    | (a, b) :: rest ->
      let is_in_circuit x =
        Array.findi circuits ~f:(fun _ group ->
          List.find group ~f:(equal x) |> Option.is_some)
      in
      let a_in_circuit = is_in_circuit a in
      let b_in_circuit = is_in_circuit b in
      let add_to_new_group a b =
        (*debug [%message "new group" (a : t) (b : t)];*)
        let new_circuit = Array.of_list [ [ a; b ] ] in
        let circuits = Array.append circuits new_circuit in
        connect' circuits (index - 1) rest
      in
      let add_to_exiting_circuit x i circuit =
        (*debug [%message "existing circuit" (x : t)];*)
        Array.set circuits i (x :: circuit);
        connect' circuits (index - 1) rest
      in
      let merge_circuits i circuit_i j circuit_j =
        (*debug [%message "merging circuit" (i : int) (j : int)];*)
        let new_circuit = circuit_i @ circuit_j in
        (*debug [%message (Array.map circuits ~f:List.length : int array)];*)
        Array.set circuits i new_circuit;
        Array.set circuits j [];
        (*let circuits =*)
        (*  Array.filteri circuits ~f:(fun index _ -> not (Int.equal index j))*)
        (*in*)
        (*debug [%message (Array.map circuits ~f:List.length : int array)];*)
        connect' circuits (index - 1) rest
      in
      (match a_in_circuit, b_in_circuit with
       | None, None -> add_to_new_group a b
       | Some (i, circuit), None -> add_to_exiting_circuit b i circuit
       | None, Some (i, circuit) -> add_to_exiting_circuit a i circuit
       | Some (i, circuit_i), Some (j, circuit_j) when i = j ->
         (*debug [%message "nothing happens!" (a : t) (b : t)];*)
         connect' circuits (index - 1) rest
       | Some (i, circuit_i), Some (j, circuit_j) ->
         merge_circuits i circuit_i j circuit_j))
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
  let index = if !should_print_debug then 10 else -1 in
  let circuits = connect' (Array.of_list []) index sorted_coords in
  let lengths =
    Array.map circuits ~f:List.length
    |> Array.to_list
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
    ROBIN----------
    ---------------
    ("new group" (a ((x 425) (y 690) (z 689))) (b ((x 162) (y 817) (z 812))))
    ROBIN----------
    circuit
    ((x 425) (y 690) (z 689))
    ((x 162) (y 817) (z 812))
    ---------------
    ("existing circuit" (x ((x 431) (y 825) (z 988))))
    ROBIN----------
    circuit
    ((x 431) (y 825) (z 988))
    ((x 425) (y 690) (z 689))
    ((x 162) (y 817) (z 812))
    ---------------
    ("new group" (a ((x 805) (y 96) (z 715))) (b ((x 906) (y 360) (z 560))))
    ROBIN----------
    circuit
    ((x 431) (y 825) (z 988))
    ((x 425) (y 690) (z 689))
    ((x 162) (y 817) (z 812))
    circuit
    ((x 805) (y 96) (z 715))
    ((x 906) (y 360) (z 560))
    ---------------
    ("nothing happens!" (a ((x 425) (y 690) (z 689)))
     (b ((x 431) (y 825) (z 988))))
    ROBIN----------
    circuit
    ((x 431) (y 825) (z 988))
    ((x 425) (y 690) (z 689))
    ((x 162) (y 817) (z 812))
    circuit
    ((x 805) (y 96) (z 715))
    ((x 906) (y 360) (z 560))
    ---------------
    ("new group" (a ((x 984) (y 92) (z 344))) (b ((x 862) (y 61) (z 35))))
    ROBIN----------
    circuit
    ((x 431) (y 825) (z 988))
    ((x 425) (y 690) (z 689))
    ((x 162) (y 817) (z 812))
    circuit
    ((x 805) (y 96) (z 715))
    ((x 906) (y 360) (z 560))
    circuit
    ((x 984) (y 92) (z 344))
    ((x 862) (y 61) (z 35))
    ---------------
    ("new group" (a ((x 117) (y 168) (z 530))) (b ((x 52) (y 470) (z 668))))
    ROBIN----------
    circuit
    ((x 431) (y 825) (z 988))
    ((x 425) (y 690) (z 689))
    ((x 162) (y 817) (z 812))
    circuit
    ((x 805) (y 96) (z 715))
    ((x 906) (y 360) (z 560))
    circuit
    ((x 984) (y 92) (z 344))
    ((x 862) (y 61) (z 35))
    circuit
    ((x 117) (y 168) (z 530))
    ((x 52) (y 470) (z 668))
    ---------------
    ("new group" (a ((x 941) (y 993) (z 340))) (b ((x 819) (y 987) (z 18))))
    ROBIN----------
    circuit
    ((x 431) (y 825) (z 988))
    ((x 425) (y 690) (z 689))
    ((x 162) (y 817) (z 812))
    circuit
    ((x 805) (y 96) (z 715))
    ((x 906) (y 360) (z 560))
    circuit
    ((x 984) (y 92) (z 344))
    ((x 862) (y 61) (z 35))
    circuit
    ((x 117) (y 168) (z 530))
    ((x 52) (y 470) (z 668))
    circuit
    ((x 941) (y 993) (z 340))
    ((x 819) (y 987) (z 18))
    ---------------
    ("existing circuit" (x ((x 739) (y 650) (z 466))))
    ROBIN----------
    circuit
    ((x 431) (y 825) (z 988))
    ((x 425) (y 690) (z 689))
    ((x 162) (y 817) (z 812))
    circuit
    ((x 739) (y 650) (z 466))
    ((x 805) (y 96) (z 715))
    ((x 906) (y 360) (z 560))
    circuit
    ((x 984) (y 92) (z 344))
    ((x 862) (y 61) (z 35))
    circuit
    ((x 117) (y 168) (z 530))
    ((x 52) (y 470) (z 668))
    circuit
    ((x 941) (y 993) (z 340))
    ((x 819) (y 987) (z 18))
    ---------------
    ("existing circuit" (x ((x 346) (y 949) (z 466))))
    ROBIN----------
    circuit
    ((x 346) (y 949) (z 466))
    ((x 431) (y 825) (z 988))
    ((x 425) (y 690) (z 689))
    ((x 162) (y 817) (z 812))
    circuit
    ((x 739) (y 650) (z 466))
    ((x 805) (y 96) (z 715))
    ((x 906) (y 360) (z 560))
    circuit
    ((x 984) (y 92) (z 344))
    ((x 862) (y 61) (z 35))
    circuit
    ((x 117) (y 168) (z 530))
    ((x 52) (y 470) (z 668))
    circuit
    ((x 941) (y 993) (z 340))
    ((x 819) (y 987) (z 18))
    ---------------
    ("merging circuit" (i 2) (j 1))
    ("Array.map circuits ~f:List.length" (4 3 2 2 2))
    ("Array.map circuits ~f:List.length" (4 5 2 2))
    ROBIN----------
    circuit
    ((x 346) (y 949) (z 466))
    ((x 431) (y 825) (z 988))
    ((x 425) (y 690) (z 689))
    ((x 162) (y 817) (z 812))
    circuit
    ((x 984) (y 92) (z 344))
    ((x 862) (y 61) (z 35))
    ((x 739) (y 650) (z 466))
    ((x 805) (y 96) (z 715))
    ((x 906) (y 360) (z 560))
    circuit
    ((x 117) (y 168) (z 530))
    ((x 52) (y 470) (z 668))
    circuit
    ((x 941) (y 993) (z 340))
    ((x 819) (y 987) (z 18))
    ---------------
    (top_3 (5 4 2))
    ("part1 sample_1" 40)
    ("part2 sample_1" 0)
    |}]
;;
