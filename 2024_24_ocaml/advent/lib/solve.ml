open! Base
open! Core

let sample_1 =
  {|x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02|}
  |> String.split_lines
;;

let sample_2 =
  {|x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj|}
  |> String.split_lines
;;

type t = int String.Table.t * (string * string * string) String.Map.t [@@deriving sexp]

let parse lines =
  let parse_initial_values =
    List.fold ~init:(String.Table.create ()) ~f:(fun values line ->
      match String.split_on_chars ~on:[ ' '; ':' ] line with
      | [ key; ""; data ] ->
        let data = Int.of_string data in
        Hashtbl.add_exn values ~key ~data;
        values
      | _ -> assert false)
  in
  let parse_transformations =
    List.fold ~init:String.Map.empty ~f:(fun transfos line ->
      match String.split ~on:' ' line with
      | [ left; op; right; "->"; key ] -> Map.add_exn transfos ~key ~data:(left, op, right)
      | _ -> assert false)
  in
  let rec parse initial_values = function
    | "" :: rest ->
      let values = parse_initial_values initial_values in
      let transformations = parse_transformations rest in
      values, transformations
    | a :: rest -> parse (a :: initial_values) rest
    | [] -> assert false
  in
  parse [] lines
;;

let get values transfos key =
  let seen = String.Hash_set.create () in
  let rec get key =
    match Hashtbl.find values key with
    | Some value -> value
    | None ->
      let left, op, right = Map.find_exn transfos key in
      Hash_set.add seen left;
      Hash_set.add seen right;
      let left = get left in
      let right = get right in
      let value =
        match op with
        | "AND" -> left * right
        | "OR" -> if left = 1 || right = 1 then 1 else 0
        | "XOR" -> if left = right then 0 else 1
        | _ -> assert false
      in
      value
  in
  get key, seen
;;

let read values transfos c =
  List.concat [ Hashtbl.keys values; Map.keys transfos ]
  |> List.filter ~f:(fun key -> String.to_list key |> List.hd_exn |> Char.equal c)
  |> List.sort ~compare:String.compare
  |> List.map ~f:(fun z -> get values transfos z)
;;

let read' values transfos c =
  read values transfos c |> List.map ~f:(fun (value, _) -> Int.to_string value)
;;

let print l = List.rev l |> String.concat |> String.pad_left ~len:46 |> print_endline

let part1 (lines : string list) =
  let values, transfos = parse lines in
  let z_values = read values transfos 'z' in
  List.foldi z_values ~init:0 ~f:(fun i total (value, _) ->
    if value = 1 then total + Int.(2 ** i) else total)
;;

let part2 (lines : string list) =
  let values, transfos = parse lines in
  let x_values = read' values transfos 'x' in
  let y_values = read' values transfos 'y' in
  print x_values;
  print y_values;
  let _, s1 = get values transfos "z40" in
  let _, s2 = get values transfos "z39" in
  let _, s3 = get values transfos "z21" in
  let _, s4 = get values transfos "z10" in
  let _, s5 = get values transfos "z08" in
  let _, s6 = get values transfos "z07" in
  let all_inter =
    List.fold [ s1; s2; s3; s4; s5; s6 ] ~init:s1 ~f:(fun total seen ->
      Hash_set.inter total seen)
  in
  let z_values = read' values transfos 'z' in
  print z_values;
  Hash_set.length all_inter
;;

let%expect_test _ =
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part1 sample_2 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    100
    ("part1 sample_1" 4)
    0011111101000
    ("part1 sample_2" 2024)
    ("part2 sample_1" 0)
    |}]
;;
