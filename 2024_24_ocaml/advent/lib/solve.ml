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

let part1 (lines : string list) =
  let values, transfos = parse lines in
  let rec get key =
    match Hashtbl.find values key with
    | Some value ->
      (*print_s [%message (key : string) (value : int)];*)
      value
    | None ->
      let left, op, right = Map.find_exn transfos key in
      (*print_s [%message (key : string) (left : string) (op : string) (right : string)];*)
      let left = get left in
      let right = get right in
      let value =
        match op with
        | "AND" -> left * right
        | "OR" -> if left = 1 || right = 1 then 1 else 0
        | "XOR" -> if left = right then 0 else 1
        | _ -> assert false
      in
      (*print_s [%message (key : string) (value : int)];*)
      value
  in
  (* Assume no z in values *)
  let z_values =
    Map.keys transfos
    |> List.filter ~f:(fun key -> String.to_list key |> List.hd_exn |> Char.equal 'z')
    |> List.sort ~compare:String.compare
    |> List.map ~f:(fun z -> z, get z)
  in
  (*print_s [%message (z_values : (string * int) list)];*)
  let z_values = z_values |> List.map ~f:(fun (_, v) -> Int.to_string v) in
  let () = z_values |> List.rev |> String.concat |> print_endline in
  List.foldi z_values ~init:0 ~f:(fun i total value ->
    (*print_s [%message (total : int) (i : int) (value : string)];*)
    if String.equal value "1" then total + Int.(2 ** i) else total)
;;

let part2 (lines : string list) =
  let _ = lines in
  0
;;

let%expect_test _ =
  (*print_s [%message (parse sample_1 : t)];*)
  (*print_s [%message (parse sample_2 : t)];*)
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
