open! Base
open! Core

let sample_1 =
  {|kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn|}
  |> String.split_lines
;;

let parse =
  List.map ~f:(fun s ->
    match String.split s ~on:'-' with
    | [ a; b ] -> a, b
    | _ -> assert false)
;;

type key = string list [@@deriving sexp, compare, hash]

module Cache = Hash_set.Make (struct
    type t = key [@@deriving sexp, compare, hash]
  end)

let get_connections lines =
  let connections = String.Table.create () in
  let () =
    parse lines
    |> List.iter ~f:(fun (a, b) ->
      Hashtbl.add_multi connections ~key:a ~data:b;
      Hashtbl.add_multi connections ~key:b ~data:a)
  in
  connections
;;

let groups_of_3 connections =
  let groups = Cache.create () in
  Hashtbl.iteri connections ~f:(fun ~key ~data ->
    let data = Array.of_list data in
    List.range 0 (Array.length data - 1)
    |> List.iter ~f:(fun i ->
      List.range (i + 1) (Array.length data)
      |> List.iter ~f:(fun j ->
        let a = key in
        let b = data.(i) in
        let c = data.(j) in
        (* a is already connected with b and c *)
        (* so we check that b and c are also connected *)
        let b_cos = Hashtbl.find_exn connections b in
        if List.exists b_cos ~f:(String.equal c)
        then (
          let group = [ a; b; c ] |> List.sort ~compare:String.compare in
          Hash_set.add groups group))));
  groups
;;

let is_connected_to_all a ~connections ~all =
  let a_cos = Hashtbl.find_exn connections a in
  Set.fold_until
    all
    ~init:true
    ~f:(fun _ b ->
      match List.exists a_cos ~f:(String.equal b) with
      | true -> Continue true
      | false -> Stop false)
    ~finish:Fn.id
;;

let biggest_group connections =
  let biggest_so_far = ref String.Set.empty in
  Hashtbl.iteri connections ~f:(fun ~key ~data ->
    let taken = String.Set.of_list [ key ] in
    let rec try_ taken = function
      | [] ->
        if Set.length taken > Set.length !biggest_so_far then biggest_so_far := taken
      | a :: rest ->
        try_ taken rest;
        if is_connected_to_all a ~connections ~all:taken then try_ (Set.add taken a) rest
    in
    try_ taken data);
  !biggest_so_far
;;

let part1 (lines : string list) =
  let connections = get_connections lines in
  let groups = groups_of_3 connections in
  Hash_set.count groups ~f:(fun group ->
    List.exists group ~f:(fun s -> String.get s 0 |> Char.equal 't'))
;;

let part2 (lines : string list) =
  let connections = get_connections lines in
  let biggest_group = biggest_group connections in
  biggest_group
  |> Set.to_list
  |> List.sort ~compare:String.compare
  |> String.concat ~sep:","
  |> print_endline;
  (*print_s [%message (biggest_group : String.Set.t)];*)
  0
;;

let%expect_test _ =
  let connections = get_connections sample_1 in
  let groups = groups_of_3 connections in
  print_s [%message (connections : key String.Table.t)];
  print_s [%message (groups : Cache.t)];
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    (connections
     ((aq (wq cg vc yn)) (cg (aq yn tb de)) (co (tc de ta ka)) (de (ka ta co cg))
      (ka (de ta tb co)) (kh (ta ub qp tc)) (qp (wh td ub kh)) (ta (kh de ka co))
      (tb (vc wq ka cg)) (tc (co td wh kh)) (td (yn qp wh tc)) (ub (vc wq kh qp))
      (vc (tb wq ub aq)) (wh (qp yn td tc)) (wq (vc aq ub tb))
      (yn (td wh cg aq))))
    (groups
     ((aq cg yn) (aq vc wq) (co de ka) (co de ta) (co ka ta) (de ka ta)
      (kh qp ub) (qp td wh) (tb vc wq) (tc td wh) (td wh yn) (ub vc wq)))
    ("part1 sample_1" 7)
    co,de,ka,ta
    ("part2 sample_1" 0)
    |}]
;;
