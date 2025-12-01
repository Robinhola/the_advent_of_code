open! Base
open! Core

let sample_1 =
  {|1
10
100
2024|}
  |> String.split_lines
;;

let sample_2 =
  {|1
2
3
2024|}
  |> String.split_lines
;;

let mix n x = Int.bit_xor x n
let prune x = x % 16777216

let%expect_test _ =
  print_s [%message (mix 15 42 : int) (mix 42 15 : int)];
  print_s [%message (prune 100000000 : int)];
  [%expect
    {|
    (("mix 15 42" 37) ("mix 42 15" 37))
    ("prune 100000000" 16113920)
    |}]
;;

type t =
  { x : int
  ; p : int
  ; d : int
  }
[@@deriving sexp]

let print_t t =
  let x = t.x |> Int.to_string |> String.pad_left ~len:8 in
  let p = t.p |> Int.to_string in
  let d = t.d |> Int.to_string in
  print_endline (x ^ ": " ^ p ^ " (" ^ d ^ ")")
;;

let next t =
  let x = t.x in
  let x = x * 64 |> mix x |> prune in
  let x = x / 32 |> mix x |> prune in
  let x = x * 2048 |> mix x |> prune in
  let p = x % 10 in
  let d = p - t.p in
  { x; p; d }
;;

let of_x x =
  let p = x % 10 in
  { x; p; d = p }
;;

let%expect_test _ =
  let t = of_x 123 in
  let test t =
    let t = next t in
    print_t t;
    t
  in
  print_t t;
  let _ = Fn.apply_n_times ~n:9 test t in
  [%expect
    {|
         123: 3 (3)
    15887950: 0 (-3)
    16495136: 6 (6)
      527345: 5 (-1)
      704524: 4 (-1)
     1553684: 4 (0)
    12683156: 6 (2)
    11100544: 4 (-2)
    12249484: 4 (0)
     7753432: 2 (-2)
    |}]
;;

let part1 (lines : string list) =
  List.sum
    (module Int)
    lines
    ~f:(fun line ->
      let x = Int.of_string line in
      let t = of_x x in
      let t = Fn.apply_n_times ~n:2000 next t in
      t.x)
;;

module Cache = struct
  module T = struct
    type t = int * int * int * int [@@deriving sexp, compare, hash]
  end

  include T
  module Hashtbl = Hashtbl.Make (T)
end

let all_suites ?(print = false) n t =
  let suites = Cache.Hashtbl.create () in
  let a = ref (next t) in
  let b = ref (next !a) in
  let c = ref (next !b) in
  let _ =
    Fn.apply_n_times
      ~n:(n - 3)
      (fun t ->
         let d = next t in
         let key = !a.d, !b.d, !c.d, d.d in
         let data = d.p in
         if print then print_s [%message (key : Cache.t) (data : int)];
         Hashtbl.add suites ~key ~data |> Fn.ignore;
         a := !b;
         b := !c;
         c := d;
         d)
      !c
  in
  suites
;;

let%expect_test _ =
  let t = of_x 123 in
  let test t =
    let t = next t in
    print_t t;
    t
  in
  print_t t;
  let _ = Fn.apply_n_times ~n:9 test t in
  let _ = (all_suites ~print:true 9 t : int Cache.Hashtbl.t) in
  [%expect
    {|
         123: 3 (3)
    15887950: 0 (-3)
    16495136: 6 (6)
      527345: 5 (-1)
      704524: 4 (-1)
     1553684: 4 (0)
    12683156: 6 (2)
    11100544: 4 (-2)
    12249484: 4 (0)
     7753432: 2 (-2)
    ((key (-3 6 -1 -1)) (data 4))
    ((key (6 -1 -1 0)) (data 4))
    ((key (-1 -1 0 2)) (data 6))
    ((key (-1 0 2 -2)) (data 4))
    ((key (0 2 -2 0)) (data 4))
    ((key (2 -2 0 -2)) (data 2))
    |}]
;;

(* 2261 was too high *)
let part2 (lines : string list) =
  let counts = Cache.Hashtbl.create () in
  List.iter lines ~f:(fun line ->
    let t = of_x (Int.of_string line) in
    let all_suites = all_suites 2000 t in
    Hashtbl.iteri all_suites ~f:(fun ~key ~data ->
      let current_total = Hashtbl.find_or_add counts key ~default:(Fn.const 0) in
      Hashtbl.set counts ~key ~data:(current_total + data)));
  Hashtbl.data counts |> List.max_elt ~compare:Int.compare |> Option.value_exn
;;

let%expect_test _ =
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_2 : int)];
  [%expect
    {|
    ("part1 sample_1" 37327623)
    ("part2 sample_2" 23)
    |}]
;;
