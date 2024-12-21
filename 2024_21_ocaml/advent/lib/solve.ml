open! Base
open! Core
open! Robin_advent_lib

let numeric_keypad =
  {|789
456
123
#0A|}
  |> String.split_lines
  |> Matrix.parse
;;

let directional_keypad =
  {|#^A
<v>|}
  |> String.split_lines
  |> Matrix.parse
;;

module Cache = Hashtbl.Make (struct
    type t = char * char [@@deriving sexp, compare, hash]
  end)

module Cache' = Hashtbl.Make (struct
    type t = char list * int [@@deriving sexp, compare, hash]
  end)

let sample_1 =
  {|029A
980A
179A
456A
379A|}
  |> String.split_lines
;;

let from_dir_to_dir = function
  | Dir.Up -> '^'
  | Dir.Right -> '>'
  | Dir.Left -> '<'
  | Dir.Down -> 'v'
  | _ -> assert false
;;

let min l = List.min_elt ~compare:Int.compare l |> Option.value_exn
let min' t = Hashtbl.keys t |> min

let rec find ~m ~seen ~bests to_ queue =
  if Queue.is_empty queue
  then bests
  else (
    let pos, i, path = Queue.dequeue_exn queue in
    if Matrix.get m pos |> Char.equal to_
    then (
      Hashtbl.add_multi bests ~key:i ~data:(List.rev path);
      find ~m ~seen ~bests to_ queue)
    else if
      Matrix.get m pos |> Char.equal '#'
      || (Hashtbl.mem seen pos && Hashtbl.find_exn seen pos < i)
      || (Hashtbl.length bests > 0 && min' bests < i)
    then find ~m ~seen ~bests to_ queue
    else (
      Hashtbl.set seen ~key:pos ~data:i;
      [ Dir.Up; Dir.Right; Dir.Down; Dir.Left ]
      |> List.filter_map ~f:(fun dir ->
        Matrix.next m pos dir
        |> Option.map ~f:(fun c -> c, i + 1, from_dir_to_dir dir :: path))
      |> Queue.enqueue_all queue;
      find ~m ~seen ~bests to_ queue))
;;

let all_shortest_paths m from to_ =
  let seen = Coord.Hashtbl.create () in
  let start = Matrix.find_exn m from in
  let queue = Queue.of_list [ start, 0, [] ] in
  find ~m ~seen ~bests:(Int.Table.create ()) to_ queue
;;

type shortest_paths = char list list Int.Table.t [@@deriving sexp]

let rec length m = function
  | c :: o :: de ->
    let i =
      match all_shortest_paths m c o |> Hashtbl.keys with
      | [ i ] -> i
      | _ -> assert false
    in
    i + length m (o :: de)
  | _ :: [] | [] -> 0
;;

let all_numeric_paths =
  let chars = [ 'A'; '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ] in
  List.cartesian_product chars chars
  |> List.map ~f:(fun (s, e) ->
    let key = String.of_list [ s; e ] in
    let possibles =
      all_shortest_paths numeric_keypad s e |> Hashtbl.data |> List.hd_exn
    in
    key, possibles)
  |> String.Table.of_alist_exn
;;

let best_directional_paths =
  let chars = [ 'A'; 'v'; '^'; '<'; '>' ] in
  List.cartesian_product chars chars
  |> List.map ~f:(fun (s, e) ->
    let key = s, e in
    let possibles =
      match all_shortest_paths directional_keypad s e |> Hashtbl.data with
      | [ possibles ] -> possibles
      | _ -> assert false
    in
    let best =
      List.min_elt possibles ~compare:(fun a b ->
        let la = length directional_keypad a in
        let lb = length directional_keypad b in
        Int.compare la lb)
      |> Option.value_exn
    in
    key, best)
  |> Cache.of_alist_exn
;;

let%expect_test _ =
  best_directional_paths
  |> Hashtbl.iteri ~f:(fun ~key ~data ->
    print_s [%message (key : char * char) (data : char list)]);
  [%expect
    {|
    ((key (v A)) (data (> ^)))
    ((key (v <)) (data (<)))
    ((key (< v)) (data (>)))
    ((key (^ A)) (data (>)))
    ((key (A v)) (data (< v)))
    ((key (> ^)) (data (< ^)))
    ((key (^ ^)) (data ()))
    ((key (A ^)) (data (<)))
    ((key (^ <)) (data (v <)))
    ((key (< ^)) (data (> ^)))
    ((key (A <)) (data (v < <)))
    ((key (> A)) (data (^)))
    ((key (< A)) (data (> > ^)))
    ((key (> >)) (data ()))
    ((key (^ v)) (data (v)))
    ((key (v >)) (data (>)))
    ((key (^ >)) (data (v >)))
    ((key (v ^)) (data (^)))
    ((key (< <)) (data ()))
    ((key (A >)) (data (v)))
    ((key (< >)) (data (> >)))
    ((key (v v)) (data ()))
    ((key (> <)) (data (< <)))
    ((key (> v)) (data (<)))
    ((key (A A)) (data ()))
    |}]
;;

let combine chemin =
  let rec combine = function
    | _ :: [] | [] -> assert false
    | [ s; e ] ->
      let key = s, e in
      let best = Hashtbl.find_exn best_directional_paths key in
      List.concat [ best; [ 'A' ] ]
    | s :: e :: rest ->
      let key = s, e in
      let best = Hashtbl.find_exn best_directional_paths key in
      let value = combine (e :: rest) in
      List.concat [ best; [ 'A' ]; value ]
  in
  combine ('A' :: chemin)
;;

let pairs s =
  let rec pairs current = function
    | [ _ ] -> List.rev current
    | a :: b :: rest -> pairs ((a ^ b) :: current) (b :: rest)
    | [] -> assert false
  in
  pairs [] (String.to_list s |> List.map ~f:Char.to_string)
;;

let algo n code =
  let pairs = pairs code in
  let possibles = List.map pairs ~f:(fun p -> p, Hashtbl.find_exn all_numeric_paths p) in
  let bests =
    List.map possibles ~f:(fun (_p, options) ->
      List.min_elt options ~compare:(fun a b ->
        let pna = (Fn.apply_n_times ~n combine) a in
        let pnb = (Fn.apply_n_times ~n combine) b in
        Int.compare (List.length pna) (List.length pnb))
      |> Option.value_exn)
    |> List.map ~f:String.of_list
    |> String.concat ~sep:"A"
  in
  let bests = bests ^ "A" in
  combine (combine (String.to_list bests)) |> String.of_list
;;

let num_part s =
  match String.split s ~on:'A' with
  | [ ""; n; "" ] -> Int.of_string n
  | _ -> assert false
;;

let part1' (lines : string list) =
  List.sum
    (module Int)
    lines
    ~f:(fun code' ->
      let code = "A" ^ code' in
      let n = num_part code in
      let p = algo 2 code in
      print_endline (code' ^ ": " ^ p);
      let l = String.length p in
      l * n)
;;

let partx ~cache ~total_iterations ~batch_size (word : char list) =
  (*let () =*)
  (*  match List.hd_exn word |> Char.equal 'A', List.last_exn word |> Char.equal 'A' with*)
  (*  | true, true -> ()*)
  (*  | false, _ | _, false -> raise_s [%message (word : char list)]*)
  (*in*)
  (*print_s [%message (word : char list)];*)
  assert (total_iterations % batch_size = 0);
  let goal = total_iterations / batch_size in
  let rec partx' ~goal i x =
    let iterations_so_far = i * batch_size in
    let key = x, iterations_so_far in
    match Hashtbl.find cache key with
    | Some value -> value
    | None ->
      let value =
        match i = goal with
        | true -> Fn.apply_n_times ~n:batch_size combine x |> List.length
        | false ->
          let next_batch =
            Fn.apply_n_times ~n:batch_size combine x
            |> String.of_list
            |> fun s ->
            String.prefix s (String.length s - 1)
            |> String.split ~on:'A'
            |> List.map ~f:(fun s -> s ^ "A")
            |> List.map ~f:String.to_list
          in
          List.sum (module Int) ~f:(partx' ~goal (i + 1)) next_batch
      in
      Hashtbl.add_exn cache ~key ~data:value;
      value
  in
  List.sum (module Int) ~f:(partx' ~goal 1) [ word ]
;;

let partx' ~cache ~total_iterations ~batch_size code =
  let bests =
    pairs code
    |> List.map ~f:(fun p -> p, Hashtbl.find_exn all_numeric_paths p)
    |> List.map ~f:(fun (_p, options) ->
      List.min_elt options ~compare:(fun a b ->
        let a = List.concat [ a; [ 'A' ] ] in
        let b = List.concat [ b; [ 'A' ] ] in
        let pna = partx ~cache ~total_iterations ~batch_size a in
        let pnb = partx ~cache ~total_iterations ~batch_size b in
        Int.compare pna pnb)
      |> Option.value_exn)
    |> List.map ~f:String.of_list
    |> String.concat ~sep:"A"
  in
  let bests = bests ^ "A" in
  partx ~cache ~total_iterations ~batch_size (String.to_list bests)
;;

let partx'' lines cache ~batch_size total =
  let value =
    List.sum
      (module Int)
      lines
      ~f:(fun code' ->
        let code = "A" ^ code' in
        let n = num_part code in
        let l = partx' ~cache ~total_iterations:total ~batch_size code in
        l * n)
  in
  (*print_s [%message (cache : int Cache'.t)];*)
  value
;;

let cache = Cache'.create ()
let part1 (lines : string list) = partx'' lines cache ~batch_size:1 2
let cache = Cache'.create ()
let part2 (lines : string list) = partx'' lines cache ~batch_size:1 25
(*let part2 (_lines : string list) = 0*)
(* 184716

high 267682029656984

     260675562033542

low  106936356357428 *)

let%expect_test _ =
  print_s [%message (part1' sample_1 : int)];
  print_s [%message (part1 sample_1 : int)];
  print_s [%message (part2 sample_1 : int)];
  [%expect
    {|
    029A: <vA<AA>>^AvAA<^A>Av<<A>>^AvA^A<vA>^Av<<A>^A>AAvA^Av<<A>A>^AAAvA<^A>A
    980A: v<<A>>^AAAvA^A<vA<AA>>^AvAA<^A>Av<<A>A>^AAAvA<^A>A<vA>^A<A>A
    179A: v<<A>>^A<vA<A>>^AAvAA<^A>Av<<A>>^AAvA^A<vA>^AA<A>Av<<A>A>^AAAvA<^A>A
    456A: v<<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>Av<<A>A>^AAvA<^A>A
    379A: v<<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>Av<<A>A>^AAAvA<^A>A
    ("part1' sample_1" 126384)
    ("part1 sample_1" 126384)
    ("part2 sample_1" 175343041201758)
    |}]
;;
