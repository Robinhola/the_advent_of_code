open! Base
open! Core
open! Import

let lines = Advent.read_lines "day13" ~for_tests:
  [ "[1,1,3,1,1]"
  ; "[1,1,5,1,1]"
  ; ""
  ; "[[1],[2,3,4]]"
  ; "[[1],4]"
  ; ""
  ; "[9]"
  ; "[[8,7,6]]"
  ; ""
  ; "[[4,4],4,4]"
  ; "[[4,4],4,4,4]"
  ; ""
  ; "[7,7,7,7]"
  ; "[7,7,7]"
  ; ""
  ; "[]"
  ; "[3]"
  ; ""
  ; "[[[]]]"
  ; "[[]]"
  ; ""
  ; "[1,[2,[3,[4,[5,6,7]]]],8,9]"
  ; "[1,[2,[3,[4,[5,6,0]]]],8,9]"
  ] ()
;;

module Input = struct
  type t =
    | List of t list 
    | Value of int
    [@@deriving sexp_of]
  ;;

  let get_list_exn = function 
    | List l -> l
    | Value _ -> failwith "not a list"
  ;;

  let rec compare (left: t) (right: t) = match left, right with
    | Value l, Value r -> r - l 
    | Value _, List _ -> compare (List [left]) right 
    | List _, Value _ -> compare left (List [right])
    | List l, List r -> 
      match l, r with
      | [], [] -> 0
      | [], _ -> 1
      | _, [] -> -1
      | l :: rest_l, r :: rest_r ->
        let result = compare l r in
        if Int.equal result 0
        then compare (List rest_l) (List rest_r)
        else result
  ;;

  let equal a b = compare a b |> Int.equal 0

  let make line =
    let line = String.to_list line in
    let rec make_ current rest =
      let add_to_current sub current = 
        let l = get_list_exn current in
        List (sub :: l)
      in
      match rest with
      | [] -> current, []
      | ',' :: rest -> make_ current rest
      | '[' :: rest -> 
        let sub, rest = make_ (List []) rest in
        let current = add_to_current sub current in
        make_ current rest
      | ']' :: rest -> 
        let l = get_list_exn current in
        List (l |> List.rev), rest
      | '1' :: '0' :: rest -> 
        let sub = Value 10 in
        let current = add_to_current sub current in
        make_ current rest
      | n :: rest -> 
        let sub = n |> Char.to_string |> Int.of_string |> Value in
        let current = add_to_current sub current in
        make_ current rest
    in
    match make_ (List []) line with
    | t, [] -> 
      let l = get_list_exn t in 
      List.hd_exn l
    | _ -> failwith "did not complete"
  ;;

  let%expect_test "" = 
    let a = make "[[2]]" in
    let b = make "[[2]]" in
    let c = make "[[3]]" in
    let d = make "[2]" in
    print_s [%message (equal a a : bool)];
    print_s [%message (equal a b : bool)];
    print_s [%message (equal a c : bool)];
    print_s [%message (equal a d : bool)];
    [%expect {|
      ("equal a a" true)
      ("equal a b" true)
      ("equal a c" false)
      ("equal a d" true) |}]
  ;;


  let%expect_test "" =
    print_s [%message ( make "[1,1,3,1,1]" : t )];
    print_s [%message ( make "[[1],[2,3,4]]" : t )];
    print_s [%message ( make "[[[]]]" : t )];
    print_s [%message ( make "[1,[2,[3,[4,[5,6,7]]]],8,9]" : t )];
    [%expect {|
      ("make \"[1,1,3,1,1]\""
       (List ((Value 1) (Value 1) (Value 3) (Value 1) (Value 1))))
      ("make \"[[1],[2,3,4]]\""
       (List ((List ((Value 1))) (List ((Value 2) (Value 3) (Value 4))))))
      ("make \"[[[]]]\"" (List ((List ((List ()))))))
      ("make \"[1,[2,[3,[4,[5,6,7]]]],8,9]\""
       (List
        ((Value 1)
         (List
          ((Value 2)
           (List
            ((Value 3) (List ((Value 4) (List ((Value 5) (Value 6) (Value 7)))))))))
         (Value 8) (Value 9)))) |}]
  ;;

  let rec read pairs = function 
    | [] -> pairs |> List.rev 
    | left :: right :: "" :: rest
    | left :: right :: rest ->
      let pair = (make left), (make right) in
      read (pair :: pairs) rest
    | _ -> failwith "wrong format"
  ;;

  let%expect_test "" =
    let pairs = read [] lines in
    let values = pairs |> List.mapi ~f:(fun i (l, r) -> i + 1, compare l r) in
    print_s [%message (values : (int * int) list)];
    let values = values
      |> List.filter ~f:(fun (_, v) -> Int.is_positive v)
      |> List.map ~f:(fun (i, _) -> i) in
    let sum = List.sum (module Int) values ~f:Fn.id in
    print_s [%message (sum : int)];
    [%expect {|
      (values ((1 2) (2 2) (3 -1) (4 1) (5 -1) (6 1) (7 -1) (8 -7)))
      (sum 13) |}]
  ;;

  let rec read_all l = function  
    | [] -> l 
    | "" :: rest -> read_all l rest
    | line :: rest ->
      let t = make line in
      read_all (t :: l) rest
  ;;



end


module T : sig
  include Day.T
end = struct
  let name = "--- Day 13: Distress Signal ---"
  let part1 =
    let pairs = Input.read [] lines in
    let values = pairs |> List.mapi ~f:(fun i (l, r) -> i + 1, Input.compare l r) in
    let values = values
      |> List.filter ~f:(fun (_, v) -> Int.is_positive v)
      |> List.map ~f:(fun (i, _) -> i) in
    let sum = List.sum (module Int) values ~f:Fn.id in
    sum

  let part2_ () = 
    let input = Input.read_all [] lines in
    let decoder_a = Input.make "[[2]]" in
    let decoder_b = Input.make "[[6]]" in
    let input = decoder_a :: decoder_b :: input in
    let sorted = input |> List.sort ~compare:Input.compare |> List.rev in
    let ia, _ = sorted |> List.findi_exn ~f:(fun _ x -> Input.equal x decoder_a) in
    let ib, _ = sorted |> List.findi_exn ~f:(fun _ x -> Input.equal x decoder_b) in
    (ia + 1) * (ib + 1)
  ;;
  let part2 = part2_ ()

  let%expect_test "" =
    print_s [%message (part1 : int) (part2 : int)];
    [%expect {| ((part1 13) (part2 140)) |}]
  ;;
end