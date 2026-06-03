open! Base
open! Core

let example =
  {|Card  1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card  2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card  3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card  4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card  5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card  6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11|}
;;

type t =
  { card : int
  ; winning_numbers : Int.Set.t
  ; card_numbers : Int.Set.t
  }
[@@deriving sexp]

module Parse = struct
  let sanitize' splitted = splitted |> List.filter ~f:(Fn.non String.is_empty)
  let sanitize numbers = sanitize' numbers |> List.map ~f:Int.of_string |> Int.Set.of_list

  let numbers (number_part : string) =
    match String.split ~on:'|' number_part |> List.map ~f:(String.split ~on:' ') with
    | [ winning; on_the_card ] -> sanitize winning, sanitize on_the_card
    | _ -> failwith "Invalid number"
  ;;

  let card (card_part : string) =
    match String.split ~on:' ' card_part |> sanitize' with
    | [ "Card"; id ] -> Int.of_string id
    | rest -> raise_s [%message "Invalid line" (card_part : string) (rest : string list)]
  ;;

  let line line =
    match String.split ~on:':' line with
    | [ card_part; number_part ] ->
      let winning_numbers, card_numbers = numbers number_part in
      { card = card card_part; card_numbers; winning_numbers }
    | _ -> raise_s [%message "Invalid line" (line : string)]
  ;;

  let data (data : string) : t list = String.split_lines data |> List.map ~f:line

  let%expect_test _ =
    print_s [%sexp (data example : t list)];
    [%expect
      {|
    (((card 1) (winning_numbers (17 41 48 83 86))
      (card_numbers (6 9 17 31 48 53 83 86)))
     ((card 2) (winning_numbers (13 16 20 32 61))
      (card_numbers (17 19 24 30 32 61 68 82)))
     ((card 3) (winning_numbers (1 21 44 53 59))
      (card_numbers (1 14 16 21 63 69 72 82)))
     ((card 4) (winning_numbers (41 69 73 84 92))
      (card_numbers (5 51 54 58 59 76 83 84)))
     ((card 5) (winning_numbers (26 28 32 83 87))
      (card_numbers (12 22 30 36 70 82 88 93)))
     ((card 6) (winning_numbers (13 18 31 56 72))
      (card_numbers (10 11 23 35 36 67 74 77)))) |}]
  ;;
end

let part1 (data : string) =
  Parse.data data
  |> List.map ~f:(fun { card_numbers; winning_numbers; _ } ->
    Set.inter winning_numbers card_numbers |> Set.length)
  |> List.sum
       (module Int)
       ~f:(function
         | 0 -> 0
         | length -> Int.pow 2 (length - 1))
;;

let add_numbers_of_copied_cards ~winning_card number_of_cards copied_card =
  let key = winning_card + copied_card in
  let data =
    let multiplier_so_far = Map.find_exn number_of_cards key in
    let from_this_card = Map.find_exn number_of_cards winning_card in
    multiplier_so_far + from_this_card
  in
  Map.set number_of_cards ~key ~data
;;

let part2 (data : string) =
  let parsed_ts : t list = Parse.data data in
  let cards_to_numbers_of_wins =
    List.map parsed_ts ~f:(fun { card; card_numbers; winning_numbers } ->
      let length = Set.inter winning_numbers card_numbers |> Set.length in
      card, length)
  in
  List.fold
    cards_to_numbers_of_wins
    ~init:(List.map parsed_ts ~f:(fun { card; _ } -> card, 1) |> Int.Map.of_alist_exn)
    ~f:(fun init (winning_card, value) ->
      List.fold
        (List.range 1 (value + 1))
        ~init
        ~f:(add_numbers_of_copied_cards ~winning_card))
  |> Map.data
  |> List.sum (module Int) ~f:Fn.id
;;

let solve () =
  print_endline [%string "part | example"];
  print_endline [%string "1    | %{part1 example#Int}"];
  print_endline [%string "2    | %{part2 example#Int}"];
  print_endline [%string "---------------"];
  print_endline [%string "part | solution"];
  print_endline [%string "1    | %{part1 Input.data#Int}"];
  print_endline [%string "2    | %{part2 Input.data#Int}"]
;;

let%expect_test _ =
  solve ();
  [%expect
    {|
    part | example
    1    | 13
    2    | 30
    ---------------
    part | solution
    1    | 22488
    2    | 7013204
    |}]
;;
