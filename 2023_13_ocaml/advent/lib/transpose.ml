open! Base
open! Core

let rec turn_rows_to_columns' (cols : char list list) = function
  | [] -> cols |> List.map ~f:(Fn.compose String.of_list List.rev)
  | row :: rest ->
    let cols =
      match cols with
      | [] -> row |> List.map ~f:(fun c -> [ c ])
      | _ -> List.zip_exn row cols |> List.map ~f:(fun (c, col) -> c :: col)
    in
    turn_rows_to_columns' cols rest
;;

let turn_rows_to_columns (rows : string list) =
  turn_rows_to_columns' [] (rows |> List.map ~f:String.to_list)
;;

let%expect_test _ =
  print_s [%message (turn_rows_to_columns [ "abc"; "ABC" ] : string list)];
  [%expect {| ("turn_rows_to_columns [\"abc\"; \"ABC\"]" (aA bB cC)) |}]
;;
