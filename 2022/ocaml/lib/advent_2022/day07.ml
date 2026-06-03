open! Base
open! Core
open! Import

module Command = struct
  type t =
    | Cd of string
    | Cd_up
    | Ls
    [@@deriving sexp_of]
  ;;

  let parse_line = function
    | "ls" :: [] -> Ls
    | "cd" :: ".." :: [] -> Cd_up
    | "cd" :: directory :: [] -> Cd directory
    | _ -> failwith "wrong format"
  ;;
end

module Output = struct
  type t =
    | File of int * string
    | Dir of string
    [@@deriving sexp_of]
  ;;

  let parse_line = function
    | "dir" :: name :: [] -> Dir name
    | size :: name :: [] -> File (Int.of_string size, name)
    | _ -> failwith "invalid format"
  ;;
end

module Input = struct
  type t =
    | Command of Command.t
    | Output of Output.t
    [@@deriving sexp_of]

  let parse_line line = match String.split ~on:' ' line with
    | "$" :: rest -> Command (Command.parse_line rest)
    | rest -> Output (Output.parse_line rest)
  ;;

end

module State = struct
  type t =
    { children : string list String.Map.t
    ; parents : string String.Map.t
    ; directories : int option String.Map.t
    ; files : int String.Map.t
    ; current : string
    } [@@deriving sexp]
  ;;

  let default =
    { children = String.Map.empty
    ; parents = String.Map.of_alist_exn [ "/", "/" ]
    ; directories = String.Map.of_alist_exn [ "/", None ]
    ; files = String.Map.empty
    ; current = ""
    }
  ;;


  let is_dir t name =
    t.directories
    |> Map.key_set
    |> Set.exists  ~f:(String.equal name)
  ;;

  let get_children t name =
    Map.find t.children name |> Option.value ~default:[]
  ;;

  let rec get_size t parent = function
    | [] -> t, Map.find_exn t.directories parent |> Option.value_exn
    | name :: rest ->
      let t, size =
      (match is_dir t name with
      | false ->
        let size = Map.find_exn t.files name in
        t, size
      | true ->
        get_size t name (get_children t name))
      in
      let current = Map.find_exn t.directories parent |> Option.value ~default:0 in
      let directories = Map.set t.directories ~key:parent ~data:(Some (current + size)) in
      get_size { t with directories } parent rest
  ;;

  let update t name =
    let parents = Map.set t.parents ~key:name ~data:t.current in
    let children =
      Map.update t.children t.current ~f:(
        fun c -> name :: (c |> Option.value ~default:[])
      )
    in
    { t with parents; children }
  ;;

  let rec make t = function
    | [] -> t
    | Input.Command Command.Cd name  ::
      Input.Command Command.Ls ::
      rest ->
        let current =
          if String.equal t.current "" then name
          else t.current ^ name ^ "/"
        in
      make { t with current } rest
    | Input.Output Output.Dir name :: rest ->
      let name = t.current ^ name ^ "/" in
      let directories = Map.add_exn t.directories ~key:name ~data:None in
      let t = update t name in
      make { t with directories } rest
    | Input.Output Output.File (size, name) :: rest ->
      let name = t.current ^ name in
      let files = Map.set t.files ~key:name ~data:size in
      let t = update t name in
      make { t with  files } rest
    | Input.Command Command.Cd_up :: rest ->
      let current = (Map.find_exn t.parents t.current) in
      make { t with current } rest
    | _ -> failwith "invalid"
  ;;
end

let get_sizes t =
  let dir = "/" in
  let t, _ = State.get_size t dir (State.get_children t dir) in
  t
;;

let lines = Advent.read_lines "day07" ~for_tests:
  [ "$ cd /"
  ; "$ ls"
  ; "dir a"
  ; "14848514 b.txt"
  ; "8504156 c.dat"
  ; "dir d"
  ; "$ cd a"
  ; "$ ls"
  ; "dir e"
  ; "29116 f"
  ; "2557 g"
  ; "62596 h.lst"
  ; "$ cd e"
  ; "$ ls"
  ; "584 i"
  ; "$ cd .."
  ; "$ cd .."
  ; "$ cd d"
  ; "$ ls"
  ; "4060174 j"
  ; "8033020 d.log"
  ; "5626152 d.ext"
  ; "7214296 k" ] ()
;;

let parsed_lines = lines
  |> List.map ~f:Input.parse_line
;;

let%expect_test "" =
  parsed_lines
  |> List.iter ~f:(fun l -> print_s [%sexp (l : Input.t)]);
  [%expect {|
    (Command (Cd /))
    (Command Ls)
    (Output (Dir a))
    (Output (File 14848514 b.txt))
    (Output (File 8504156 c.dat))
    (Output (Dir d))
    (Command (Cd a))
    (Command Ls)
    (Output (Dir e))
    (Output (File 29116 f))
    (Output (File 2557 g))
    (Output (File 62596 h.lst))
    (Command (Cd e))
    (Command Ls)
    (Output (File 584 i))
    (Command Cd_up)
    (Command Cd_up)
    (Command (Cd d))
    (Command Ls)
    (Output (File 4060174 j))
    (Output (File 8033020 d.log))
    (Output (File 5626152 d.ext))
    (Output (File 7214296 k)) |}]
;;

let%expect_test "" =
  let t =
    parsed_lines
    |> State.make State.default
  in
  print_s [%message (t : State.t) ];
  [%expect {|
    (t
     ((children
       ((/ (/d/ /c.dat /b.txt /a/)) (/a/ (/a/h.lst /a/g /a/f /a/e/))
        (/a/e/ (/a/e/i)) (/d/ (/d/k /d/d.ext /d/d.log /d/j))))
      (parents
       ((/ /) (/a/ /) (/a/e/ /a/) (/a/e/i /a/e/) (/a/f /a/) (/a/g /a/)
        (/a/h.lst /a/) (/b.txt /) (/c.dat /) (/d/ /) (/d/d.ext /d/)
        (/d/d.log /d/) (/d/j /d/) (/d/k /d/)))
      (directories ((/ ()) (/a/ ()) (/a/e/ ()) (/d/ ())))
      (files
       ((/a/e/i 584) (/a/f 29116) (/a/g 2557) (/a/h.lst 62596) (/b.txt 14848514)
        (/c.dat 8504156) (/d/d.ext 5626152) (/d/d.log 8033020) (/d/j 4060174)
        (/d/k 7214296)))
      (current /d/))) |}]
;;

let%expect_test "" =
  let t = parsed_lines |> State.make State.default in
  let dir = "/" in
  let t, size = State.get_size t dir (State.get_children t dir) in
  print_s [%message (t: State.t) (size: int)];
  [%expect {|
    ((t
      ((children
        ((/ (/d/ /c.dat /b.txt /a/)) (/a/ (/a/h.lst /a/g /a/f /a/e/))
         (/a/e/ (/a/e/i)) (/d/ (/d/k /d/d.ext /d/d.log /d/j))))
       (parents
        ((/ /) (/a/ /) (/a/e/ /a/) (/a/e/i /a/e/) (/a/f /a/) (/a/g /a/)
         (/a/h.lst /a/) (/b.txt /) (/c.dat /) (/d/ /) (/d/d.ext /d/)
         (/d/d.log /d/) (/d/j /d/) (/d/k /d/)))
       (directories
        ((/ (48381165)) (/a/ (94853)) (/a/e/ (584)) (/d/ (24933642))))
       (files
        ((/a/e/i 584) (/a/f 29116) (/a/g 2557) (/a/h.lst 62596) (/b.txt 14848514)
         (/c.dat 8504156) (/d/d.ext 5626152) (/d/d.log 8033020) (/d/j 4060174)
         (/d/k 7214296)))
       (current /d/)))
     (size 48381165)) |}]
;;

let find_directory_to_delete (t: State.t) goal =
  let root_size = Map.find_exn t.directories "/" |> Option.value_exn in
  let required_space = goal - (70000000 - root_size) in
  t.directories
  |> Map.to_alist
  |> List.map ~f:(fun (name, v) -> name, v |> Option.value_exn)
  |> List.filter ~f:(fun (_, v) -> v >= required_space)
  |> List.sort ~compare:(fun (_, left) (_, right) -> left - right)
  |> List.hd_exn
;;

let%expect_test "" =
  let t = parsed_lines |> State.make State.default |> get_sizes in
  let to_delete = find_directory_to_delete t 30000000 in
  print_s [%message (to_delete: string * int)];
  [%expect {|
    (to_delete (/d/ 24933642)) |}]
;;

module T : sig
  include Day.T
end = struct
  let name = "--- Day 7: No Space Left On Device ---"
  let part1 =
    let t = parsed_lines |> State.make State.default |> get_sizes in
    t.directories
    |> Map.data
    |> List.map ~f:(fun v -> v |> Option.value_exn)
    |> List.filter ~f:(fun v -> v < 100000)
    |> List.sum (module Int) ~f:Fn.id
  ;;
  let part2 =
    let t = parsed_lines |> State.make State.default |> get_sizes in
    let _, size = find_directory_to_delete t 30000000 in
    size
  ;;

  let%expect_test "" =
    print_s [%message (part1: int) (part2: int)];
    [%expect {| ((part1 95437) (part2 24933642)) |}]
  ;;
end
