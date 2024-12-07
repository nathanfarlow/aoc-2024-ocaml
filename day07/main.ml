open! Core
open! Common

let go ops =
  let rec valid ~value ~target = function
    | [] -> value = target
    | x :: xs ->
        List.exists ops ~f:(fun op -> valid ~value:(op value x) ~target xs)
  in
  List.filter ~f:(fun (target, values) -> valid ~value:0 ~target values)
  >> sum ~f:fst >> print_int

let part1 = go [ ( + ); ( * ) ]

let part2 =
  let ( || ) a b = Int.of_string (sprintf "%d%d" a b) in
  go [ ( + ); ( * ); ( || ) ]

let parse =
  let open Angstrom in
  let line =
    let target = integer <* string ": " in
    let values = sep_by1 space integer in
    lift2 Tuple2.create target values
  in
  sep_by1 end_of_line line |> exec ~consume:Prefix

let () = run_with_input_file ~part1 ~part2 ~parse
