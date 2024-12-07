open! Core
open! Common

let part1 =
  let rec valid ~value ~target = function
    | [] -> value = target
    | x :: xs ->
        valid ~value:(value + x) ~target xs
        || valid ~value:(value * x) ~target xs
  in
  List.filter ~f:(fun (target, values) -> valid ~value:0 ~target values)
  >> sum ~f:fst >> print_int

let part2 =
  let rec valid ~value ~target = function
    | [] -> value = target
    | x :: xs ->
        valid ~value:(value + x) ~target xs
        || valid ~value:(value * x) ~target xs
        || valid
             ~value:Int.(of_string (to_string value ^ to_string x))
             ~target xs
  in
  List.filter ~f:(fun (target, values) -> valid ~value:0 ~target values)
  >> sum ~f:fst >> print_int

let parse =
  let open Angstrom in
  let line =
    let first = integer <* string ": " in
    let second = sep_by1 space integer in
    lift2 Tuple2.create first second
  in
  sep_by1 end_of_line line |> exec ~consume:Prefix

let () = run_with_input_file ~part1 ~part2 ~parse
