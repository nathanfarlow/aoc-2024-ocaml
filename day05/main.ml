open! Core
open! Common
open! Angstrom

let is_update_good rules l =
  let is_first_elem_good = function
    | x :: xs ->
        List.for_all xs ~f:(fun y ->
            match Map.find rules y with
            | None -> true
            | Some ys -> not (Set.mem ys x))
  in
  List.init (List.length l) ~f:(List.drop l)
  |> List.for_all ~f:is_first_elem_good

let part1 (pairs, updates) =
  let rules =
    Map.of_alist_multi (module Int) pairs
    |> Map.map ~f:(Set.of_list (module Int))
  in
  List.filter updates ~f:(is_update_good rules)
  |> List.sum
       (module Int)
       ~f:(fun l ->
         let len = List.length l in
         List.nth_exn l (len / 2))
  |> print_int

let part2 _ = failwith ""

let parse =
  let pairs =
    sep_by1 eol (sep_by1 (char '|') integer >>| function [ a; b ] -> (a, b))
  in
  let updates = sep_by1 eol (sep_by1 (char ',') integer) in
  lift2 Tuple2.create pairs (ws *> updates) |> exec ~consume:Prefix

let () = run_with_input_file ~part1 ~part2 ~parse
