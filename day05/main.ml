open! Core
open! Common

let violates_rule rules left right =
  match Map.find rules right with None -> false | Some xs -> Set.mem xs left

let sort rules arr =
  let arr = Array.copy arr in
  for _ = 0 to Array.length arr - 1 do
    for i = 0 to Array.length arr - 1 do
      for j = i + 1 to Array.length arr - 1 do
        if violates_rule rules arr.(i) arr.(j) then Array.swap arr i j
      done
    done
  done;
  arr

let is_update_good rules arr = Array.equal Int.equal arr (sort rules arr)

let sum_and_print =
  List.sum (module Int) ~f:(fun arr -> arr.(Array.length arr / 2)) >> print_int

let part1 (rules, updates) =
  List.filter updates ~f:(is_update_good rules) |> sum_and_print

let part2 (rules, updates) =
  List.filter updates ~f:(Fn.non (is_update_good rules))
  |> List.map ~f:(sort rules)
  |> sum_and_print

let parse =
  let open Angstrom in
  let rules =
    sep_by1 end_of_line
      (sep_by1 (char '|') integer >>| function [ a; b ] -> (a, b))
    >>| Map.of_alist_multi (module Int)
    >>| Map.map ~f:(Set.of_list (module Int))
  in
  let updates =
    sep_by1 end_of_line (sep_by1 (char ',') integer >>| Array.of_list)
  in
  lift2 Tuple2.create rules (ws *> updates) |> exec ~consume:Prefix

let () = run_with_input_file ~part1 ~part2 ~parse
