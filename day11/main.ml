open! Core
open! Common

let split stone =
  let digits = Int.to_string stone |> String.to_list in
  if List.length digits % 2 = 0 then
    let first, second = List.split_n digits (List.length digits / 2) in
    let to_int = String.of_char_list >> Int.of_string in
    Some (to_int first, to_int second)
  else None

let go n =
  let blink =
    Memo.recursive ~hashable:Point.hashable (fun blink (stone, remaining) ->
        if remaining = 0 then 1
        else if stone = 0 then blink (1, remaining - 1)
        else
          match split stone with
          | Some (a, b) -> blink (a, remaining - 1) + blink (b, remaining - 1)
          | None -> blink (stone * 2024, remaining - 1))
  in
  sum ~f:(fun stone -> blink (stone, n)) >> print_int

let part1 = go 25
let part2 = go 75

let parse =
  let open Angstrom in
  sep_by1 ws integer |> exec ~consume:Prefix

let () = run_with_input_file ~part1 ~part2 ~parse
