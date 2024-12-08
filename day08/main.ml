open! Core
open! Common

let find_antennas grid =
  let letters = Hashtbl.create (module Char) in
  Array.iteri grid ~f:(fun i ->
      Array.iteri ~f:(fun j -> function
        | '.' -> ()
        | cell ->
            let locs = Hashtbl.find letters cell |> Option.value ~default:[] in
            Hashtbl.set letters ~key:cell ~data:((i, j) :: locs)));
  letters

let part1 grid =
  let antennas = find_antennas grid in
  let locs = Hash_set.create (module Point) in
  Hashtbl.iter antennas ~f:(fun l ->
      Sequence.iter (triangular l) ~f:(fun ((y1, x1), tl) ->
          List.iter tl ~f:(fun (y2, x2) ->
              let dy, dx = (y2 - y1, x2 - x1) in
              Hash_set.add locs (y1 - dy, x1 - dx);
              Hash_set.add locs (y2 + dy, x2 + dx))));
  let locs = Hash_set.to_list locs |> List.filter ~f:(Grid.in_bounds grid) in
  List.length locs |> print_int

let part2 _ = ()

let parse =
  let open Angstrom in
  many (many_till any_char end_of_line >>| Array.of_list)
  >>| Array.of_list |> exec

let () = run_with_input_file ~part1 ~part2 ~parse
