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

let go ~start ~end_ grid =
  let antennas = find_antennas grid in
  let locs = Hash_set.create (module Point) in
  Hashtbl.iter antennas ~f:(fun antenna_group ->
      Sequence.iter (all_pairs antenna_group) ~f:(fun ((y1, x1), (y2, x2)) ->
          for i = start to end_ do
            let dy, dx = (i * (y2 - y1), i * (x2 - x1)) in
            Hash_set.add locs (y1 - dy, x1 - dx);
            Hash_set.add locs (y2 + dy, x2 + dx)
          done));
  let locs = Hash_set.to_list locs |> List.filter ~f:(Grid.in_bounds grid) in
  List.length locs |> print_int

let part1 = go ~start:1 ~end_:1
let part2 grid = go ~start:0 ~end_:(Grid.width grid + Grid.height grid) grid

let parse =
  let open Angstrom in
  many (many_till any_char end_of_line >>| Array.of_list)
  >>| Array.of_list |> exec

let () = run_with_input_file ~part1 ~part2 ~parse
