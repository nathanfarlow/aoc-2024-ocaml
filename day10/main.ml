open! Core
open! Common

let part1 grid =
  let score grid start =
    let aux =
      Memo.recursive ~hashable:Point.hashable (fun aux pos ->
          match Grid.get_opt grid pos with
          | None -> Set.empty (module Point)
          | Some 9 -> Set.singleton (module Point) pos
          | Some cur ->
              Grid.neighbors grid pos
              |> List.filter ~f:(fun (_, neighbor) -> neighbor = cur + 1)
              |> List.map ~f:(fst >> aux)
              |> Set.union_list (module Point))
    in
    aux start |> Set.length
  in
  Grid.find_all grid 0 ~equal:Int.equal |> sum ~f:(score grid) |> print_int

let part2 grid =
  let score grid start =
    let aux =
      Memo.recursive ~hashable:Point.hashable (fun aux pos ->
          match Grid.get_opt grid pos with
          | None -> 0
          | Some 9 -> 1
          | Some cur ->
              Grid.neighbors grid pos
              |> List.filter ~f:(fun (_, neighbor) -> neighbor = cur + 1)
              |> sum ~f:(fst >> aux))
    in
    aux start
  in
  Grid.find_all grid 0 ~equal:Int.equal |> sum ~f:(score grid) |> print_int

let parse =
  let open Angstrom in
  many (many_till digit end_of_line >>| Array.of_list) >>| Array.of_list |> exec

let () = run_with_input_file ~part1 ~part2 ~parse
