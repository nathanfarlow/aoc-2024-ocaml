open! Core
open! Common

module Pos = struct
  type t = int * int

  include Tuple.Comparable (Int) (Int)
end

let find_guard =
  Array.find_mapi_exn ~f:(fun i ->
      Array.find_mapi ~f:(fun j -> function `Guard -> Some (i, j) | _ -> None))

let part1 grid =
  let rec step (i, j) (dy, dx) visited =
    let visited = Set.add visited (i, j) in
    let f_i, f_j = (i + dy, j + dx) in
    match Array.get_opt grid f_i >>= fun row -> Array.get_opt row f_j with
    | None -> visited
    | Some (`Empty | `Guard) -> step (f_i, f_j) (dy, dx) visited
    | Some `Obstacle -> step (i, j) (dx, -dy) visited
  in
  step (find_guard grid) (-1, 0) (Set.empty (module Pos))
  |> Set.length |> print_int

let part2 _ = failwith ""

let parse =
  let open Angstrom in
  let one =
    char '.'
    >>| (fun _ -> `Empty)
    <|> (char '#' >>| fun _ -> `Obstacle)
    <|> (char '^' >>| fun _ -> `Guard)
  in
  many (many_till one end_of_line >>| Array.of_list) >>| Array.of_list |> exec

let () = run_with_input_file ~part1 ~part2 ~parse
