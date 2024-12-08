open! Core
open! Common

module Loc = struct
  module T = struct
    type t = { i : int; j : int; dx : int; dy : int } [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let find_guard =
  Array.find_mapi_exn ~f:(fun i ->
      Array.find_mapi ~f:(fun j -> function `Guard -> Some (i, j) | _ -> None))

let simulate grid =
  let rec step loc visited =
    match Set.mem visited loc with
    | true -> `Cycle
    | false -> (
        let visited = Set.add visited loc in
        let forward = Loc.{ loc with i = loc.i + loc.dy; j = loc.j + loc.dx } in
        match Grid.get_opt grid (forward.i, forward.j) with
        | None ->
            let unique_positions =
              Set.map (module Point) visited ~f:(fun { i; j; _ } -> (i, j))
            in
            `Off_edge unique_positions
        | Some (`Empty | `Guard) -> step forward visited
        | Some `Obstacle ->
            let rotate = Loc.{ loc with dx = -loc.dy; dy = loc.dx } in
            step rotate visited)
  in
  let i, j = find_guard grid in
  step Loc.{ i; j; dy = -1; dx = 0 } (Set.empty (module Loc))

let part1 grid =
  match simulate grid with
  | `Off_edge visited -> Set.length visited |> print_int

let part2 grid =
  let num = ref 0 in
  for i = 0 to Array.length grid - 1 do
    for j = 0 to Array.length grid.(0) - 1 do
      let prev = grid.(i).(j) in
      match prev with
      | `Empty ->
          (grid.(i).(j) <- `Obstacle;
           match simulate grid with
           | `Cycle -> num := !num + 1
           | `Off_edge _ -> ());
          grid.(i).(j) <- prev
      | _ -> ()
    done
  done;
  print_int !num

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
