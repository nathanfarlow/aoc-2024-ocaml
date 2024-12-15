open! Core
open! Common

type entry = Wall | Empty | Robot | Box [@@deriving equal, sexp_of]
type dir = Up | Down | Left | Right [@@deriving equal, sexp_of]

let dir_to_point = function
  | Up -> (-1, 0)
  | Down -> (1, 0)
  | Left -> (0, -1)
  | Right -> (0, 1)

type input = { grid : entry array array; instructions : dir list }
[@@deriving sexp_of]

module Part1 = struct
  let score grid =
    Grid.find_all ~equal:equal_entry grid Box
    |> sum ~f:(fun (i, j) -> (100 * i) + j)

  let f input =
    let grid = Grid.copy input.grid in
    let rec push pos dir =
      let next_pos = Point.O.(pos + dir_to_point dir) in
      let can_push =
        match Grid.get grid next_pos with
        | Wall -> false
        | Empty | Robot -> true
        | Box -> push next_pos dir
      in
      if can_push then (
        Grid.set grid next_pos (Grid.get grid pos);
        Grid.set grid pos Empty);
      can_push
    in
    List.iter input.instructions ~f:(fun dir ->
        let start_pos = Grid.find_exn ~equal:equal_entry grid Robot in
        push start_pos dir |> ignore);
    score grid |> print_int
end

module Part2 = struct
  module Entry2 = struct
    type t = Wall | Empty | Robot | Left_box | Right_box
    [@@deriving equal, sexp_of]
  end

  let make_grid grid =
    Array.map grid
      ~f:
        (Array.concat_map ~f:(function
          | Wall -> [| Entry2.Wall; Wall |]
          | Empty -> [| Empty; Empty |]
          | Robot -> [| Robot; Empty |]
          | Box -> [| Left_box; Right_box |]))

  let score grid =
    Grid.find_all ~equal:Entry2.equal grid Entry2.Left_box
    |> sum ~f:(fun (i, j) -> (100 * i) + j)

  let f input =
    let open Entry2 in
    let grid = make_grid input.grid in
    let rec push pos dir =
      let next_pos = Point.O.(pos + dir_to_point dir) in
      let is_vert =
        match dir with Up | Down -> true | Left | Right -> false
      in
      let both f1 f2 =
        Option.both f1 f2 >>| fun (f1, f2) () ->
        f1 ();
        f2 ()
      in
      let continuation =
        match Grid.get grid next_pos with
        | Wall -> None
        | Empty | Robot -> Some (fun () -> ())
        | Left_box when is_vert ->
            let right_box = Point.O.(next_pos + (0, 1)) in
            both (push right_box dir) (push next_pos dir)
        | Right_box when is_vert ->
            let left_box = Point.O.(next_pos + (0, -1)) in
            both (push left_box dir) (push next_pos dir)
        | Left_box | Right_box -> push next_pos dir
      in
      both continuation
        (Some
           (fun () ->
             (* JANK! *)
             match Grid.get grid next_pos with
             | Empty ->
                 Grid.set grid next_pos (Grid.get grid pos);
                 Grid.set grid pos Empty
             | _ -> ()))
    in
    List.iter input.instructions ~f:(fun dir ->
        let start_pos = Grid.find_exn ~equal:Entry2.equal grid Robot in
        push start_pos dir |> Option.iter ~f:(fun f -> f ()));
    score grid |> print_int
end

let parse =
  let open Angstrom in
  let lift assoc =
    List.map assoc ~f:(fun (c, v) -> char c >>| fun _ -> v)
    |> List.reduce_exn ~f:( <|> )
  in
  let grid =
    let char =
      [ ('#', Wall); ('.', Empty); ('@', Robot); ('O', Box) ] |> lift
    in
    many_till (many_till char end_of_line >>| Array.of_list) end_of_line
    >>| Array.of_list
  in
  let instructions =
    let direction =
      [ ('^', Up); ('v', Down); ('<', Left); ('>', Right) ] |> lift
    in
    sep_by1 end_of_line (many direction) >>| List.concat
  in
  lift2 (fun grid instructions -> { grid; instructions }) grid instructions
  |> exec ~consume:Prefix

let () = run_with_input_file ~part1:Part1.f ~part2:Part2.f ~parse
