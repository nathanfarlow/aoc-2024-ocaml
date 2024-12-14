open! Core
open! Common

let regions grid =
  let visited = Hash_set.create (module Point) in
  let rec region pos =
    if Hash_set.mem visited pos then Set.empty (module Point)
    else (
      Hash_set.add visited pos;
      let rest =
        Grid.neighbors grid pos
        |> List.filter ~f:(fun (_, neighbor) ->
               Char.equal (Grid.get grid pos) neighbor)
        |> List.map ~f:(fun (n, _) -> region n)
        |> Set.union_list (module Point)
      in
      Set.add rest pos)
  in
  Array.mapi grid ~f:(fun i row ->
      Array.mapi row ~f:(fun j _ -> region (i, j)) |> Array.to_list)
  |> Array.to_list |> List.concat
  |> List.filter ~f:(Fn.non Set.is_empty)

let area region = Set.length region

let part1 grid =
  let perimeter region =
    Set.to_list region
    |> sum ~f:(fun (i, j) ->
           [ (i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1) ]
           |> List.count ~f:(Fn.non (Set.mem region)))
  in
  sum (regions grid) ~f:(fun region -> area region * perimeter region)
  |> print_int

module Key = struct
  module T = struct
    type t = { pos : Point.t; dir : Point.t } [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)
end

let add (a, b) (c, d) = (a + c, b + d)
let neg (a, b) = (-a, -b)
let swap (a, b) = (b, a)
let is_side region pos dir = not (Set.mem region (add pos dir))

let part2 grid =
  let num_sides region =
    let visited = Hash_set.create (module Key) in
    let rec mark pos dir =
      match
        ( Hash_set.mem visited { Key.pos; dir },
          Set.mem region pos,
          Set.mem region (add pos dir) )
      with
      | false, true, false ->
          Hash_set.add visited { Key.pos; dir };
          let swapped = swap dir in
          mark (add pos swapped) dir;
          mark (add pos (neg swapped)) dir
      | _ -> ()
    in
    let answer = ref 0 in
    let visit pos =
      [ (0, 1); (1, 0); (0, -1); (-1, 0) ]
      |> List.iter ~f:(fun dir ->
             match
               (is_side region pos dir, Hash_set.mem visited { Key.pos; dir })
             with
             | true, false ->
                 incr answer;
                 mark pos dir
             | _ -> ())
    in
    Set.iter region ~f:visit;
    !answer
  in
  regions grid
  |> sum ~f:(fun region -> area region * num_sides region)
  |> print_int

let parse =
  let open Angstrom in
  many (many_till any_char end_of_line >>| Array.of_list)
  >>| Array.of_list |> exec

let () = run_with_input_file ~part1 ~part2 ~parse
