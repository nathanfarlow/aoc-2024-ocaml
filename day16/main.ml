open! Core
open! Common

module Vertex = struct
  type t = { pos : Point.t; dir : Point.t } [@@deriving compare, hash, sexp]
end

module G =
  Graph.Imperative.Digraph.AbstractLabeled
    (Vertex)
    (struct
      include Int

      let default = 0
    end)

module Path_weight = struct
  type edge = G.E.t
  type t = int

  let weight x = G.E.label x
  let zero = 0
  let add = ( + )
  let compare = compare
end

module Dij = Graph.Path.Dijkstra (G) (Path_weight)

type input = {
  grid : [ `Wall | `Empty ] Grid.t;
  start : Point.t;
  end_ : Point.t;
}
[@@deriving sexp_of]

let score ~dir:(y, x) ~new_dir =
  List.Assoc.find_exn
    [ ((y, x), 0); ((x, -y), 1000); ((-y, -x), 2000); ((-x, y), 1000) ]
    ~equal:Point.equal new_dir

let dirs = Point.[ up; down; left; right ]

let iter grid f =
  Array.iteri grid ~f:(fun i ->
      printf "%d/%d\n%!" i (Array.length grid);
      Array.iteri ~f:(fun j -> function
        | `Wall -> () | `Empty -> List.iter dirs ~f:(fun dir -> f (i, j) dir)))

let make_graph grid =
  let g = G.create () in
  let nodes = Hashtbl.create (module Vertex) in
  iter grid (fun pos dir ->
      let key = { Vertex.pos; dir } in
      let data = G.V.create key in
      Hashtbl.add_exn nodes ~key ~data;
      G.add_vertex g data);
  iter grid (fun pos dir ->
      List.iter (Grid.neighbors grid pos) ~f:(fun (n_pos, n) ->
          match n with
          | `Wall -> ()
          | `Empty ->
              let from = Hashtbl.find_exn nodes { Vertex.pos; dir } in
              let new_dir = Point.O.(n_pos - pos) in
              let to_ =
                Hashtbl.find_exn nodes { Vertex.pos = n_pos; dir = new_dir }
              in
              let score = score ~dir ~new_dir in
              let edge = G.E.create from (score + 1) to_ in
              G.add_edge_e g edge));
  (g, Hashtbl.find_exn nodes)

let find_shortest (start_pos, start_dir) (end_pos, end_dir) (g, find_node) =
  let start = find_node { Vertex.pos = start_pos; dir = start_dir } in
  let end_ = find_node { Vertex.pos = end_pos; dir = end_dir } in
  Option.try_with (fun () ->
      Dij.shortest_path g start end_ |> fst |> sum ~f:Path_weight.weight)

let find_shortest_any_dir start end_ graph =
  List.filter_map dirs ~f:(fun dir -> find_shortest start (end_, dir) graph)
  |> List.min_elt ~compare:Int.compare

let part1 { grid; start; end_ } =
  make_graph grid
  |> find_shortest_any_dir (start, Point.right) end_
  |> Option.value_exn |> print_int

let part2 { grid; start; end_ } =
  let graph = make_graph grid in
  let best =
    find_shortest_any_dir (start, Point.right) end_ graph |> Option.value_exn
  in
  let valid = Hash_set.create (module Point) in
  iter grid (fun pos dir ->
      let start_to_here = find_shortest (start, Point.right) (pos, dir) graph in
      let here_to_end = find_shortest_any_dir (pos, dir) end_ graph in
      match (start_to_here, here_to_end) with
      | Some a, Some b when a + b = best -> Hash_set.add valid pos
      | _ -> ());
  print_int (Hash_set.length valid)

let parse s =
  let open Angstrom in
  let open struct
    type cell = Start | End | Wall | Empty [@@deriving equal]
  end in
  let lift assoc =
    List.map assoc ~f:(fun (c, v) -> char c >>| fun _ -> v)
    |> List.reduce_exn ~f:( <|> )
  in
  let grid =
    let char =
      [ ('#', Wall); ('.', Empty); ('S', Start); ('E', End) ] |> lift
    in
    many (many_till char end_of_line >>| Array.of_list) >>| Array.of_list
  in
  let grid = exec grid s in
  let start, end_ =
    let find = Grid.find_exn ~equal:equal_cell grid in
    (find Start, find End)
  in
  let grid =
    Array.map grid
      ~f:
        (Array.map ~f:(function
          | Wall -> `Wall
          | Empty | Start | End -> `Empty))
  in
  { grid; start; end_ }

let () = run_with_input_file ~part1 ~part2 ~parse
