open! Core
open! Common

type puzzle = { a : Point.t; b : Point.t; prize : Point.t } [@@deriving sexp_of]

let cost (a, b) = (3 * a) + b

let part1 puzzles =
  let compare = Comparable.lift Int.compare ~f:cost in
  let solve { a = ax, ay; b = bx, by; prize = px, py } =
    let seq = Sequence.range 0 101 in
    let go f = Sequence.filter_map seq ~f |> Sequence.min_elt ~compare in
    go (fun a_times ->
        go (fun b_times ->
            let ax, ay = (a_times * ax, a_times * ay) in
            let bx, by = (b_times * bx, b_times * by) in
            let x, y = (ax + bx, ay + by) in
            Option.some_if (x = px && y = py) (a_times, b_times)))
  in
  let answer = List.filter_map puzzles ~f:solve |> sum ~f:cost in
  print_int answer

let part2 puzzles =
  let x = Lp.var ~integer:true "x" in
  let y = Lp.var ~integer:true "y" in
  let solve { a; b; prize } =
    let m = 10000000000000 in
    let as_floats = Tuple2.map ~f:Float.of_int in
    let ax, ay = as_floats a in
    let bx, by = as_floats b in
    let px, py = Tuple2.map prize ~f:(( + ) m) |> as_floats in
    let open Lp in
    let obj = minimize ((c 3. *~ x) ++ y) in
    let c0 = (c ax *~ x) ++ (c bx *~ y) =~ c px in
    let c1 = (c ay *~ x) ++ (c by *~ y) =~ c py in
    make obj [ c0; c1 ] |> Lp_glpk.solve ~term_output:false |> function
    | Ok (_, xs) -> Some PMap.(find x xs, find y xs)
    | Error _ -> None
  in
  List.filter_map puzzles ~f:solve
  |> sum ~f:(Tuple2.map ~f:Float.to_int >> cost)
  |> print_int

let parse =
  let open Angstrom in
  let button =
    lift2 Tuple2.create
      (string "Button " *> any_char *> string ": X+" *> integer)
      (string ", Y+" *> integer <* end_of_line)
  in
  let prize =
    lift2 Tuple2.create
      (string "Prize: X=" *> integer)
      (string ", Y=" *> integer <* end_of_line)
  in
  let puzzle = lift3 (fun a b prize -> { a; b; prize }) button button prize in
  sep_by1 ws puzzle |> exec

let () = run_with_input_file ~part1 ~part2 ~parse
