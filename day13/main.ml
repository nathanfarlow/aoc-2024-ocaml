open! Core
open! Common

type puzzle = { a : Point.t; b : Point.t; prize : Point.t } [@@deriving sexp_of]

let solve =
  let x = Lp.var ~integer:true "x" in
  let y = Lp.var ~integer:true "y" in
  fun { a; b; prize } ->
    let as_floats = Tuple2.map ~f:Float.of_int in
    let ax, ay = as_floats a in
    let bx, by = as_floats b in
    let px, py = as_floats prize in
    let open Lp in
    let obj = minimize ((c 3. *~ x) ++ y) in
    let c0 = (c ax *~ x) ++ (c bx *~ y) =~ c px in
    let c1 = (c ay *~ x) ++ (c by *~ y) =~ c py in
    make obj [ c0; c1 ] |> Lp_glpk.solve ~term_output:false |> function
    | Ok (_, xs) -> Some PMap.(find x xs, find y xs)
    | Error _ -> None

let go =
  List.filter_map ~f:solve
  >> sum ~f:(Tuple2.map ~f:Float.to_int >> fun (a, b) -> (3 * a) + b)
  >> print_int

let part1 = go

let part2 =
  List.map ~f:(fun p ->
      { p with prize = Tuple2.map p.prize ~f:(( + ) 10000000000000) })
  >> go

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
