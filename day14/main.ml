open! Core
open! Common

type input = { pos : Point.t; vel : Point.t } [@@deriving sexp_of]

let ( mod ) a b = (b + (a mod b)) mod b
let tuple_mod (a, b) (x, y) = (a mod x, b mod y)

let get l w h (x, y) =
  List.filter l ~f:(fun { pos = px, py; _ } ->
      x <= px && px < x + (w / 2) && y <= py && py < y + (h / 2))

let quadrants (w, h) l =
  [ (0, 0); ((w / 2) + 1, 0); (0, (h / 2) + 1); ((w / 2) + 1, (h / 2) + 1) ]
  |> List.map ~f:(get l w h)

let prod ~f l = List.fold l ~init:1 ~f:(fun acc x -> acc * f x)
let bounds = (101, 103)

let step { pos; vel } =
  let pos = tuple_mod Point.O.(pos + vel) bounds in
  { pos; vel }

let part1 input =
  Sequence.range 0 100
  |> Sequence.fold ~init:input ~f:(fun l _ -> List.map ~f:step l)
  |> quadrants bounds |> prod ~f:List.length |> print_int

let part2 input =
  Sequence.range 1 100000
  |> Sequence.fold
       ~init:(input, (-1, Int.max_value))
       ~f:(fun (l, (best_idx, best_val)) i ->
         let l = List.map ~f:step l in
         let score = quadrants bounds l |> prod ~f:List.length in
         let best =
           if score < best_val then (i, score) else (best_idx, best_val)
         in
         (l, best))
  |> snd |> fst |> print_int

let parse =
  let open Angstrom in
  let pos =
    lift2 Tuple2.create
      (string "p=" *> integer <* char ',')
      (integer <* char ' ')
  in
  let vel =
    lift2 Tuple2.create
      (string "v=" *> integer <* char ',')
      (integer <* end_of_line)
  in
  let input = lift2 (fun pos vel -> { pos; vel }) pos vel in
  many input |> exec

let () = run_with_input_file ~part1 ~part2 ~parse
