open! Core
open! Common
open! Angstrom

let part1 =
  List.sum (module Int) ~f:(function `Mul (a, b) -> a * b | _ -> 0)
  >> print_int

let part2 =
  List.fold ~init:(true, 0) ~f:(fun (do_, acc) -> function
    | `Mul (a, b) when do_ -> (do_, acc + (a * b))
    | `Mul _ -> (do_, acc)
    | `Do -> (true, acc)
    | `Don't -> (false, acc))
  >> snd >> print_int

let parse =
  let mul =
    lift2 Tuple2.create
      (string "mul(" *> integer <* char ',')
      (integer <* char ')')
    >>| fun a -> `Mul a
  in
  let do_ = string "do()" >>| fun _ -> `Do in
  let don't = string "don't()" >>| fun _ -> `Don't in
  many (skip_till (mul <|> do_ <|> don't)) |> exec ~consume:Prefix

let () = run_with_input_file ~part1 ~part2 ~parse
