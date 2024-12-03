open! Core
open! Common
open! Angstrom

let part1 [ l1; l2 ] =
  let sort = List.sort ~compare:Int.compare in
  List.zip_exn (sort l1) (sort l2)
  |> List.sum (module Int) ~f:(fun (a, b) -> abs (a - b))
  |> printf "%d\n"

let part2 [ l1; l2 ] =
  let l2 =
    List.map l2 ~f:(fun x -> (x, ()))
    |> Map.of_alist_multi (module Int)
    |> Map.map ~f:List.length
  in
  List.sum
    (module Int)
    ~f:(fun x -> match Map.find l2 x with None -> 0 | Some n -> x * n)
    l1
  |> printf "%d\n"

let parse =
  many (many_till (integer <* space) eol) >>| List.transpose_exn |> parse

let () = run_with_input_file ~part1 ~part2 ~parse
