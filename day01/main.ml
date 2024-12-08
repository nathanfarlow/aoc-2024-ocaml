open! Core
open! Common
open! Angstrom

let part1 [ l1; l2 ] =
  let sort = List.sort ~compare:Int.compare in
  List.zip_exn (sort l1) (sort l2)
  |> List.sum (module Int) ~f:(fun (a, b) -> abs (a - b))
  |> printf "%d\n"

let part2 [ l1; l2 ] =
  let occurences =
    List.sort_and_group ~compare:Int.compare l2
    |> List.map ~f:(fun l -> (List.hd_exn l, List.length l))
    |> Map.of_alist_exn (module Int)
  in
  List.sum
    (module Int)
    ~f:(fun x -> match Map.find occurences x with None -> 0 | Some n -> x * n)
    l1
  |> printf "%d\n"

let parse =
  many (many_till (integer <* space) end_of_line) >>| List.transpose_exn |> exec

let () = run_with_input_file ~part1 ~part2 ~parse
