open! Core
open! Common

let part1 (_, arr) =
  let rec aux acc left right =
    if left > right then acc
    else
      match (arr.(left), arr.(right)) with
      | Some val_, _ -> aux (acc + (left * val_)) (left + 1) right
      | None, Some val_ -> aux (acc + (left * val_)) (left + 1) (right - 1)
      | None, None -> aux acc left (right - 1)
  in
  aux 0 0 (Array.length arr - 1) |> print_int

let part2 (pairs, arr) =
  let rec copy cur left right =
    if left > right then false
    else
      match (arr.(left), arr.(right)) with
      | _, r when not @@ [%equal: int option] r cur -> true
      | Some _, _ -> false
      | None, _ ->
          if copy cur (left + 1) (right - 1) then (
            arr.(left) <- cur;
            arr.(right) <- None;
            true)
          else false
  in
  List.rev pairs
  |> List.fold
       ~init:(Array.length arr - 1)
       ~f:(fun right (files, holes) ->
         let right = right - holes in
         for i = 0 to right do
           copy arr.(right) i right |> ignore
         done;
         right - files)
  |> ignore;
  Array.fold arr ~init:0 ~f:(fun acc elem -> acc + Option.value elem ~default:0)
  |> print_int

let parse s =
  let open Angstrom in
  let pair = lift2 Tuple2.create digit (digit <|> return 0) in
  let pairs = exec (many pair) s ~consume:Prefix in
  let arr =
    List.concat_mapi pairs ~f:(fun id (file, hole) ->
        List.init file ~f:(fun _ -> Some id) @ List.init hole ~f:(fun _ -> None))
    |> Array.of_list
  in
  (pairs, arr)

let () = run_with_input_file ~part1 ~part2 ~parse
