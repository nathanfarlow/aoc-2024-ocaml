open! Core
open! Common

let part1 input =
  let arr =
    List.concat_mapi input ~f:(fun id (file, hole) ->
        List.init file ~f:(fun _ -> Some id) @ List.init hole ~f:(fun _ -> None))
    |> Array.of_list
  in
  let rec aux acc left right =
    if left > right then acc
    else
      match (arr.(left), arr.(right)) with
      | Some val_, _ -> aux (acc + (left * val_)) (left + 1) right
      | None, Some val_ -> aux (acc + (left * val_)) (left + 1) (right - 1)
      | None, None -> aux acc left (right - 1)
  in
  aux 0 0 (Array.length arr - 1) |> print_int

let part2 input =
  let arr =
    List.concat_mapi input ~f:(fun id (file, hole) ->
        List.init file ~f:(fun _ -> Some id) @ List.init hole ~f:(fun _ -> None))
    |> Array.of_list
  in
  let rec copy cur left right =
    match (arr.(left), arr.(right)) with
    | _, r when not @@ [%equal: int option] r cur -> true
    | l, _ when [%equal: int option] l cur -> false
    | Some _, _ -> false
    | None, _ ->
        if copy cur (left + 1) (right - 1) then (
          arr.(left) <- cur;
          arr.(right) <- None;
          true)
        else false
  in
  List.rev input
  |> List.fold
       ~init:(Array.length arr - 1)
       ~f:(fun right (files, hole) ->
         let right = right - hole in
         for i = 0 to right do
           copy arr.(right) i right |> ignore
         done;
         right - files)
  |> ignore;
  Array.foldi arr ~init:0 ~f:(fun i acc a ->
      acc + (i * Option.value a ~default:0))
  |> print_int

let parse =
  let open Angstrom in
  let pair = lift2 Tuple2.create digit (digit <|> return 0) in
  many pair |> exec ~consume:Prefix

let () = run_with_input_file ~part1 ~part2 ~parse
