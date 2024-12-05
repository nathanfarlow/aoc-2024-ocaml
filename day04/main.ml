open! Core
open! Common
open! Angstrom

let count_all s parsers =
  List.init (String.length s) ~f:(fun i ->
      let sub = String.drop_prefix s i in
      List.filter_map parsers ~f:(fun p -> exec_opt ~consume:Prefix p sub))
  |> List.sum (module Int) ~f:List.length

let part1 (s, width) =
  List.cartesian_product [ "XMAS"; "SAMX" ] [ 0; width - 1; width; width + 1 ]
  |> List.map ~f:(fun (s, n) ->
         String.to_list s
         |> List.map ~f:(fun c -> char c >>| ignore)
         |> List.intersperse ~sep:(take n >>| ignore)
         |> List.reduce_exn ~f:( *> ))
  |> count_all s |> print_int

let part2 (s, width) =
  let a = [ ('M', 'S'); ('S', 'M') ] in
  List.cartesian_product a a
  |> List.map ~f:(fun ((tl, br), (tr, bl)) ->
         [
           char tl *> any_char *> char tr;
           any_char *> char 'A' *> any_char;
           char bl *> any_char *> char br;
         ]
         |> List.map ~f:(map ~f:ignore)
         |> List.intersperse ~sep:(take (width - 2) >>| ignore)
         |> List.reduce_exn ~f:( *> ))
  |> count_all s |> print_int

let parse s =
  (s, exec (many_till any_char end_of_line) s ~consume:Prefix |> List.length)

let () = run_with_input_file ~part1 ~part2 ~parse
