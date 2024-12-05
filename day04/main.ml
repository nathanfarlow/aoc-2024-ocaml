open! Core
open! Common

let part1 s =
  let open Angstrom in
  let width =
    exec (many_till any_char end_of_line) s ~consume:Prefix |> List.length
  in
  let parsers =
    List.concat_map
      [ width - 1; width; width + 1; 0 ]
      ~f:(fun n ->
        List.map [ "XMAS"; "SAMX" ]
          ~f:
            (String.to_list
            >> List.map ~f:(fun c -> char c >>| ignore)
            >> List.intersperse ~sep:(take n >>| ignore)
            >> List.reduce_exn ~f:( *> )))
  in
  List.init (String.length s) ~f:(fun i ->
      let sub = String.drop_prefix s i in
      List.filter_map parsers ~f:(fun p -> exec_opt ~consume:Prefix p sub))
  |> List.sum (module Int) ~f:List.length
  |> print_int

let part2 _ = failwith ""
let () = run_with_input_file ~part1 ~part2 ~parse:Fn.id
