open! Core
open! Common
open! Angstrom

let increasing (x, y) = x < y
let decreasing (x, y) = x > y

let valid_diff (x, y) =
  let diff = abs (y - x) in
  1 <= diff && diff <= 3

let report_is_safe report =
  let adj_pairs = zip_next report in
  let all f = List.for_all adj_pairs ~f in
  (all increasing || all decreasing) && all valid_diff

module Part1 = struct
  let f reports = sum reports ~f:(report_is_safe >> Bool.to_int) |> print_int
end

module Part2 = struct
  let drop_each =
    let rec aux prev = function
      | [] -> []
      | hd :: tl -> (prev @ tl) :: aux (prev @ [ hd ]) tl
    in
    aux []

  let f reports =
    List.map reports ~f:(fun report ->
        List.exists (drop_each report) ~f:report_is_safe)
    |> sum ~f:Bool.to_int |> print_int
end

let parse = many (many_till (integer <* space) eol) |> exec
let () = run_with_input_file ~part1:Part1.f ~part2:Part2.f ~parse
