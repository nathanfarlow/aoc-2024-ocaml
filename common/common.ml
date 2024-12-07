open! Core
module Math = Math
include Option.Let_syntax
open Angstrom

let ( >> ) f g x = g (f x)
let space = skip_while (function ' ' | '\t' -> true | _ -> false)
let integer = take_while1 Char.is_digit >>| Int.of_string
let eol = string "\n" >>| ignore

let exec ?(consume = Angstrom.Consume.All) parser s =
  parse_string ~consume parser s |> Result.ok_or_failwith

let ws = skip_while Char.is_whitespace

let exec_opt ?consume parser s =
  match exec ?consume parser s with x -> Some x | exception _ -> None

let skip_till p = fix (fun m -> p <|> any_char *> m)

let zip_next l =
  let rec aux acc = function
    | [] | [ _ ] -> acc
    | x :: (y :: _ as xs) -> aux ((x, y) :: acc) xs
  in
  aux [] l |> List.rev

let%expect_test "zip_next" =
  zip_next [ 1; 2; 3; 4 ] |> [%sexp_of: (int * int) list] |> print_s;
  [%expect {| ((1 2) (2 3) (3 4)) |}]

module Array = struct
  include Array

  let get_opt arr i =
    if i < 0 || i >= Array.length arr then None
    else Some (Array.unsafe_get arr i)
end

let sum ~f = List.sum (module Int) ~f
let print_int = printf "%d\n"

let run_with_input_file ~part1 ~part2 ~parse =
  Command.basic ~summary:"Advent of code"
    (let%map_open.Command f =
       choose_one
         ~if_nothing_chosen:
           (Default_to
              (fun s ->
                print_endline "Part 1:";
                part1 s;
                print_endline "Part 2:";
                part2 s))
         [
           flag "-part1" no_arg ~doc:"Run part 1"
           |> map ~f:(fun b -> Option.some_if b part1);
           flag "-part2" no_arg ~doc:"Run part 2"
           |> map ~f:(fun b -> Option.some_if b part2);
         ]
     and input = anon ("input" %: string) in
     fun () -> In_channel.read_all input |> parse |> f)
  |> Command_unix.run
