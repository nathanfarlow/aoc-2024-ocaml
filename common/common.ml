open! Core
module Math = Math
include Option.Let_syntax
open Angstrom

let ( >> ) f g x = g (f x)
let space = take_while (function ' ' | '\t' -> true | _ -> false)
let integer = take_while1 Char.is_digit >>| Int.of_string
let eol = string "\n" >>| ignore
let exec parser s = parse_string ~consume:All parser s |> Result.ok_or_failwith

let run_with_input_file ~part1 ~part2 ~parse =
  Command.basic ~summary:"Advent of code"
    (let%map_open.Command f =
       choose_one ~if_nothing_chosen:Raise
         [
           flag "-part1" no_arg ~doc:"Run part 1"
           |> map ~f:(fun b -> Option.some_if b part1);
           flag "-part2" no_arg ~doc:"Run part 2"
           |> map ~f:(fun b -> Option.some_if b part2);
         ]
     and input = anon ("input" %: string) in
     fun () -> In_channel.read_all input |> parse |> f)
  |> Command_unix.run
