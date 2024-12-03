open! Core
module Math = Math
include module type of Option.Let_syntax

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val space : string Angstrom.t
val integer : int Angstrom.t
val eol : unit Angstrom.t
val parse : 'a Angstrom.t -> string -> 'a

val run_with_input_file :
  part1:('a -> unit) -> part2:('a -> unit) -> parse:(string -> 'a) -> unit
