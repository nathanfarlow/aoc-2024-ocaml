open! Core
module Math = Math
include module type of Option.Let_syntax

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val space : unit Angstrom.t
val integer : int Angstrom.t
val eol : unit Angstrom.t
val skip_till : 'a Angstrom.t -> 'a Angstrom.t
val exec : ?consume:Angstrom.Consume.t -> 'a Angstrom.t -> string -> 'a
val ws : unit Angstrom.t

val exec_opt :
  ?consume:Angstrom.Consume.t -> 'a Angstrom.t -> string -> 'a option

module Array : sig
  include module type of Array

  val get_opt : 'a array -> int -> 'a option
end

val zip_next : 'a list -> ('a * 'a) list
val sum : f:('a -> int) -> 'a list -> int
val print_int : int -> unit

val run_with_input_file :
  part1:('a -> unit) -> part2:('a -> unit) -> parse:(string -> 'a) -> unit
