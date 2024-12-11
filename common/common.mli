open! Core
module Math = Math
module Grid = Grid
module Point = Point

module Syntax : sig
  include module type of Option.Let_syntax

  val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
end

include module type of Syntax

module Angstrom : sig
  include module type of Angstrom

  val space : unit t
  (** Space or tab *)

  val digit : int t
  (** 0 through 9 *)

  val integer : int t

  val skip_till : 'a t -> 'a t
  (** Skip all characters until we encounter the given parser. *)

  val ws : unit t
  (** Skips [Char.is_whitespace] *)

  val exec : ?consume:Consume.t -> 'a t -> string -> 'a
  val exec_opt : ?consume:Consume.t -> 'a t -> string -> 'a option
end

val triangular : 'a list -> ('a * 'a list) Sequence.t
(** Equivalent to
    {v
       for i in range(len(l)):
        for j in range(i + 1, len(l)):
          yield i, l[j:]
    v}
*)

val all_pairs : 'a list -> ('a * 'a) Sequence.t
(** Equivalent to
    {v
      for i in range(len(l)):
        for j in range(i + 1, len(l)):
          yield l[i], l[j]
    v}
 *)

val zip_next : 'a list -> ('a * 'a) list
(** Equivalent to
    {v
      list(zip(l, l[1:]))
    v}
 *)

val sum : f:('a -> int) -> 'a list -> int
val print_int : int -> unit

val run_with_input_file :
  part1:('a -> unit) -> part2:('a -> unit) -> parse:(string -> 'a) -> unit
