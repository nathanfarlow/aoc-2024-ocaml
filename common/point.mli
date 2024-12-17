(** Represents a 2D integer coordinate *)

open! Core

type t = int * int [@@deriving hash, sexp]

module O : sig
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
end

include Comparable.S with type t := t
include Hashable.S with type t := t

val up : t
val down : t
val left : t
val right : t
