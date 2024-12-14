(** Represents a 2D integer coordinate *)

open! Core

type t = int * int [@@deriving hash, sexp]

include Comparable.S with type t := t
include Hashable.S with type t := t
