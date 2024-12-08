(** Represents a 2D integer coordinate *)

open! Core

type t = int * int [@@deriving compare, hash, sexp]
