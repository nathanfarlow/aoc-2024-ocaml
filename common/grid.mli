(** A 2D array with bounds checking. *)

open! Core

type 'a t = 'a array array

val height : 'a t -> int
val width : 'a t -> int
val in_bounds : 'a t -> Point.t -> bool
val get : 'a t -> Point.t -> 'a
val get_opt : 'a t -> Point.t -> 'a option
