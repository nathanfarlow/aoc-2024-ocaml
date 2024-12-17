(** A 2D array with bounds checking. *)

open! Core

type 'a t = 'a array array [@@deriving sexp_of]

val height : 'a t -> int
val width : 'a t -> int
val in_bounds : 'a t -> Point.t -> bool
val get : 'a t -> Point.t -> 'a
val get_opt : 'a t -> Point.t -> 'a option
val set : 'a t -> Point.t -> 'a -> unit
val neighbors : 'a t -> Point.t -> (Point.t * 'a) list
val find_all : equal:('a -> 'b -> bool) -> 'a t -> 'b -> Point.t list
val find_opt : equal:('a -> 'b -> bool) -> 'a t -> 'b -> Point.t option
val find_exn : equal:('a -> 'b -> bool) -> 'a t -> 'b -> Point.t
val copy : 'a t -> 'a t
