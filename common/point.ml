open! Core

type t = int * int

include Tuple.Comparable (Int) (Int)
include Tuple.Hashable (Int) (Int)
