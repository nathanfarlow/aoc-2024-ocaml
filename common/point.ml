open! Core

type t = int * int

module O = struct
  let lift f (a, b) (c, d) = (f a c, f b d)
  let ( + ) = lift ( + )
  let ( - ) = lift ( - )
  let ( * ) = lift ( * )
end

include Tuple.Comparable (Int) (Int)
include Tuple.Hashable (Int) (Int)
