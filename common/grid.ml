open! Core

type 'a t = 'a array array

let height = Array.length
let width t = Array.length t.(0)
let get t (i, j) = t.(i).(j)
let in_bounds t (i, j) = i >= 0 && i < height t && j >= 0 && j < width t
let get_opt t (i, j) = if in_bounds t (i, j) then Some t.(i).(j) else None
