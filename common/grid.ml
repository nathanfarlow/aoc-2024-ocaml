open! Core

type 'a t = 'a array array

let height = Array.length
let width t = Array.length t.(0)
let get t (i, j) = t.(i).(j)
let in_bounds t (i, j) = i >= 0 && i < height t && j >= 0 && j < width t
let get_opt t (i, j) = if in_bounds t (i, j) then Some t.(i).(j) else None
let set t (i, j) v = t.(i).(j) <- v

let neighbors grid (i, j) =
  [ (i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1) ]
  |> List.filter_map ~f:(fun pos ->
         get_opt grid pos |> Option.map ~f:(fun val_ -> (pos, val_)))

let find_all ~equal grid val_ =
  let locs = ref [] in
  Array.iteri grid ~f:(fun i ->
      Array.iteri ~f:(fun j other ->
          if equal other val_ then locs := (i, j) :: !locs));
  !locs

let find_opt ~equal grid val_ = find_all ~equal grid val_ |> List.hd
let find_exn ~equal grid val_ = find_opt ~equal grid val_ |> Option.value_exn
let copy grid = Array.map grid ~f:Array.copy |> Array.copy
