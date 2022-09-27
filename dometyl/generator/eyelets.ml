open Scad_ml

type loc =
  | Thumb of Util.idx * Util.idx
  | Body of Util.idx * Util.idx
  | Point of v2
  | U of float

type t = (Eyelet.t list[@scad.d3]) [@@deriving scad]

(* TODO:
   - revise loc type so that it makes sense
   - should it be one type, and have a placement function that dynamically
    makes use of optional walls / connect inputs (ignore corresponding elements
    if they are missing)
   - U and Point could be handed along to Eyelet.place
   - Thumb and Body must instead refer to wall locations (need NESW identifier,
    not just indices for th key) *)
