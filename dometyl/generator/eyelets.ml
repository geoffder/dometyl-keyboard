(* open Scad_ml *)

(* type loc = *)
(*   | Thumb of [ `N | `E | `S | `W ] * Util.idx *)
(*   | Body of [ `N | `E | `S | `W ] * Util.idx *)
(*   | Point of v3 *)
(*   | U of float *)

type wall_loc =
  | Thumb of [ `N | `E | `S | `W ] * Util.idx
  | Body of [ `N | `E | `S | `W ] * Util.idx

type t = (Eyelet.t list[@scad.d3]) [@@deriving scad]

(* TODO:
   - revise loc type so that it makes sense
   - should it be one type, and have a placement function that dynamically
    makes use of optional walls / connect inputs (ignore corresponding elements
    if they are missing)
   - U and Point could be handed along to Eyelet.place

   UPDATE:
   - With the adjustments to Eyelet.make and Eyelet.place, I think that the
    additional functionality needed is low enough that I should just include
    this stuff in the Eyelet module.
   - Point and U would be passed along to place anyway, which handles free
    placement of eyelets relative to an inline/outline
   - so all that is really needed is a function that takes a Walls.t and a
    wall_loc list to translate into points for place
*)
