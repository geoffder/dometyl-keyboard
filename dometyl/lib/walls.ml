open! Base
open! Scad_ml

(* TODO: Come up with how I would like to organize the generated walls, such that
 * joining them up will be sensical.
 * - What do I want to supply the top-level functions in this module? Map of columns,
 * with wall generating closures (drop and siding) and a list of where to make each?
 * - Keep in mind that I am going to need to join up the thumb to the west side
 * and southern wall of the middle finger column. Otherwise, the thumbs functions will
 * be separate since the pattern is different there. Makes sense to have the separate
 * modules with their own make functions then. *)
module Body = struct
  type col =
    { north : Wall.t option
    ; south : Wall.t option
    }

  type sides =
    { west : Wall.t Map.M(Int).t
    ; east : Wall.t Map.M(Int).t
    }

  type t =
    { cols : col Map.M(Int).t
    ; sides : sides
    }
end

module Thumb = struct
  type key =
    { north : Wall.t option
    ; south : Wall.t option
    }

  type sides =
    { west : Wall.t option
    ; east : Wall.t option
    }

  type t =
    { keys : key Map.M(Int).t
    ; sides : sides
    }
end
