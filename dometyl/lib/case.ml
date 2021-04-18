open Scad_ml

module KeyHole = struct
  let outer_w = 19.
  let inner_w = 14.
  let thickness = 4.

  let scad =
    let outer = Model.cube ~center:true (outer_w, outer_w, thickness) in
    let inner = Model.cube ~center:true (inner_w, inner_w, thickness +. 0.1) in
    Model.difference outer [ inner ]
end

module Column = struct
  let scad = KeyHole.scad
end
