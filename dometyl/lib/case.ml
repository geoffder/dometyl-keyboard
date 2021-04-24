open! Base
open! Scad_ml

module Key = KeyHole.Make (struct
  let outer_w = 19.
  let inner_w = 14.
  let thickness = 4.
end)

module Col = Column.Make (struct
  let n_keys = 3

  module Key = Key

  module Curve = Curvature.Make (struct
    let centre_idx = 1
    let angle = Curvature.X, Math.pi /. 12.
    let radius = Curvature.Z, -85.
  end)
end)

module Thumb = Column.Make (struct
  let n_keys = 3

  module Key = Key

  module Curve = Curvature.Make (struct
    let centre_idx = 1
    let angle = Curvature.Z, Math.pi /. 12.
    let radius = Curvature.X, 85.
  end)
end)
