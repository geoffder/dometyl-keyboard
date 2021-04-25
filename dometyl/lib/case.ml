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
    let style = Curvature.Well
    let centre_idx = 1
    let angle = Math.pi /. 12.
    let radius = 85.
  end)
end)

module Thumb = Column.Make (struct
  let n_keys = 3

  module Key = Key

  module Curve = Curvature.Make (struct
    let style = Curvature.Fan
    let centre_idx = 1
    let angle = Math.pi /. 12.
    let radius = 85.
  end)
end)
