open Core_kernel
open Scad_ml

module KeyHole = struct
  type t =
    { scad : Scad_ml.Model.t
    ; north_face : Scad_ml.Model.t
    ; south_face : Scad_ml.Model.t
    }

  let outer_w = 19.
  let inner_w = 14.
  let thickness = 4.

  let north_face =
    Model.square ~center:true (outer_w, thickness)
    |> Model.rotate (0., Math.pi /. 2., 0.)
    |> Model.translate (0., outer_w /. 2., 0.)

  let south_face =
    Model.square ~center:true (outer_w, thickness)
    |> Model.rotate (0., Math.pi /. 2., 0.)
    |> Model.translate (0., outer_w /. 2., 0.)

  let scad =
    let outer = Model.cube ~center:true (outer_w, outer_w, thickness) in
    let inner = Model.cube ~center:true (inner_w, inner_w, thickness +. 0.1) in
    Model.difference outer [ inner ]

  let apply t f =
    { scad = f t.scad; north_face = f t.north_face; south_face = f t.south_face }

  let t = { scad; north_face; south_face }
end

module Column = struct
  let n_keys = 3
  let angle = Math.pi /. 12.
  let radius = 85.

  let place_keys keys i =
    let next =
      Util.rotate_about_pt (angle *. Int.to_float i, 0., 0.) (0., 0., -.radius)
      |> KeyHole.apply KeyHole.t
    in
    next :: keys

  (* let join_keys *)

  let keys = List.fold (List.range 0 n_keys) ~init:[] ~f:place_keys
  let scad = Model.union (List.map ~f:(fun k -> k.scad) keys)
end
