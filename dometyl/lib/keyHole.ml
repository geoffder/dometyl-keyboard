open Base
open Scad_ml

module type Config = sig
  val outer_w : float
  val inner_w : float
  val thickness : float
end

module type S = sig
  include Config

  type t =
    { scad : Model.t
    ; north_face : Model.t
    ; south_face : Model.t
    ; east_face : Model.t
    ; west_face : Model.t
    }

  val map : f:(Model.t -> Model.t) -> t -> t
  val t : t
end

module Make (C : Config) : S = struct
  include C

  type t =
    { scad : Model.t
    ; north_face : Model.t
    ; south_face : Model.t
    ; east_face : Model.t
    ; west_face : Model.t
    }

  let map ~f t =
    { scad = f t.scad
    ; north_face = f t.north_face
    ; south_face = f t.south_face
    ; east_face = f t.east_face
    ; west_face = f t.west_face
    }

  let side_face =
    Model.rotate
      (Math.pi /. 2., 0., 0.)
      (Model.cube ~center:true (outer_w, thickness, 0.1))

  let north_face = Model.translate (0., outer_w /. 2., 0.) side_face
  let south_face = Model.translate (0., outer_w /. -2., 0.) side_face

  let west_face =
    side_face
    |> Model.rotate (0., 0., Math.pi /. 2.)
    |> Model.translate (outer_w /. -2., 0., 0.)

  let east_face =
    side_face
    |> Model.rotate (0., 0., Math.pi /. 2.)
    |> Model.translate (outer_w /. 2., 0., 0.)

  let scad =
    let outer = Model.cube ~center:true (outer_w, outer_w, thickness) in
    let inner = Model.cube ~center:true (inner_w, inner_w, thickness +. 0.1) in
    Model.difference outer [ inner ]

  let t = { scad; north_face; south_face; east_face; west_face }
end
