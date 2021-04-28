open Base
open Scad_ml
open Sigs

module type Config = sig
  val outer_w : float
  val inner_w : float
  val thickness : float
  val clips : [ `Mx | `Niz ]
end

module type S = sig
  include Config

  module Face : sig
    module Points : sig
      type t =
        { top_left : Core.pos_t
        ; top_right : Core.pos_t
        ; bot_left : Core.pos_t
        ; bot_right : Core.pos_t
        ; centre : Core.pos_t
        }

      val make : float * float * float -> t

      include Transformable with type t := t
    end

    type t =
      { scad : Model.t
      ; points : Points.t
      }

    val make : float * float * float -> t

    include Transformable with type t := t
  end

  module Faces : sig
    type t =
      { north : Face.t
      ; south : Face.t
      ; east : Face.t
      ; west : Face.t
      }

    include Transformable with type t := t
  end

  type t =
    { scad : Model.t
    ; origin : Core.pos_t
    ; faces : Faces.t
    }

  include Transformable with type t := t

  val t : t
end

module Make (C : Config) : S = struct
  include C

  module Face = struct
    module Points = struct
      type t =
        { top_left : Core.pos_t
        ; top_right : Core.pos_t
        ; bot_left : Core.pos_t
        ; bot_right : Core.pos_t
        ; centre : Core.pos_t
        }

      let make (x, y, _) =
        { top_left = x /. -2., y /. 2., 0.
        ; top_right = x /. 2., y /. 2., 0.
        ; bot_left = x /. -2., y /. -2., 0.
        ; bot_right = x /. 2., y /. -2., 0.
        ; centre = 0., 0., 0.
        }

      let map ~f t =
        { top_left = f t.top_left
        ; top_right = f t.top_right
        ; bot_left = f t.bot_left
        ; bot_right = f t.bot_right
        ; centre = f t.centre
        }

      let translate p = map ~f:(Math.add p)
      let rotate r = map ~f:(Math.rotate r)
      let rotate_about_pt r p = map ~f:(Math.rotate_about_pt r p)
    end

    type t =
      { scad : Model.t
      ; points : Points.t
      }

    let make size = { scad = Model.cube ~center:true size; points = Points.make size }

    let translate p t =
      { scad = Model.translate p t.scad; points = Points.translate p t.points }

    let rotate r t = { scad = Model.rotate r t.scad; points = Points.rotate r t.points }

    let rotate_about_pt r p t =
      { scad = Model.rotate_about_pt r p t.scad
      ; points = Points.rotate_about_pt r p t.points
      }
  end

  module Faces = struct
    type t =
      { north : Face.t
      ; south : Face.t
      ; east : Face.t
      ; west : Face.t
      }

    let map ~f t =
      { north = f t.north; south = f t.south; east = f t.east; west = f t.west }

    let translate p = map ~f:(Face.translate p)
    let rotate r = map ~f:(Face.rotate r)
    let rotate_about_pt r p = map ~f:(Face.rotate_about_pt r p)
  end

  type t =
    { scad : Model.t
    ; origin : Core.pos_t
    ; faces : Faces.t
    }

  let translate p t =
    { scad = Model.translate p t.scad
    ; origin = Math.add p t.origin
    ; faces = Faces.translate p t.faces
    }

  let rotate r t =
    { scad = Model.rotate r t.scad
    ; origin = Math.rotate r t.origin
    ; faces = Faces.rotate r t.faces
    }

  let rotate_about_pt r p t =
    { scad = Model.rotate_about_pt r p t.scad
    ; origin = Math.rotate_about_pt r p t.origin
    ; faces = Faces.rotate_about_pt r p t.faces
    }

  let side_face =
    Face.rotate (Math.pi /. 2., 0., 0.) (Face.make (outer_w, thickness, 0.1))

  let north = Face.translate (0., outer_w /. 2., 0.) side_face
  let south = Face.translate (0., outer_w /. -2., 0.) side_face

  let west =
    side_face
    |> Face.rotate (0., 0., Math.pi /. 2.)
    |> Face.translate (outer_w /. -2., 0., 0.)

  let east =
    side_face
    |> Face.rotate (0., 0., Math.pi /. 2.)
    |> Face.translate (outer_w /. 2., 0., 0.)

  let hole =
    let outer = Model.cube ~center:true (outer_w, outer_w, thickness) in
    let inner = Model.cube ~center:true (inner_w, inner_w, thickness +. 0.1) in
    Model.difference outer [ inner ]

  let scad =
    match clips with
    | `Mx  ->
      let clip =
        Model.rotate
          (Math.pi /. 2., 0., 0.)
          (Model.cube ~center:true (5., thickness -. 1.3, 0.5))
      in
      Model.difference
        hole
        [ Model.translate (0., inner_w /. 2., -1.3) clip
        ; Model.translate (0., inner_w /. -2., -1.3) clip
        ]
    | `Niz ->
      let height = thickness /. 2. in
      let nub = Model.circle 1.5 |> Model.linear_extrude ~height in
      Model.union
        [ hole
        ; Model.translate (inner_w /. 2., 0., -.height) nub
        ; Model.translate (inner_w /. -2., 0., -.height) nub
        ]

  let t = { scad; origin = 0., 0., 0.; faces = { north; south; east; west } }
end

module RotateClips (K : S) : S = struct
  include K

  let t =
    let t' = rotate (0., 0., Math.pi /. 2.) t in
    let { faces = { north; south; east; west }; _ } = t' in
    { t' with faces = { north = east; south = west; east = south; west = north } }
end
