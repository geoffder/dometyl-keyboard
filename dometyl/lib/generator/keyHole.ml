open Base
open Scad_ml

module Face = struct
  type t =
    { scad : Scad.t
    ; points : Points.t
    }
  [@@deriving scad]

  let make ((x, y, _) as size) =
    let points =
      Points.
        { top_left = x /. -2., y /. 2., 0.
        ; top_right = x /. 2., y /. 2., 0.
        ; bot_left = x /. -2., y /. -2., 0.
        ; bot_right = x /. 2., y /. -2., 0.
        ; centre = 0., 0., 0.
        }
    in
    { scad = Scad.cube ~center:true size; points }

  let direction { points = { top_left; top_right; _ }; _ } =
    Vec3.normalize Vec3.(top_left <-> top_right)
end

module Faces = struct
  type t =
    { north : Face.t
    ; south : Face.t
    ; east : Face.t
    ; west : Face.t
    }
  [@@deriving scad]

  let map ~f t =
    { north = f t.north; south = f t.south; east = f t.east; west = f t.west }

  let fold ~f ~init t =
    let flipped = Fn.flip f in
    f init t.north |> flipped t.south |> flipped t.east |> flipped t.west

  let make w h depth =
    let vert north =
      Face.rotate
        Float.(pi /. 2., 0., if north then 0. else pi)
        (Face.make (w, depth, 0.1))
    and lat west =
      Face.rotate
        Float.(pi /. 2., 0., pi /. if west then 2. else -2.)
        (Face.make (h, depth, 0.1))
    in
    { north = Face.translate (0., h /. 2., 0.) (vert true)
    ; south = Face.translate (0., h /. -2., 0.) (vert false)
    ; west = Face.translate (w /. -2., 0., 0.) (lat true)
    ; east = Face.translate (w /. 2., 0., 0.) (lat false)
    }

  let face t = function
    | `North -> t.north
    | `South -> t.south
    | `East  -> t.east
    | `West  -> t.west
end

module Kind = struct
  type niz =
    { clip_height : float
    ; snap_slot_h : float
    }

  type mx = unit

  type _ t =
    | Mx : mx -> mx t
    | Niz : niz -> niz t
end

type 'k config =
  { spec : 'k Kind.t
  ; outer_w : float
  ; outer_h : float
  ; inner_w : float
  ; inner_h : float
  ; thickness : float
  ; clip : Scad.t -> Scad.t
  ; cap_height : float
  ; clearance : float
  }

type 'k t =
  { config : 'k config [@scad.ignore]
  ; scad : Scad.t
  ; origin : Vec3.t
  ; faces : Faces.t
  ; cap : Scad.t option
  ; cutout : Scad.t option
  }
[@@deriving scad]

let orthogonal t side =
  Vec3.(normalize ((Faces.face t.faces side).points.centre <-> t.origin))

let normal t =
  let Points.{ top_left; bot_left; _ } = (Faces.face t.faces `North).points in
  Vec3.(normalize (top_left <-> bot_left))

let translate p t =
  { t with
    scad = Scad.translate p t.scad
  ; origin = Vec3.add p t.origin
  ; faces = Faces.translate p t.faces
  ; cap = Option.map ~f:(Scad.translate p) t.cap
  ; cutout = Option.map ~f:(Scad.translate p) t.cutout
  }

let mirror ax t =
  { t with
    scad = Scad.mirror ax t.scad
  ; origin = Vec3.mirror ax t.origin
  ; faces = Faces.mirror ax t.faces
  ; cap = Option.map ~f:(Scad.mirror ax) t.cap
  ; cutout = Option.map ~f:(Scad.mirror ax) t.cutout
  }

let rotate r t =
  { t with
    scad = Scad.rotate r t.scad
  ; origin = Vec3.rotate r t.origin
  ; faces = Faces.rotate r t.faces
  ; cap = Option.map ~f:(Scad.rotate r) t.cap
  ; cutout = Option.map ~f:(Scad.rotate r) t.cutout
  }

let rotate_about_pt r p t =
  { t with
    scad = Scad.rotate_about_pt r p t.scad
  ; origin = Vec3.rotate_about_pt r p t.origin
  ; faces = Faces.rotate_about_pt r p t.faces
  ; cap = Option.map ~f:(Scad.rotate_about_pt r p) t.cap
  ; cutout = Option.map ~f:(Scad.rotate_about_pt r p) t.cutout
  }

let quaternion q t =
  { t with
    scad = Scad.quaternion q t.scad
  ; origin = Vec3.quaternion q t.origin
  ; faces = Faces.quaternion q t.faces
  ; cap = Option.map ~f:(Scad.quaternion q) t.cap
  ; cutout = Option.map ~f:(Scad.quaternion q) t.cutout
  }

let quaternion_about_pt q p t =
  { t with
    scad = Scad.quaternion_about_pt q p t.scad
  ; origin = Vec3.quaternion_about_pt q p t.origin
  ; faces = Faces.quaternion_about_pt q p t.faces
  ; cap = Option.map ~f:(Scad.quaternion_about_pt q p) t.cap
  ; cutout = Option.map ~f:(Scad.quaternion_about_pt q p) t.cutout
  }

let rotate_about_origin r t =
  let p = Vec3.negate t.origin in
  { t with
    scad = Scad.rotate_about_pt r p t.scad
  ; faces = Faces.rotate_about_pt r p t.faces
  ; cap = Option.map ~f:(Scad.rotate_about_pt r p) t.cap
  ; cutout = Option.map ~f:(Scad.rotate_about_pt r p) t.cutout
  }

let quaternion_about_origin angle t =
  let p = Vec3.negate t.origin
  and q = Quaternion.make (normal t) angle in
  { t with
    scad = Scad.quaternion_about_pt q p t.scad
  ; faces = Faces.quaternion_about_pt q p t.faces
  ; cap = Option.map ~f:(Scad.quaternion_about_pt q p) t.cap
  ; cutout = Option.map ~f:(Scad.quaternion_about_pt q p) t.cutout
  }

let cycle_faces ({ faces = { north; south; east; west }; _ } as t) =
  { t with faces = { north = west; south = east; east = north; west = south } }

let make
    ?cap
    ?cutout
    ({ outer_w; outer_h; inner_w; inner_h; thickness; clip; cap_height; _ } as config)
  =
  let hole =
    let outer = Scad.cube ~center:true (outer_w, outer_h, thickness) in
    let inner = Scad.cube ~center:true (inner_w, inner_h, thickness +. 0.1) in
    Scad.difference outer [ inner ]
  in
  { config
  ; scad = clip hole
  ; origin = 0., 0., 0.
  ; faces = Faces.make outer_w outer_h thickness
  ; cap = Option.map ~f:(Scad.translate (0., 0., cap_height +. (thickness /. 2.))) cap
  ; cutout
  }

let mirror_internals t =
  { t with
    scad = Scad.mirror (1., 0., 0.) t.scad
  ; cutout = Option.map ~f:(Scad.mirror (1., 0., 0.)) t.cutout
  }

let cutout_scad = function
  | { scad; cutout = Some cut; _ } -> Scad.difference scad [ cut ]
  | { scad; _ } -> scad
