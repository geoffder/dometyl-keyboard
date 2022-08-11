open Base
open Scad_ml

module Face = struct
  type t =
    { scad : Scad.d3
    ; points : Points.t
    }
  [@@deriving scad]

  let make (V3.{ x; y; _ } as size) =
    let points =
      Points.
        { top_left = v3 (x /. -2.) (y /. 2.) 0.
        ; top_right = v3 (x /. 2.) (y /. 2.) 0.
        ; bot_left = v3 (x /. -2.) (y /. -2.) 0.
        ; bot_right = v3 (x /. 2.) (y /. -2.) 0.
        ; centre = v3 0. 0. 0.
        }
    in
    { scad = Scad.cube ~center:true size; points }

  let direction { points = { top_left; top_right; _ }; _ } =
    V3.normalize V3.(top_left -@ top_right)
end

module Faces = struct
  type t =
    { north : Face.t [@scad.d3]
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
        Float.(v3 (pi /. 2.) 0. (if north then 0. else pi))
        (Face.make (v3 w depth 0.1))
    and lat west =
      Face.rotate
        Float.(v3 (pi /. 2.) 0. (pi /. if west then 2. else -2.))
        (Face.make (v3 h depth 0.1))
    in
    { north = Face.translate (v3 0. (h /. 2.) 0.) (vert true)
    ; south = Face.translate (v3 0. (h /. -2.) 0.) (vert false)
    ; west = Face.translate (v3 (w /. -2.) 0. 0.) (lat true)
    ; east = Face.translate (v3 (w /. 2.) 0. 0.) (lat false)
    }

  let face t = function
    | `North -> t.north
    | `South -> t.south
    | `East  -> t.east
    | `West  -> t.west
end

module Kind = struct
  type key = Key
  type 'k t = Key : key t
end

type 'k config =
  { spec : 'k Kind.t
  ; outer_w : float
  ; outer_h : float
  ; inner_w : float
  ; inner_h : float
  ; thickness : float
  ; clip : Scad.d3 -> Scad.d3
  ; cap_height : float
  ; clearance : float
  }

type 'k t =
  { config : 'k config [@scad.ignore]
  ; scad : Scad.d3
  ; origin : V3.t
  ; faces : Faces.t
  ; cap : Scad.d3 option
  ; cutout : Scad.d3 option
  }
[@@deriving scad]

let orthogonal t side =
  V3.(normalize ((Faces.face t.faces side).points.centre -@ t.origin))

let normal t =
  let Points.{ top_left; bot_left; _ } = (Faces.face t.faces `North).points in
  V3.(normalize (top_left -@ bot_left))

let rotate_about_origin r t =
  let about = V3.negate t.origin in
  { t with
    scad = Scad.rotate ~about r t.scad
  ; faces = Faces.rotate ~about r t.faces
  ; cap = Option.map ~f:(Scad.rotate ~about r) t.cap
  ; cutout = Option.map ~f:(Scad.rotate ~about r) t.cutout
  }

let quaternion_about_origin angle t =
  let about = V3.negate t.origin
  and q = Quaternion.make (normal t) angle in
  { t with
    scad = Scad.quaternion ~about q t.scad
  ; faces = Faces.quaternion ~about q t.faces
  ; cap = Option.map ~f:(Scad.quaternion ~about q) t.cap
  ; cutout = Option.map ~f:(Scad.quaternion ~about q) t.cutout
  }

let cycle_faces ({ faces = { north; south; east; west }; _ } as t) =
  { t with faces = { north = west; south = east; east = north; west = south } }

let make
    ?(render = true)
    ?cap
    ?cutout
    ({ outer_w; outer_h; inner_w; inner_h; thickness; clip; cap_height; _ } as config)
  =
  let hole =
    let outer = Scad.cube ~center:true (v3 outer_w outer_h thickness) in
    let inner = Scad.cube ~center:true (v3 inner_w inner_h (thickness +. 0.1)) in
    clip @@ Scad.difference outer [ inner ]
  in
  { config
  ; scad = (if render then Scad.render hole else hole)
  ; origin = v3 0. 0. 0.
  ; faces = Faces.make outer_w outer_h thickness
  ; cap = Option.map ~f:(Scad.translate (v3 0. 0. (cap_height +. (thickness /. 2.)))) cap
  ; cutout = (if render then Option.map ~f:Scad.render cutout else cutout)
  }

let mirror_internals t =
  { t with
    scad = Scad.mirror (v3 1. 0. 0.) t.scad
  ; cutout = Option.map ~f:(Scad.mirror (v3 1. 0. 0.)) t.cutout
  }

let cutout_scad = function
  | { scad; cutout = Some cut; _ } -> Scad.difference scad [ cut ]
  | { scad; _ } -> scad
