open Base
open Scad_ml

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

    let fold ~f ~init t =
      let flipped = Fn.flip f in
      f init t.top_left
      |> flipped t.top_right
      |> flipped t.bot_left
      |> flipped t.bot_right

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

  (* let angles { points = { top_left; top_right; bot_left; _ }; _ } =
   *   let x, z =
   *     let dx, dy, dz = Util.(top_right <-> top_left) in
   *     Float.atan (dz /. dy), Float.atan (dx /. dy)
   *   and y =
   *     let dx, _, dz = Util.(top_left <-> bot_left) in
   *     Float.atan (dz /. dx)
   *   in
   *   Stdio.print_endline "face angles:";
   *   Stdio.printf "x angle: %.2f \n" x;
   *   Stdio.printf "y angle: %.2f \n" y;
   *   Stdio.printf "z angle: %.2f \n\n" z;
   *   x, y, z
   *
   * let angles' { points = { top_left; top_right; _ }; _ } =
   *   let dx, dy, dz = Math.norm Util.(top_left <-> top_right) in
   *   let x = Float.acos dx
   *   and y = Float.acos dy
   *   and z = Float.acos dz in
   *   Stdio.print_endline "face angles:";
   *   Stdio.printf "x angle: %.2f \n" x;
   *   Stdio.printf "y angle: %.2f \n" y;
   *   Stdio.printf "z angle: %.2f \n\n" z;
   *   x, y, z *)

  let direction { points = { top_left; top_right; _ }; _ } =
    Math.norm Util.(top_left <-> top_right)
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

  let fold ~f ~init t =
    let flipped = Fn.flip f in
    f init t.north |> flipped t.south |> flipped t.east |> flipped t.west

  let make width depth =
    let half_w = width /. 2. in
    let rot_lat = Face.rotate (0., 0., Math.pi /. 2.) in
    let base = Face.rotate (Math.pi /. 2., 0., 0.) (Face.make (width, depth, 0.1)) in
    { north = Face.translate (0., half_w, 0.) base
    ; south = Face.translate (0., -.half_w, 0.) base
    ; west = base |> rot_lat |> Face.translate (-.half_w, 0., 0.)
    ; east = base |> rot_lat |> Face.translate (half_w, 0., 0.)
    }

  let face t = function
    | `North -> t.north
    | `South -> t.south
    | `East  -> t.east
    | `West  -> t.west

  let translate p = map ~f:(Face.translate p)
  let rotate r = map ~f:(Face.rotate r)
  let rotate_about_pt r p = map ~f:(Face.rotate_about_pt r p)
end

module Kind = struct
  type niz =
    { clip_height : float
    ; snap_slot_h : float
    }

  type _ t =
    | Mx : unit -> unit t
    | Niz : niz -> niz t
end

type 'k config =
  { spec : 'k Kind.t
  ; outer_w : float
  ; inner_w : float
  ; thickness : float
  ; clip : Model.t -> Model.t
  }

type 'k t =
  { config : 'k config
  ; scad : Model.t
  ; origin : Core.pos_t
  ; faces : Faces.t
  }

let translate p t =
  { t with
    scad = Model.translate p t.scad
  ; origin = Math.add p t.origin
  ; faces = Faces.translate p t.faces
  }

let rotate r t =
  { t with
    scad = Model.rotate r t.scad
  ; origin = Math.rotate r t.origin
  ; faces = Faces.rotate r t.faces
  }

let rotate_about_pt r p t =
  { t with
    scad = Model.rotate_about_pt r p t.scad
  ; origin = Math.rotate_about_pt r p t.origin
  ; faces = Faces.rotate_about_pt r p t.faces
  }

let rotate_clips t =
  let t' = rotate (0., 0., Math.pi /. 2.) t in
  let { faces = { north; south; east; west }; _ } = t' in
  { t' with faces = { north = east; south = west; east = south; west = north } }

(* NOTE: These key angle finding functions assume that the key in question is a part
 * of a column oriented along the y-axis *)
let angles { faces = { north; west; _ }; _ } =
  let x, z' =
    let dx, dy, dz = Util.(west.points.top_right <-> west.points.top_left) in
    Float.atan (dz /. dy), Float.atan (dx /. dy)
  and y, z =
    let dx, dy, dz = Util.(north.points.top_right <-> north.points.top_left) in
    Float.atan (dz /. dx), Float.atan (dy /. dx)
  in
  Stdio.print_endline "keyhole angles:";
  Stdio.printf "x angle: %.2f \n" x;
  Stdio.printf "y angle: %.2f \n" y;
  Stdio.printf "z angle: %.2f \n" z;
  Stdio.printf "z? angle: %.2f \n\n" z';
  x, y, z

let orthogonal t side =
  Math.norm Util.((Faces.face t.faces side).points.centre <-> t.origin)

let make ({ outer_w; inner_w; thickness; clip; _ } as config) =
  let hole =
    let outer = Model.cube ~center:true (outer_w, outer_w, thickness) in
    let inner = Model.cube ~center:true (inner_w, inner_w, thickness +. 0.1) in
    Model.difference outer [ inner ]
  in
  { config; scad = clip hole; origin = 0., 0., 0.; faces = Faces.make outer_w thickness }
