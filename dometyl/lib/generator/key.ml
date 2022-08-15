open Base
open Scad_ml

module Face = struct
  type t =
    { path : Path3.t
    ; points : Points.t
    }
  [@@deriving scad]

  (* let make (V3.{ x; y; _ } as size) = *)
  (*   let points = *)
  (*     Points. *)
  (*       { top_left = v3 (x /. -2.) (y /. 2.) 0. *)
  (*       ; top_right = v3 (x /. 2.) (y /. 2.) 0. *)
  (*       ; bot_left = v3 (x /. -2.) (y /. -2.) 0. *)
  (*       ; bot_right = v3 (x /. 2.) (y /. -2.) 0. *)
  (*       ; centre = v3 0. 0. 0. *)
  (*       } *)
  (*   in *)
  (*   { scad = Scad.cube ~center:true size; points } *)

  let direction { points = { top_left; top_right; _ }; _ } =
    V3.normalize V3.(top_left -@ top_right)
end

module Faces = struct
  (* type face = *)
  (*   { path : Path3.t *)
  (*   ; points : Points.t *)
  (*   } *)
  (* [@@deriving scad] *)

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
    let f' = Fn.flip f in
    f init t.north |> f' t.south |> f' t.east |> f' t.west

  (* let make w h depth = *)
  (*   let vert north = *)
  (*     Face.rotate *)
  (*       Float.(v3 (pi /. 2.) 0. (if north then 0. else pi)) *)
  (*       (Face.make (v3 w depth 0.1)) *)
  (*   and lat west = *)
  (*     Face.rotate *)
  (*       Float.(v3 (pi /. 2.) 0. (pi /. if west then 2. else -2.)) *)
  (*       (Face.make (v3 h depth 0.1)) *)
  (*   in *)
  (*   { north = Face.translate (v3 0. (h /. 2.) 0.) (vert true) *)
  (*   ; south = Face.translate (v3 0. (h /. -2.) 0.) (vert false) *)
  (*   ; west = Face.translate (v3 (w /. -2.) 0. 0.) (lat true) *)
  (*   ; east = Face.translate (v3 (w /. 2.) 0. 0.) (lat false) *)
  (*   } *)

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
  ; corner : Path2.Round.corner
  ; fn : int option
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

let rotate_about_origin r t = rotate ~about:t.origin r t

let quaternion_about_origin angle t =
  let q = Quaternion.make (normal t) angle in
  quaternion ~about:t.origin q t

let cycle_faces ({ faces = { north; south; east; west }; _ } as t) =
  { t with faces = { north = west; south = east; east = north; west = south } }

let make
    ?(render = true)
    ?cap
    ?cutout
    ( { outer_w; outer_h; inner_w; inner_h; thickness; clip; cap_height; corner; fn; _ }
    as config )
  =
  let sq = Path2.square ~center:true (v2 outer_w thickness) in
  (* TODO: corner in config should be option, no roundover if None *)
  let pth = Path2.(roundover ?fn @@ Round.flat ~corner ~closed:true sq) in
  let hole =
    let outer =
      Mesh.linear_extrude ~center:true ~height:outer_h (Poly2.make pth)
      |> Mesh.xrot (Float.pi /. -2.)
      |> Mesh.to_scad
    and inner = Scad.cube ~center:true (v3 inner_w inner_h (thickness +. 0.1)) in
    clip @@ Scad.sub outer inner
  in
  let faces =
    let top_edge, bot_edge =
      match Path2.segment ~closed:true pth with
      | s0 :: s1 :: segs ->
        let f ((a, a_len, b, b_len) as acc) (s : V2.line) =
          let s_len = V2.distance s.a s.b in
          if Float.(s_len > a_len)
          then s, s_len, b, b_len
          else if Float.(s_len > b_len)
          then a, a_len, s, s_len
          else acc
        in
        let a, _, b, _ =
          List.fold_left
            ~f
            ~init:(s0, V2.distance s0.a s0.b, s1, V2.distance s1.a s1.b)
            segs
        in
        if Float.(a.a.y > 0.) then a, b else b, a
      | _                -> failwith "unreachable"
    in
    let south =
      let path = Path3.(ytrans (outer_h /. -2.) @@ of_path2 ~plane:Plane.xz pth) in
      let points =
        Points.
          { top_left = V3.of_v2 top_edge.a
          ; top_right = V3.of_v2 top_edge.b
          ; bot_left = V3.of_v2 bot_edge.a
          ; bot_right = V3.of_v2 bot_edge.b
          ; centre = v3 0. 0. 0.
          }
        |> Points.xrot (Float.pi /. -2.)
        |> Points.ytrans (outer_h /. -2.)
      in
      Face.{ path; points }
    in
    let north = Face.zrot Float.pi south in
    let west =
      let path =
        [ south.points.bot_left
        ; north.points.bot_right
        ; north.points.top_right
        ; south.points.top_left
        ]
      and points =
        Points.
          { bot_left = south.points.bot_left
          ; bot_right = north.points.bot_right
          ; top_left = south.points.top_left
          ; top_right = north.points.top_right
          ; centre = v3 (outer_w /. -2.) 0. 0.
          }
      in
      Face.{ path; points }
    in
    let east = Face.zrot Float.pi west in
    Faces.{ north; south; east; west }
  in
  { config
  ; scad = (if render then Scad.render hole else hole)
  ; origin = v3 0. 0. 0.
  ; faces
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
