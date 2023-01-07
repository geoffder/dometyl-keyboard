open! OCADml
open! OSCADml

module Face = struct
  type t =
    { path : Path3.t
    ; points : Points.t
    ; bounds : Points.t
    ; normal : V3.t [@cad.unit]
    }
  [@@deriving cad]

  let direction { points = { top_left; top_right; _ }; _ } =
    V3.normalize V3.(top_left -@ top_right)
end

module Faces = struct
  type t =
    { north : Face.t [@cad.d3]
    ; south : Face.t
    ; east : Face.t
    ; west : Face.t
    }
  [@@deriving cad]

  let map f t = { north = f t.north; south = f t.south; east = f t.east; west = f t.west }

  let fold f init t =
    let f' = Fun.flip f in
    f init t.north |> f' t.south |> f' t.east |> f' t.west

  let face t = function
    | `North -> t.north
    | `South -> t.south
    | `East -> t.east
    | `West -> t.west
end

type config =
  { outer_w : float
  ; outer_h : float
  ; inner_w : float
  ; inner_h : float
  ; thickness : float
  ; clip : Scad.d3 -> Scad.d3
  ; cap_height : float
  ; clearance : float
  ; corner : Path3.Round.corner option
  ; fn : int option
  }

type t =
  { config : config [@cad.ignore]
  ; scad : Scad.d3
  ; origin : V3.t
  ; faces : Faces.t
  ; cap : Scad.d3 option
  ; cutout : Scad.d3 option
  }
[@@deriving cad]

let orthogonal t side = (Faces.face t.faces side).normal

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
  let sq =
    Path3.square ~center:true ~plane:Plane.(neg xz) (v2 outer_w thickness)
    |> Path3.ytrans (outer_h /. -2.)
  in
  let front =
    match corner with
    | Some corner ->
      Path3.(roundover ?fn ~overrun:`Fix @@ Round.flat ~corner ~closed:true sq)
    | None -> sq
  in
  let hole =
    let outer = Scad.of_mesh @@ Mesh.of_rows [ Path3.ytrans outer_h front; front ]
    and inner = Scad.cube ~center:true (v3 inner_w inner_h (thickness +. 0.1)) in
    clip @@ Scad.sub outer inner
  in
  let faces =
    let edges path =
      match Path3.segment ~closed:true path with
      | s0 :: s1 :: segs ->
        let f ((a, a_len, b, b_len) as acc) (s : V3.line) =
          let s_len = V3.distance s.a s.b in
          if a_len > b_len && s_len > b_len
          then a, a_len, s, s_len
          else if s_len > a_len
          then s, s_len, b, b_len
          else acc
        in
        let a, _, b, _ =
          List.fold_left f (s0, V3.distance s0.a s0.b, s1, V3.distance s1.a s1.b) segs
        in
        if a.a.z > 0. then a, b else b, a
      | _ -> failwith "unreachable"
    in
    let south =
      let top_edge, bot_edge = edges front in
      let points =
        Points.
          { top_left = top_edge.b
          ; top_right = top_edge.a
          ; bot_left = bot_edge.a
          ; bot_right = bot_edge.b
          ; centre = v3 0. (outer_h /. -2.) 0.
          }
      and normal = v3 0. (-1.) 0. in
      Face.{ path = front; points; bounds = Points.of_ccw_path sq; normal }
    in
    let north = Face.zrot Float.pi south in
    let east =
      let sq =
        [ north.points.bot_right
        ; south.points.bot_left
        ; south.points.top_left
        ; north.points.top_right
        ]
      in
      let path =
        match corner with
        | Some corner ->
          Path3.(roundover ?fn ~overrun:`Fix @@ Round.flat ~corner ~closed:true sq)
        | None -> sq
      in
      let top_edge, bot_edge = edges path
      and centre = v3 (outer_w /. 2.) 0. 0. in
      let points =
        Points.
          { top_left = top_edge.b
          ; top_right = top_edge.a
          ; bot_left = bot_edge.a
          ; bot_right = bot_edge.b
          ; centre
          }
      and bounds =
        Points.
          { top_left = north.points.top_right
          ; top_right = south.points.top_left
          ; bot_left = north.points.bot_right
          ; bot_right = south.points.bot_left
          ; centre
          }
      and normal = v3 1. 0. 0. in
      Face.{ path; points; bounds; normal }
    in
    let west = Face.zrot Float.pi east in
    Faces.{ north; south; east; west }
  in
  { config
  ; scad = (if render then Scad.render hole else hole)
  ; origin = v3 0. 0. 0.
  ; faces
  ; cap = Option.map (Scad.translate (v3 0. 0. (cap_height +. (thickness /. 2.)))) cap
  ; cutout = (if render then Option.map Scad.render cutout else cutout)
  }

let mirror_internals t =
  { t with
    scad = Scad.mirror (v3 1. 0. 0.) t.scad
  ; cutout = Option.map (Scad.mirror (v3 1. 0. 0.)) t.cutout
  }

let to_scad = function
  | { scad; cutout = Some cut; _ } -> Scad.difference scad [ cut ]
  | { scad; _ } -> scad
