open Scad_ml
open Syntax

module Steps = struct
  type t =
    [ `PerZ of float
    | `Flat of int
    ]

  let to_int t z =
    match t with
    | `PerZ mm -> Int.max 2 (Float.to_int (z /. mm))
    | `Flat n -> n
end

module Drawer = struct
  type loc =
    [ `BL
    | `BR
    | `TL
    | `TR
    | `CN
    | `B of float
    | `T of float
    | `L of float
    | `R of float
    | `XY of float * float
    ]

  type t = loc -> Path3.t

  let map f (t : t) : t = t >> f
  let translate p t loc = Path3.translate p (t loc)
  let xtrans x t loc = Path3.xtrans x (t loc)
  let ytrans y t loc = Path3.ytrans y (t loc)
  let ztrans z t loc = Path3.ztrans z (t loc)
  let scale s t loc = Path3.scale s (t loc)
  let mirror ax t loc = Path3.mirror ax (t loc)
  let rotate ?about r t loc = Path3.rotate ?about r (t loc)
  let xrot ?about r t loc = Path3.xrot ?about r (t loc)
  let yrot ?about r t loc = Path3.yrot ?about r (t loc)
  let zrot ?about r t loc = Path3.zrot ?about r (t loc)
  let axis_rotate ?about ax r t loc = Path3.axis_rotate ?about ax r (t loc)
  let quaternion ?about q t loc = Path3.quaternion ?about q (t loc)
  let affine m t loc = Path3.affine m (t loc)
end

type config =
  { d1 : [ `Abs of float | `Rel of float ]
  ; d2 : float
  ; clearance : float
  ; n_steps : Steps.t
  ; scale : V2.t option
  ; scale_ez : (V2.t * V2.t) option
  ; eyelet_config : Eyelet.config option
  }

let default =
  { d1 = `Abs 14.
  ; d2 = 8.
  ; clearance = 1.5
  ; n_steps = `Flat 4
  ; scale = None
  ; scale_ez = None
  ; eyelet_config = None
  }

type t =
  { scad : Scad.d3
  ; start : Points.t
  ; cleared : Points.t
  ; foot : Points.t
  ; drawer : Drawer.t
  ; bounds_drawer : Drawer.t
  ; screw : Eyelet.t option
  }
[@@deriving scad]

(* Compute a rotation around the face's bottom or top edge, depending on which way
   it's orthoganal is pointing in z, that makes the short edge (between the
   bottom and top long edge), as vertical as possible. The pivoted face, and its
   new orthogonal are returned. *)
let swing_face key_origin face =
  let dir = Key.Face.direction face in
  let ortho = V3.(normalize (face.points.centre -@ key_origin)) in
  let about, z_sign =
    if V3.get_z ortho > 0.
    then V3.mid face.points.bot_left face.points.bot_right, 1.
    else V3.mid face.points.top_left face.points.top_right, -1.
  in
  let q =
    let proj = Plane.(project @@ of_normal dir) in
    let up = V3.(normalize (face.points.top_left -@ face.points.bot_left)) in
    Quaternion.make dir @@ (V2.angle (proj up) (proj @@ v3 0. 0. 1.) *. z_sign)
  in
  Key.Face.quaternion ~about q face, V3.quaternion q ortho

(* TODO: Think of scaling d1 based on how high the key is, though maybe should
   do so in the higher level functions in walls that call this one. Having a larger
   d1 value will improve the clearance for the tall columns, which aren't in such a
   hurry to move in xy (since they have a larger distance to do it).

   NOTE: `Flat and `ZRatio as the type for d1? `ZRatio being a % of Z that should
   be assigned as d1. Would that make the bow of the curve more consistent? *)

let poly_siding
    ?(x_off = 0.)
    ?(y_off = 0.)
    ?(clearance = 1.5)
    ?(n_steps = `Flat 4)
    ?(d1 = `Abs 14.)
    ?(d2 = 10.)
    ?scale
    ?scale_ez
    ?eyelet_config
    side
    (key : Key.t)
  =
  (* TODO: may do away with the x_off logic, since it really messes with the
    rotation of the sweep transforms. Instead, may users should just scale the walls? *)
  let x_off = x_off *. 0. in
  let start_face = Key.Faces.face key.faces side in
  let pivoted_face, ortho = swing_face key.origin start_face in
  let cleared_face = Key.Face.translate (V3.map (( *. ) clearance) ortho) pivoted_face in
  let xy = V3.(normalize (mul ortho (v3 1. 1. 0.)))
  and dir = Points.direction cleared_face.points
  and fn = Steps.to_int n_steps cleared_face.points.centre.z in
  let d1 =
    match d1 with
    | `Abs d -> d
    | `Rel frac -> cleared_face.points.centre.z *. frac
  and step = 1. /. Float.of_int fn in
  let bz end_z =
    let ({ x; y; z } as cx) = cleared_face.points.centre in
    let p1 = V3.(cx -@ (ortho *$ 0.01)) (* fudge for union *)
    and p2 = V3.((xy *@ v3 d1 d1 0.) +@ v3 (x +. x_off) (y +. y_off) z)
    and p3 = V3.((xy *@ v3 d2 d2 0.) +@ v3 (x +. x_off) (y +. y_off) end_z) in
    Bezier3.make [ p1; p2; p3 ]
  and counter =
    (* counter the rotation created by the z tilt of the face, such that the
       angle of the wall is more in line with the xy angle of the originating face *)
    let a = V3.angle dir (v3 dir.x dir.y 0.) *. Math.sign dir.z *. -1. in
    let s = Quaternion.(slerp (make ortho 0.) (make ortho a))
    and ez = Easing.make (v2 0.42 0.) (v2 1. 1.) in
    let f i = Affine3.of_quaternion @@ s (ez (Float.of_int i *. step)) in
    List.init (fn + 1) f
  and centred =
    Path3.translate (V3.neg @@ cleared_face.points.centre) cleared_face.path
  in
  let scaler =
    match scale with
    | Some s ->
      let p = Path3.to_plane centred in
      let a = V2.angle (V3.project p dir) (v2 1. 0.) in
      let ez =
        match scale_ez with
        | Some (a, b) -> Easing.make a b
        | None -> Fun.id
      in
      let factor i = V2.lerp (v2 1. 1.) s (ez (Float.of_int i *. step)) in
      fun i pt ->
        V3.project p pt
        |> V2.rotate a
        |> V2.scale (factor i)
        |> V2.rotate (-.a)
        |> V2.lift p
    | None -> fun _ pt -> pt
  in
  let transforms =
    (* TODO: decide whether the end_z fudge should be parameterized *)
    let trans =
      Path3.to_transforms ~mode:`NoAlign (Bezier3.curve ~fn (bz 0.))
      |> Util.last
      |> Affine3.compose (Util.last counter)
    in
    let last_shape = Path3.affine trans (List.map (scaler fn) centred) in
    let end_z = Float.max ((Path3.bbox last_shape).min.z *. -1.) 0. +. 0.05 in
    Path3.to_transforms ~mode:`NoAlign (Bezier3.curve ~fn (bz end_z))
    |> List.map2 (fun c m -> Affine3.(c %> m)) counter
    |> Util.prune_transforms ~shape:(fun i -> List.map (scaler i) centred)
  in
  if List.length transforms < 2
  then
    failwith
      "Insufficient valid wall sweep transformations, consider tweaking d parameters.";
  let scad =
    let rows =
      List.map
        (fun (i, m) -> List.map (fun p -> V3.affine m (scaler i p)) centred)
        transforms
    and clearing =
      Mesh.slice_profiles ~slices:(`Flat 5) [ start_face.path; cleared_face.path ]
    in
    let final =
      let s = Util.last rows in
      let flat = List.map (fun { x; y; z = _ } -> v3 x y 0.) s in
      Mesh.slice_profiles ~slices:(`Flat 5) [ s; flat ]
    in
    Mesh.of_rows ~style:`MinEdge (List.concat [ clearing; List.tl rows; List.tl final ])
    |> Mesh.to_scad
  and foot =
    let i, m = Util.last transforms in
    let f p =
      let { x; y; z = _ } = V3.(affine m (scaler i (p -@ cleared_face.points.centre))) in
      v3 x y 0.
    in
    Points.map f cleared_face.points
  and drawer ~bounds =
    let start, cleared =
      if bounds
      then start_face.bounds, cleared_face.bounds
      else start_face.points, cleared_face.points
    in
    fun pt ->
      let p0, p1 =
        let f x y =
          let g (p : Points.t) =
            let bot = V3.lerp p.bot_left p.bot_right x
            and top = V3.lerp p.top_left p.top_right x in
            V3.lerp bot top y
          in
          g start, g cleared
        in
        match pt with
        | `TL -> start_face.points.top_left, cleared_face.points.top_left
        | `TR -> start_face.points.top_right, cleared_face.points.top_right
        | `BL -> start_face.points.bot_left, cleared_face.points.bot_left
        | `BR -> start_face.points.bot_right, cleared_face.points.bot_right
        | `CN -> start_face.points.centre, cleared_face.points.centre
        | `T x -> f x 1.
        | `B x -> f x 0.
        | `L y -> f 0. y
        | `R y -> f 1. y
        | `XY (x, y) -> f x y
      in
      let c1 = V3.sub p1 cleared_face.points.centre in
      let trans = List.rev transforms
      and f (i, m) = V3.affine m (scaler i c1) in
      let last = f (List.hd trans) in
      let flat = V3.(last *@ v 1. 1. 0.) in
      p0
      :: List.fold_left
           (fun acc im -> f im :: acc)
           (if V3.approx ~eps:0.1 last flat then [ flat ] else [ last; flat ])
           (List.tl trans)
  in
  let screw =
    match eyelet_config with
    | Some config ->
      let open Eyelet in
      let placement =
        let n = V3.neg xy in
        match config with
        | { hole = Through; _ } -> Normal (V2.of_v3 n)
        | { hole = Inset _; outer_rad; _ } ->
          let offset = outer_rad +. V3.(norm (sub foot.top_left foot.bot_left) /. 4.) in
          Point V3.(to_v2 (mid foot.top_left foot.top_right +@ (n *$ offset)))
      in
      let l = V2.(lerp (of_v3 foot.bot_left) (of_v3 foot.top_left) 0.25)
      and r = V2.(lerp (of_v3 foot.bot_right) (of_v3 foot.top_right) 0.25) in
      Some (make ~placement config [ l; r ])
    | None -> None
  in
  { scad = Util.value_map_opt ~default:scad (fun s -> Eyelet.apply s scad) screw
  ; start = start_face.points
  ; cleared = cleared_face.points
  ; foot
  ; drawer = drawer ~bounds:false
  ; bounds_drawer = drawer ~bounds:true
  ; screw
  }

let poly_of_config
    ?x_off
    ?y_off
    { d1; d2; clearance; n_steps; scale; scale_ez; eyelet_config }
  =
  poly_siding ~d1 ~d2 ?x_off ?y_off ~clearance ~n_steps ?eyelet_config ?scale ?scale_ez

let column_drop
    ?clearance
    ?n_steps
    ?d1
    ?d2
    ?scale
    ?scale_ez
    ?eyelet_config
    ~spacing
    ~columns
    side
    idx
  =
  let key, face, hanging =
    let c : Column.t = IMap.find idx columns in
    match side with
    | `North ->
      let key = snd @@ IMap.max_binding c.keys in
      let edge_y = V3.get_y key.faces.north.points.centre in
      key, key.faces.north, ( <= ) edge_y
    | `South ->
      let key = IMap.find 0 c.keys in
      let edge_y = V3.get_y key.faces.south.points.centre in
      key, key.faces.south, ( >= ) edge_y
  in
  let x_dodge =
    match IMap.find_opt (idx + 1) columns with
    | Some next_c ->
      let right_x = V3.get_x face.points.top_right
      and next_face = Key.Faces.face (snd @@ IMap.max_binding next_c.keys).faces side in
      let diff =
        if hanging (V3.get_y next_face.points.centre)
        then right_x -. V3.get_x next_face.points.bot_left
        else -.spacing
      in
      if diff > 0. then diff +. spacing else Float.max 0. (spacing +. diff)
    | _ -> 0.
  in
  poly_siding
    ~x_off:(x_dodge *. -1.)
    ?clearance
    ?d1
    ?d2
    ?n_steps
    ?eyelet_config
    ?scale
    ?scale_ez
    side
    key

let drop_of_config ~spacing { d1; d2; clearance; n_steps; scale; scale_ez; eyelet_config }
  =
  column_drop ~d1 ~d2 ~clearance ~n_steps ~spacing ?scale ?scale_ez ?eyelet_config

let start_direction { start = { top_left; top_right; _ }; _ } =
  V3.normalize V3.(top_left -@ top_right)

let foot_direction { foot = { top_left; top_right; _ }; _ } =
  V3.normalize V3.(top_left -@ top_right)

let to_scad t = t.scad
