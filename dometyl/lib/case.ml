open! Base
open! Scad_ml
open! Infix
open! Sigs
module Key = KeyHole.Make (Niz.HoleConfig)

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

module Thumb = struct
  include Column.Make (struct
    let n_keys = 3

    module Key = KeyHole.RotateClips (Key)

    module Curve = Curvature.Make (struct
      let style = Curvature.Fan
      let centre_idx = 1
      let angle = Math.pi /. 12.
      let radius = 85.
    end)
  end)

  (* orient along x-axis *)
  let t = rotate (0., 0., Math.pi /. -2.) t
end

module Plate = struct
  type t =
    { scad : Model.t
    ; columns : Col.t Map.M(Int).t
    ; thumb : Thumb.t
    }

  let n_cols = 5
  let spacing = 2.
  let centre_col = 2
  let tent = Math.pi /. 12.
  let thumb_offset = -5., -50., -8.
  let thumb_angle = Math.(0., pi /. -4., pi /. 5.)

  (* TODO: would like to actually calculate this. Will need to take offsets,
   *  total plate depth, tenting, and required guts clearance into account.
   *
   *  Now that the points are carried by keyholes, should be able to do a proper
   * job of this. Calculate the lowest point, then ensure that there is enough space
   * below. Should thumb remain separate since the concerns are a bit different? *)
  let rise = 18.

  (* TODO: tune *)
  let lookup = function
    | 2 -> 0., 6., -6. (* middle *)
    | 3 -> 0., 3., -2. (* ring *)
    | i when i >= 4 -> 0., -12., 6. (* pinky *)
    | _ -> 0., 0., 0.

  let col_offsets =
    let space = Col.Key.outer_w +. spacing in
    let f m i =
      let data = Util.(lookup i <+> (space *. Float.of_int i, 0., rise)) in
      Map.add_exn ~key:i ~data m
    in
    List.fold ~f ~init:(Map.empty (module Int)) (List.range 0 n_cols)

  let centre_offset = Map.find_exn col_offsets centre_col

  let apply_tent (type a) (module M : Transformable with type t = a) off col : a =
    M.(rotate_about_pt (0., tent, 0.) Util.(off <-> centre_offset) col)

  let place_col off = apply_tent (module Col) off Col.t |> Col.translate off
  let columns = Map.map ~f:place_col col_offsets

  let lowest_z =
    let open Util in
    let face_low ({ points = ps; _ } : Key.Face.t) =
      Float.min (get_z ps.top_right) (get_z ps.top_left)
      |> Float.min (get_z ps.bot_right)
      |> Float.min (get_z ps.bot_left)
      |> Float.min (get_z ps.centre)
    in
    let key_low ({ faces = fs; _ } : Key.t) =
      face_low fs.north
      |> Float.min (face_low fs.south)
      |> Float.min (face_low fs.east)
      |> Float.min (face_low fs.west)
    in
    let col_low ({ keys = ks; _ } : Col.t) =
      Map.fold
        ~f:(fun ~key:_ ~data m -> Float.min m (key_low data))
        ~init:Float.max_value
        ks
    in
    Map.fold
      ~f:(fun ~key:_ ~data m -> Float.min m (col_low data))
      ~init:Float.max_value
      columns

  let bottom_marker = Model.cube (100., 1., 1.) |> Model.translate (0., 0., lowest_z)

  let thumb =
    let off = Util.(thumb_offset <+> (0., 0., rise)) in
    Thumb.(rotate thumb_angle t |> translate off |> apply_tent (module Thumb) off)

  let scad =
    Model.union
      (Map.fold
         ~f:(fun ~key:_ ~data l -> data.scad :: l)
         ~init:[ thumb.scad; bottom_marker ]
         (* ~init:[ thumb.scad ] *)
         columns )

  let corners =
    let mark p = Model.cube ~center:true (1., 1., 1.) |> Model.translate p in
    Model.union
      (Map.fold
         ~f:(fun ~key:_ ~data l ->
           mark data.origin
           ::
           mark data.faces.south.points.bot_left
           ::
           mark data.faces.south.points.bot_right
           ::
           mark data.faces.south.points.top_left
           ::
           mark data.faces.south.points.centre
           :: mark data.faces.south.points.top_right :: l )
         ~init:[]
         (* thumb.keys ) *)
         (Map.find_exn columns (centre_col + 1)).keys )

  (* let scad = Model.union [ scad; corners ] *)

  (* let jank_base =
   *   let full = Model.minkowski [ Model.projection scad; Model.circle 7. ] in
   *   Model.difference full [ Model.offset (`Delta (-10.)) full ] *)
  (* let scad = Model.union [ scad; jank_base ] *)

  let jank_wall =
    let c = Map.find_exn columns centre_col in
    let k = Map.find_exn c.keys 0 in
    let Key.Face.{ scad = fs; points = { centre; _ } } = k.faces.south in
    let tilt = Model.rotate_about_pt (Math.pi /. 4., 0., 0.) (Math.negate centre) fs in
    let base =
      Model.cube ~center:true (Key.outer_w, Key.thickness, 0.1)
      |> Model.translate Util.(centre <*> (1., 1., 0.) <+> (0., -5., 0.))
    in
    Model.union [ Model.hull [ fs; tilt ]; Model.hull [ tilt; base ] ]

  let bez_wall i =
    let c = Map.find_exn columns i in
    let k = Map.find_exn c.keys 0 in
    let Key.Face.{ points = { centre = (_, _, cz) as centre; _ }; _ } = k.faces.south in
    let bez =
      Bezier.quad ~p1:(0., 0., 0.) ~p2:(0., -5., -.Key.thickness) ~p3:(0., -8., -1.5 *. cz)
    in
    let ps = Bezier.curve bez 0.1 in
    let chunk =
      Model.cylinder ~center:true (Key.thickness /. 2.) Key.outer_w
      |> Model.rotate (0., (Math.pi /. 2.) +. tent, 0.)
      |> Model.translate centre
    in
    let hulls =
      List.fold
        ~init:(chunk, [])
        ~f:(fun (last, acc) p ->
          let next = Model.translate p chunk in
          next, Model.hull [ last; next ] :: acc )
        ps
      |> fun (_, hs) -> Model.union hs
    in
    Model.difference
      hulls
      [ Model.projection hulls
        |> Model.linear_extrude ~height:cz
        |> Model.translate (0., 0., -.cz)
      ]

  let scad = Model.union [ scad; bez_wall 2; bez_wall 3; bez_wall 4 ]
  let t = { scad; columns; thumb }
end

module A3144Cutout = Sensor.Make (Sensor.A3144PrintConfig)

module NizPlatform = Niz.Platform.Make (struct
  module HoleConfig = Niz.HoleConfig
  module SensorCutout = A3144Cutout

  let w = 20.
  let dome_w = 19.
  let dome_waist = 15. (* width at narrow point, ensure enough space at centre *)

  (* NOTE: BKE ~= 0.95mm; DES ~= 0.73mm. A value of 1.15 seems to fit both without
   * being too tight or loose on either. *)
  let dome_thickness = 1.15
  let base_thickness = 2.25
  let sensor_depth = 1.5
  let snap_clearance = 0.3
  let snap_len = 0.7
  let lug_height = 1.5
end)

let niz_cross_section =
  Model.difference
    (Model.union
       [ Model.translate
           (0., 0., NizPlatform.wall_height +. (Key.thickness /. 2.))
           Key.t.scad
       ; NizPlatform.scad
       ] )
    [ Model.cube ~center:true (25., 15., 20.) |> Model.translate (0., -7.5, 0.) ]
