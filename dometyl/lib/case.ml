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
  let thumb_offset = -5., -50., 9.
  let thumb_angle = Math.(0., pi /. -4., pi /. 5.)
  let clearance = 7.

  (* TODO: tune *)
  let lookup = function
    | 2 -> 0., 6., -6. (* middle *)
    | 3 -> 0., 3., -2. (* ring *)
    | i when i >= 4 -> 0., -12., 6. (* pinky *)
    | _ -> 0., 0., 0.

  let col_offsets =
    let space = Col.Key.outer_w +. spacing in
    let f m i =
      let data = Util.(lookup i <+> (space *. Float.of_int i, 0., 0.)) in
      Map.add_exn ~key:i ~data m
    in
    List.fold ~f ~init:(Map.empty (module Int)) (List.range 0 n_cols)

  let centre_offset = Map.find_exn col_offsets centre_col

  let apply_tent (type a) (module M : Transformable with type t = a) off col : a =
    M.(rotate_about_pt (0., tent, 0.) Util.(off <-> centre_offset) col)

  let place_col off = apply_tent (module Col) off Col.t |> Col.translate off

  let columns =
    let placed = Map.map ~f:place_col col_offsets in
    let lowest_z =
      let face_low ({ points = ps; _ } : Key.Face.t) =
        Key.Face.Points.fold
          ~f:(fun m p -> Float.min m (Util.get_z p))
          ~init:Float.max_value
          ps
      in
      let key_low ({ faces = fs; _ } : Key.t) =
        Key.Faces.fold
          ~f:(fun m face -> Float.min m (face_low face))
          ~init:Float.max_value
          fs
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
        placed
    in
    Map.map ~f:(Col.translate (0., 0., clearance -. lowest_z)) placed

  let thumb =
    let open Thumb in
    rotate thumb_angle t
    |> translate thumb_offset
    |> apply_tent (module Thumb) thumb_offset

  let scad =
    Model.union
      (Map.fold ~f:(fun ~key:_ ~data l -> data.scad :: l) ~init:[ thumb.scad ] columns)

  (* let jank_base =
   *   let full = Model.minkowski [ Model.projection scad; Model.circle 7. ] in
   *   Model.difference full [ Model.offset (`Delta (-10.)) full ] *)
  (* let scad = Model.union [ scad; jank_base ] *)

  (* TODO: should design the wall generation to be able to adjust to tenting
   * better. Right now, the tent above 12deg leads to the higher index column to hang over
   * the middle finger column, so, maybe the walls should be angled slightly, instead of
   * dropping straight down. *)
  (* TODO: need to paramaterize `North wall initial y span based on the radius of the
   * column, and the number of keys per column (number of rows). Southern walls technically
   * are still affected by the radius, but since it's only one key, worth it may not be a
   * problem. I suppose for completeness I may as well implement the slightly different
   * calculations. *)
  let bez_wall side i =
    let c = Map.find_exn columns i in
    let face, y_sign =
      match side with
      | `North -> (snd @@ Map.max_elt_exn c.keys).faces.north, 1.
      | `South -> (Map.find_exn c.keys 0).faces.south, -1.
    in
    let Key.Face.{ points = { centre = (_, _, cz) as centre; _ }; _ } = face in
    (* TODO: to help avoid lower columns with tenting, use the offset lookup to
     * compare the offsets of the current column `i` with the next `i+1`. If the
     * next column is lower in z (and alongside in y, depending on the y-offsets and
     * the North/South side of this wall), then the wall will need to jog over
     * (in the direction towards the top of the tent) in order to avoid collision.
     * To accomplish this, I could have the bezier dodge in x by the time the z
     * distance is covered. Distance that needs to jogged in x can be obtained
     * by comparing the central x coordinate of the current columns eastern face
     * with the x coordinate of the next columns western face (plus spacing?). *)
    let ps =
      let bez =
        Bezier.quad
          ~p1:
            (0., 0., 0.)
            (* ~p2:
             *   ( -.Key.thickness *. Float.sin tent
             *   , y_sign *. 5.
             *   , -.Key.thickness *. Float.cos tent )
             * ~p3:
             *   ( -.Key.thickness *. Float.sin tent
             *   , y_sign *. 8.
             *   , -.cz +. (Key.thickness /. 2.) ) *)
          ~p2:(0., y_sign *. 5., -.Key.thickness)
          ~p3:(0., y_sign *. 8., -.cz +. (Key.thickness /. 2.))
      in
      Bezier.curve bez 0.1
    in
    let rs =
      let bez = Bezier.quad ~p1:(0., 0., 0.) ~p2:(0., -.tent, 0.) ~p3:(0., -.tent, 0.) in
      Bezier.curve bez 0.1
    in
    let chunk =
      Model.cylinder ~center:true (Key.thickness /. 2.) Key.outer_w
      |> Model.rotate (0., (Math.pi /. 2.) +. tent, 0.)
      |> Model.translate centre
    in
    let hulls =
      List.fold2_exn
        ~init:(chunk, [])
        ~f:(fun (last, acc) p r ->
          let next =
            Model.rotate_about_pt r (Math.negate centre) chunk |> Model.translate p
          in
          next, Model.hull [ last; next ] :: acc )
        ps
        rs
      |> fun (_, hs) -> Model.union hs
    in
    hulls

  let scad =
    Model.union
      [ scad
      ; bez_wall `South 2
      ; bez_wall `South 3
      ; bez_wall `South 4
      ; bez_wall `North 0
      ; bez_wall `North 1
      ; bez_wall `North 2
      ; bez_wall `North 3
      ; bez_wall `North 4
      ]

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
