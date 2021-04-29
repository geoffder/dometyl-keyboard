open! Base
open! Scad_ml
open! Infix

module Key = KeyHole.Make (struct
  let outer_w = 19.
  let inner_w = 14.
  let thickness = 4.
  let clips = `Niz
end)

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

module Thumb = Column.Make (struct
  let n_keys = 3

  module Key = KeyHole.RotateClips (Key)

  module Curve = Curvature.Make (struct
    let style = Curvature.Fan
    let centre_idx = 1
    let angle = Math.pi /. 12.
    let radius = 85.
  end)
end)

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
  let thumb_offset = -10., -45., 5.
  let thumb_angle = Math.(pi /. 6., pi /. 5., pi /. -3.)

  (* TODO: would like to actually calculate this. Will need to take offsets,
   *  total plate depth, tenting, and required guts clearance into account. *)
  let rise = 18.

  let lookup = function
    | 2 -> 0., 6., -6. (* middle *)
    | 3 -> 0., 3., -2. (* ring *)
    | i when i >= 4 -> 0., -12., 6. (* pinky *)
    | _ -> 0., 0., 0.

  (* TODO: tune, these are placeholders *)
  let col_offsets =
    let space = Col.Key.outer_w +. spacing in
    let f m i =
      let data = Util.(lookup i <+> (space *. Float.of_int i, 0., rise)) in
      Map.add_exn ~key:i ~data m
    in
    List.fold ~f ~init:(Map.empty (module Int)) (List.range 0 n_cols)

  let centre_offset = Map.find_exn col_offsets centre_col

  (* TODO: Consider best way to use this info. Use to set a clearance? Or simply
   * add a warning to the user if this value is too low? *)
  let lowest_z =
    let f ~key:_ ~data:(x, y, z) low =
      Math.rotate_y
        tent
        Util.(
          (x +. (Col.Key.outer_w /. 2.), y, z -. (Col.Key.thickness /. 2.))
          <-> centre_offset)
      |> Util.( <+> ) centre_offset
      |> Util.get_z
      |> Float.min low
    in
    Map.fold ~f ~init:Float.max_value col_offsets

  let bottom_marker = Model.cube (100., 1., 1.) |> Model.translate (0., 0., lowest_z)

  let place_col off =
    Col.(rotate_about_pt (0., tent, 0.) Util.(off <-> centre_offset) t |> translate off)

  let columns = Map.map ~f:place_col col_offsets

  let thumb =
    Thumb.(rotate thumb_angle t |> translate Util.(thumb_offset <+> (0., 0., rise)))

  let scad =
    Model.union
      (Map.fold
         ~f:(fun ~key:_ ~data l -> data.scad :: l )
           (* ~init:[ thumb.scad; bottom_marker ] *)
         ~init:[ thumb.scad ]
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

module NizBot = NizBottom.Make (Key)
