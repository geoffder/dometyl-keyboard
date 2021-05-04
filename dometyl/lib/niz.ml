open! Base
open! Scad_ml

module Top = struct
  let clip_height = 1.30
end

module Bottom = struct
  let x = 17.15
  let y = 17.5
  let z = 4.
  let bulge_thickness = 0.5
  let bulge_length = 6.5
  let bulge_height = 3.2
  let ellipse_inset_x_rad = 1.5
  let circle_inset_y_scale = 1.2
  let corner_cut_rad = 5.
  let corner_cut_off = 2.75

  let ellipse =
    Model.scale (1., circle_inset_y_scale, 1.) (Model.circle ellipse_inset_x_rad)
    |> Model.translate (x /. 2., 0., 0.)

  let bulge =
    Model.cube (bulge_length, bulge_thickness +. 0.1, bulge_height)
    |> Model.translate (bulge_length /. -2., (y /. 2.) -. 0.1, 0.)

  let cutter =
    Model.circle corner_cut_rad
    |> Model.translate ((x /. 2.) +. corner_cut_off, (y /. 2.) +. corner_cut_off, 0.)

  let scad =
    Model.difference
      (Model.square ~center:true (x, y))
      [ ellipse
      ; Model.mirror (1, 0, 0) ellipse
      ; cutter
      ; Model.mirror (1, 0, 0) cutter
      ; Model.mirror (1, 1, 0) cutter
      ; Model.mirror (0, 1, 0) cutter
      ]
    |> Model.linear_extrude ~height:z
    |> fun b -> Model.union [ b; bulge; Model.mirror (0, 1, 0) bulge ]
end

module HoleConfig : KeyHole.Config = struct
  let outer_w = 19.5
  let inner_w = 14.
  let thickness = 4.

  let clip hole =
    let inset_depth = thickness -. Top.clip_height in
    let inset =
      Model.square ~center:true (inner_w +. 3.15, inner_w +. 3.5)
      |> Model.linear_extrude ~height:(inset_depth +. 0.1)
      |> Model.translate (0., 0., (thickness /. -2.) -. 0.1)
    in
    let bot =
      Model.translate (0., 0., (Bottom.z /. -2.) -. Top.clip_height) Bottom.scad
    in
    Model.difference hole [ inset; bot ]
end

module Platform = struct
  let w = 21.
  let dome_w = 18.8

  (* NOTE: BKE ~= 0.95mm; DES ~= 0.73mm
   *  A value of 1.15 fits the BKE domes fine, though, may be able to get tighter.
   * Since DES are thinner, the tightness of fit between the platform and case plate may
   * suffer and make clipping with a generic value difficult. Should paramaterize so that
   * it can be easily adjusted for the the chosen brand of dome. *)
  let dome_thickness = 1.15
  let base_thickness = 1.5
  let bottom_scale_factor = 1. (* downsize for tighter fit? *)

  let base =
    Model.cube ~center:true (w, w, base_thickness)
    |> Model.translate (0., 0., base_thickness /. -2.)

  (* NOTE: Don't know why these config values on their own are resulting in a bit
   * of a gap between walls and plate. Possibly settle on a magic fudge value if the
   * issue persists / I don't find a better solution or the bug causing it. *)
  let wall_height =
    Bottom.z -. HoleConfig.thickness +. Top.clip_height +. dome_thickness +. 0.25

  let lug_height = 1.5

  let dome_cut =
    let dt = dome_thickness in
    let poly = Model.polygon [ dt /. 2., 0.; dt, 0.; dt, dt; dt /. 2., dt /. 2. ] in
    let prism =
      poly
      |> Model.linear_extrude ~height:dome_w
      |> Model.translate (dt /. -2., dt /. -2., -.dome_w /. 2.)
      |> Model.rotate (Math.pi /. 2., 0., 0.)
      |> Model.translate ((-.dome_w /. 2.) -. (dt /. 2.), 0., dt /. 2.)
    in
    let corner =
      let face =
        poly
        |> Model.linear_extrude ~height:0.1
        |> Model.translate (-.dt, dt /. -2., 0.)
        |> Model.rotate (Math.pi /. 2., 0., 0.)
      in
      Model.hull [ face; Model.rotate (0., 0., Math.pi /. -2.) face ]
      |> Model.translate (dome_w /. -2., dome_w /. 2., dt /. 2.)
    in
    (* ensure overlap *)
    let fudged_w = dome_w +. 0.01 in
    Model.union
      [ prism
      ; Model.mirror (1, 0, 0) prism
      ; Model.mirror (1, 1, 0) prism |> Model.mirror (0, 1, 0)
      ; Model.mirror (1, 1, 0) prism
      ; corner
      ; Model.mirror (1, 0, 0) corner
      ; Model.mirror (0, 1, 0) corner
      ; Model.mirror (0, 1, 0) corner |> Model.mirror (1, 0, 0)
      ; Model.cube (fudged_w, fudged_w, dome_thickness)
        |> Model.translate (fudged_w /. -2., fudged_w /. -2., 0.)
      ]

  let ramp_cut =
    let cut_l = 5.7 in
    let cut_h = 4. in
    (* let cut_l = 4. in
     * let cut_h = 4. in *)
    let half_l = cut_l /. 2. in
    let half_h = cut_h /. 2. in
    let poly = Model.polygon [ 0., 0.; cut_h, 0.; cut_h, cut_l ] in
    let x_prism =
      Model.linear_extrude ~height:Bottom.x poly
      |> Model.translate (-.half_h, -.half_l, Bottom.x /. -2.)
      |> Model.rotate (Math.pi /. 2., Math.pi /. 2., Math.pi /. 2.)
      |> Model.translate (0., (dome_w /. 2.) -. half_l, half_h +. dome_thickness)
    in
    let y_prism =
      Model.linear_extrude ~height:Bottom.y poly
      |> Model.translate (-.half_h, -.half_l, Bottom.y /. -2.)
      |> Model.rotate (Math.pi /. 2., Math.pi /. 2., 0.)
      |> Model.translate ((dome_w /. 2.) -. half_l, 0., half_h +. dome_thickness)
    in
    let corners =
      let block =
        Model.cube ~center:true (dome_w, cut_l, cut_h)
        |> Model.translate (0., (dome_w /. 2.) -. half_l, half_h +. dome_thickness)
      in
      let intersect =
        let x =
          Model.difference
            x_prism
            [ Model.translate (0., (Bottom.y -. dome_w) /. 2., 0.) block ]
          |> Model.translate ((dome_w -. Bottom.x) /. 2., 0., 0.)
        and y =
          Model.difference
            y_prism
            [ Model.rotate (0., 0., Math.pi /. -2.) block
              |> Model.translate ((Bottom.x -. dome_w) /. 2., 0., 0.)
            ]
          |> Model.translate (0., (dome_w -. Bottom.y) /. 2., 0.)
        in
        Model.intersection [ x; y ]
      in
      Model.union
        [ intersect
        ; Model.mirror (0, 1, 0) intersect
        ; Model.mirror (1, 0, 0) intersect
        ; Model.mirror (1, 0, 0) intersect |> Model.mirror (0, 1, 0)
        ]
    in
    Model.union
      [ x_prism
      ; Model.mirror (0, 1, 0) x_prism
      ; y_prism
      ; Model.mirror (1, 0, 0) y_prism
      ; corners
      ]

  let lugs =
    Model.difference
      (Model.cube ~center:true (Bottom.x -. 0.001, Bottom.y -. 0.001, lug_height))
      [ Model.translate (0., 0., -1.) Bottom.scad ]
    |> Model.translate (0., 0., (lug_height /. 2.) +. wall_height -. 0.001)

  let walls =
    let block =
      Model.union
        [ Model.cube ~center:true (w, w, wall_height +. 0.001)
          |> Model.translate (0., 0., wall_height /. 2.)
        ; lugs
        ]
    in
    Model.difference
      block
      [ Model.translate (0., 0., -0.001) Bottom.scad; dome_cut; ramp_cut ]

  (* FIXME: does this union overlap (previously missing) fix the issue with
   * the gap in the print? Note that the keyhole also has bit of an overhang with
   * the clips. Should I make those angled, or not bother? *)
  let scad =
    Model.union
      [ base
      ; Model.translate (0., 0., -0.001) walls
        (* ; Model.translate (30., 0., 0.) dome_cut
         * ; Model.translate (30., 0., 0.) ramp_cut *)
      ]

  (* let scad =
   *   Model.difference
   *     scad
   *     [ Model.translate (0., 20., 0.) (Model.cube ~center:true (30., 30., 10.))
   *     ; Model.translate (-20., 0., 0.) (Model.cube ~center:true (30., 30., 10.))
   *     ] *)
end
