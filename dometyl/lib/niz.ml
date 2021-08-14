open! Base
open! Scad_ml

module Bottom = struct
  let x = 17.5 (* NOTE: was 17.25 *)

  let y = 17.5
  let z = 4.
  let bulge_thickness = 0.5
  let bulge_length = 6.5
  let bulge_height = 3.2
  let ellipse_offset = -0.25
  let ellipse_inset_x_rad = 1.6 (* was 1.4 *)

  let ellipse_inset_y_scale = 1.2
  let corner_cut_rad = 5.
  let corner_cut_off = 2.75

  let ellipse =
    Model.scale (1., ellipse_inset_y_scale, 1.) (Model.circle ~fn:32 ellipse_inset_x_rad)
    |> Model.translate ((x /. 2.) +. ellipse_offset, 0., 0.)

  let bulge =
    Model.cube (bulge_length, bulge_thickness +. 0.1, bulge_height)
    |> Model.translate (bulge_length /. -2., (y /. 2.) -. 0.1, 0.)

  let cutter =
    Model.circle ~fn:64 corner_cut_rad
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

let plate_clearance = 6.5

let hole_config =
  let clip_height = 1.0
  and snap_slot_h = 1.2
  and snap_outer_wall = 0.2
  and outer_w = 19.5
  and inner_w = 14.
  and inner_h = 14.
  and thickness = 4.
  and cap_height = 5. (* mx is 6.25, but niz seems to measure at 4.9 ~ 5. *) in
  let clip hole =
    let inset_depth = thickness -. clip_height in
    let inset =
      Model.square ~center:true (Bottom.x, Bottom.y)
      |> Model.linear_extrude ~height:(inset_depth +. 0.01)
      |> Model.translate (0., 0., (thickness /. -2.) -. 0.01)
    in
    let bot = Model.translate (0., 0., (Bottom.z /. -2.) -. clip_height) Bottom.scad in
    let snap =
      let w = Bottom.ellipse_inset_x_rad *. Bottom.ellipse_inset_y_scale *. 2. in
      let slot =
        let len = outer_w -. inner_w in
        Model.cube ~center:true (len, w, snap_slot_h)
        |> Model.translate
             ( (outer_w /. 2.) -. (len /. 2.) -. snap_outer_wall
             , 0.
             , ((thickness -. snap_slot_h) /. 2.) -. clip_height )
      and ramp =
        let z = thickness -. clip_height in
        Model.polygon [ 0., z /. -2.; snap_slot_h, z /. -2.; 0., z /. 2. ]
        |> Model.linear_extrude ~height:w
        |> Model.rotate (Float.pi /. 2., 0., 0.)
        |> Model.translate (Bottom.x /. 2., w /. 2., z /. -2.)
      in
      Model.union [ slot; ramp ]
    in
    Model.difference hole [ inset; bot; snap; Model.mirror (1, 0, 0) snap ]
  in
  KeyHole.
    { spec = Kind.Niz { clip_height; snap_slot_h }
    ; outer_w
    ; inner_w
    ; inner_h
    ; thickness
    ; clip
    ; cap_height
    }

module Platform = struct
  type config =
    { w : float
    ; dome_w : float
    ; dome_waist : float
    ; dome_thickness : float
    ; base_thickness : float
    ; sensor_depth : float
    ; lug_height : float
    ; snap_clearance : float
    ; snap_len : float
    ; sensor_config : Sensor.Config.t
    }

  type t =
    { config : config
    ; wall_height : float
    ; scad : Model.t
    }

  let make
      ( { w
        ; dome_w
        ; dome_waist
        ; dome_thickness
        ; base_thickness
        ; sensor_depth
        ; lug_height
        ; snap_clearance
        ; snap_len
        ; sensor_config
        } as config )
    =
    let (KeyHole.Kind.Niz { clip_height; snap_slot_h }) = hole_config.spec in
    let base =
      let slab =
        Model.cube ~center:true (w, w, base_thickness)
        |> Model.translate (0., 0., base_thickness /. -2.)
      in
      Model.difference slab [ Sensor.(sink (make sensor_config) sensor_depth) ]
    in
    (* NOTE: Don't know why these config values on their own are resulting in a bit
     * of a gap between walls and plate. Possibly settle on a magic fudge value if the
     * issue persists / I don't find a better solution or the bug causing it. *)
    let wall_height =
      Bottom.z -. hole_config.thickness +. clip_height +. dome_thickness +. 0.25
    in
    let pillars =
      let waist_cut =
        let width = Bottom.ellipse_inset_x_rad *. Bottom.ellipse_inset_y_scale *. 2. in
        Model.polygon
          [ 0., 0.
          ; dome_waist, 0.
          ; dome_waist, dome_thickness
          ; dome_waist -. 0.5, dome_thickness +. 0.5
          ; 0.5, dome_thickness +. 0.5
          ; 0., dome_thickness
          ]
        |> Model.linear_extrude ~height:width
        |> Model.translate (dome_waist /. -2., 0., width /. -2.)
        |> Model.rotate (Float.pi /. 2., 0., 0.)
      in
      let cyl =
        Model.difference
          (Bottom.ellipse |> Model.linear_extrude ~height:wall_height)
          [ Model.cube
              ( Bottom.ellipse_inset_x_rad
              , Bottom.ellipse_inset_x_rad *. Bottom.ellipse_inset_y_scale *. 2.
              , wall_height )
            |> Model.translate
                 ( w /. 2.
                 , Bottom.ellipse_inset_x_rad *. Bottom.ellipse_inset_y_scale *. -1.
                 , 0. )
          ]
      in
      Model.difference (Model.union [ cyl; Model.mirror (1, 0, 0) cyl ]) [ waist_cut ]
    and dome_cut =
      (* ensure overlap *)
      let fudged_w = dome_w +. 0.01 in
      Model.cube (fudged_w, fudged_w, dome_thickness)
      |> Model.translate (fudged_w /. -2., fudged_w /. -2., 0.)
    in
    let ramp_cut =
      (* ~30 degrees *)
      let cut_l = 5.7 in
      let cut_h = 4. in
      let half_l = cut_l /. 2. in
      let half_h = cut_h /. 2. in
      let poly = Model.polygon [ 0., 0.; cut_h, 0.; cut_h, cut_l ] in
      let x_prism =
        Model.linear_extrude ~height:Bottom.x poly
        |> Model.translate (-.half_h, -.half_l, Bottom.x /. -2.)
        |> Model.rotate (Float.pi /. 2., Float.pi /. 2., Float.pi /. 2.)
        |> Model.translate (0., (dome_w /. 2.) -. half_l, half_h +. dome_thickness)
      in
      let y_prism =
        Model.difference
          ( Model.linear_extrude ~height:Bottom.y poly
          |> Model.translate (-.half_h, -.half_l, Bottom.y /. -2.)
          |> Model.rotate (Float.pi /. 2., Float.pi /. 2., 0.)
          |> Model.translate ((dome_w /. 2.) -. half_l, 0., half_h +. dome_thickness) )
          [ Model.scale (1., 1., 1.2) pillars ]
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
              [ Model.rotate (0., 0., Float.pi /. -2.) block
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
    and lugs =
      Model.difference
        (Model.cube ~center:true (Bottom.x -. 0.001, Bottom.y -. 0.001, lug_height))
        [ Model.translate (0., 0., -1.) Bottom.scad ]
      |> Model.translate (0., 0., (lug_height /. 2.) +. wall_height -. 0.001)
    in
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
    and snap_heads =
      let width = 2. *. Bottom.ellipse_inset_x_rad *. Bottom.ellipse_inset_y_scale
      and z =
        wall_height
        +. hole_config.thickness
        -. clip_height
        -. snap_slot_h
        +. (snap_clearance /. 2.)
      in
      let tab =
        Model.cube (snap_len, width, snap_slot_h -. snap_clearance)
        |> Model.translate (Bottom.x /. 2., width /. -2., z)
      in
      let neck =
        Model.difference
          Bottom.ellipse
          [ Model.square (Bottom.ellipse_inset_x_rad, width)
            |> Model.translate (Bottom.x /. 2., width /. -2., 0.)
          ]
        |> Model.linear_extrude ~height:(z -. wall_height)
        |> Model.translate (0., 0., wall_height +. snap_slot_h -. snap_clearance)
      in
      Model.union [ tab; Model.mirror (1, 0, 0) tab; neck; Model.mirror (1, 0, 0) neck ]
    in
    { config
    ; wall_height
    ; scad =
        Model.union [ base; Model.translate (0., 0., -0.001) walls; pillars; snap_heads ]
    }
end
