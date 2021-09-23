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
    Scad.scale (1., ellipse_inset_y_scale, 1.) (Scad.circle ~fn:32 ellipse_inset_x_rad)
    |> Scad.translate ((x /. 2.) +. ellipse_offset, 0., 0.)

  let bulge =
    Scad.cube (bulge_length, bulge_thickness +. 0.1, bulge_height)
    |> Scad.translate (bulge_length /. -2., (y /. 2.) -. 0.1, 0.)

  let cutter =
    Scad.circle ~fn:64 corner_cut_rad
    |> Scad.translate ((x /. 2.) +. corner_cut_off, (y /. 2.) +. corner_cut_off, 0.)

  let scad =
    Scad.difference
      (Scad.square ~center:true (x, y))
      [ ellipse
      ; Scad.mirror (1., 0., 0.) ellipse
      ; cutter
      ; Scad.mirror (1., 0., 0.) cutter
      ; Scad.mirror (1., 1., 0.) cutter
      ; Scad.mirror (0., 1., 0.) cutter
      ]
    |> Scad.linear_extrude ~height:z
    |> fun b -> Scad.union [ b; bulge; Scad.mirror (0., 1., 0.) bulge ]
end

let hole_config =
  let clip_height = 1.0
  and snap_slot_h = 1.2
  and snap_outer_wall = 0.2
  and outer_w = 19.5
  and outer_h = 19.5
  and inner_w = 14.
  and inner_h = 14.
  and thickness = 4.
  and cap_height = 5. (* mx is 6.25, but niz seems to measure at 4.9 ~ 5. *)
  and clearance = 6.5 in
  let clip hole =
    let inset_depth = thickness -. clip_height in
    let inset =
      Scad.square ~center:true (Bottom.x, Bottom.y)
      |> Scad.linear_extrude ~height:(inset_depth +. 0.01)
      |> Scad.translate (0., 0., (thickness /. -2.) -. 0.01)
    in
    let bot = Scad.translate (0., 0., (Bottom.z /. -2.) -. clip_height) Bottom.scad in
    let snap =
      let w = Bottom.ellipse_inset_x_rad *. Bottom.ellipse_inset_y_scale *. 2. in
      let slot =
        let len = outer_w -. inner_w in
        Scad.cube ~center:true (len, w, snap_slot_h)
        |> Scad.translate
             ( (outer_w /. 2.) -. (len /. 2.) -. snap_outer_wall
             , 0.
             , ((thickness -. snap_slot_h) /. 2.) -. clip_height )
      and ramp =
        let z = thickness -. clip_height in
        Scad.polygon [ 0., z /. -2.; snap_slot_h, z /. -2.; 0., z /. 2. ]
        |> Scad.linear_extrude ~height:w
        |> Scad.rotate (Float.pi /. 2., 0., 0.)
        |> Scad.translate (Bottom.x /. 2., w /. 2., z /. -2.)
      in
      Scad.union [ slot; ramp ]
    in
    Scad.difference hole [ inset; bot; snap; Scad.mirror (1., 0., 0.) snap ]
  in
  KeyHole.
    { spec = Kind.Niz { clip_height; snap_slot_h }
    ; outer_w
    ; outer_h
    ; inner_w
    ; inner_h
    ; thickness
    ; clip
    ; cap_height
    ; clearance
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

  (* NOTE: BKE ~= 0.95mm; DES ~= 0.73mm. A value of 1.15 seems to fit both without
   * being too tight or loose on either. *)
  let default_config =
    { w = 20.
    ; dome_w = 19.
    ; dome_waist = 15. (* width at narrow point, ensure enough space at centre *)
    ; dome_thickness = 1.15
    ; base_thickness = 2.25
    ; sensor_depth = 1.5
    ; snap_clearance = 0.3
    ; snap_len = 0.8 (* Try 1.2, see if it's possiblle *)
    ; lug_height = 1.5
    ; sensor_config = Sensor.Config.a3144_print
    }

  type t =
    { config : config
    ; wall_height : float
    ; scad : Scad.t
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
        Scad.cube ~center:true (w, w, base_thickness)
        |> Scad.translate (0., 0., base_thickness /. -2.)
      in
      Scad.difference slab [ Sensor.(sink (make sensor_config) sensor_depth) ]
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
        Scad.polygon
          [ 0., 0.
          ; dome_waist, 0.
          ; dome_waist, dome_thickness
          ; dome_waist -. 0.5, dome_thickness +. 0.5
          ; 0.5, dome_thickness +. 0.5
          ; 0., dome_thickness
          ]
        |> Scad.linear_extrude ~height:width
        |> Scad.translate (dome_waist /. -2., 0., width /. -2.)
        |> Scad.rotate (Float.pi /. 2., 0., 0.)
      in
      let cyl =
        Scad.difference
          (Bottom.ellipse |> Scad.linear_extrude ~height:wall_height)
          [ Scad.cube
              ( Bottom.ellipse_inset_x_rad
              , Bottom.ellipse_inset_x_rad *. Bottom.ellipse_inset_y_scale *. 2.
              , wall_height )
            |> Scad.translate
                 ( w /. 2.
                 , Bottom.ellipse_inset_x_rad *. Bottom.ellipse_inset_y_scale *. -1.
                 , 0. )
          ]
      in
      Scad.difference (Scad.union [ cyl; Scad.mirror (1., 0., 0.) cyl ]) [ waist_cut ]
    and dome_cut =
      (* ensure overlap *)
      let fudged_w = dome_w +. 0.01 in
      Scad.cube (fudged_w, fudged_w, dome_thickness)
      |> Scad.translate (fudged_w /. -2., fudged_w /. -2., 0.)
    in
    let ramp_cut =
      (* ~30 degrees *)
      let cut_l = 5.7 in
      let cut_h = 4. in
      let half_l = cut_l /. 2. in
      let half_h = cut_h /. 2. in
      let poly = Scad.polygon [ 0., 0.; cut_h, 0.; cut_h, cut_l ] in
      let x_prism =
        Scad.linear_extrude ~height:Bottom.x poly
        |> Scad.translate (-.half_h, -.half_l, Bottom.x /. -2.)
        |> Scad.rotate (Float.pi /. 2., Float.pi /. 2., Float.pi /. 2.)
        |> Scad.translate (0., (dome_w /. 2.) -. half_l, half_h +. dome_thickness)
      in
      let y_prism =
        Scad.difference
          ( Scad.linear_extrude ~height:Bottom.y poly
          |> Scad.translate (-.half_h, -.half_l, Bottom.y /. -2.)
          |> Scad.rotate (Float.pi /. 2., Float.pi /. 2., 0.)
          |> Scad.translate ((dome_w /. 2.) -. half_l, 0., half_h +. dome_thickness) )
          [ Scad.scale (1., 1., 1.2) pillars ]
      in
      let corners =
        let block =
          Scad.cube ~center:true (dome_w, cut_l, cut_h)
          |> Scad.translate (0., (dome_w /. 2.) -. half_l, half_h +. dome_thickness)
        in
        let intersect =
          let x =
            Scad.difference
              x_prism
              [ Scad.translate (0., (Bottom.y -. dome_w) /. 2., 0.) block ]
            |> Scad.translate ((dome_w -. Bottom.x) /. 2., 0., 0.)
          and y =
            Scad.difference
              y_prism
              [ Scad.rotate (0., 0., Float.pi /. -2.) block
                |> Scad.translate ((Bottom.x -. dome_w) /. 2., 0., 0.)
              ]
            |> Scad.translate (0., (dome_w -. Bottom.y) /. 2., 0.)
          in
          Scad.intersection [ x; y ]
        in
        Scad.union
          [ intersect
          ; Scad.mirror (0., 1., 0.) intersect
          ; Scad.mirror (1., 0., 0.) intersect
          ; Scad.mirror (1., 0., 0.) intersect |> Scad.mirror (0., 1., 0.)
          ]
      in
      Scad.union
        [ x_prism
        ; Scad.mirror (0., 1., 0.) x_prism
        ; y_prism
        ; Scad.mirror (1., 0., 0.) y_prism
        ; corners
        ]
    and lugs =
      Scad.difference
        (Scad.cube ~center:true (Bottom.x -. 0.001, Bottom.y -. 0.001, lug_height))
        [ Scad.translate (0., 0., -1.) Bottom.scad ]
      |> Scad.translate (0., 0., (lug_height /. 2.) +. wall_height -. 0.001)
    in
    let walls =
      let block =
        Scad.union
          [ Scad.cube ~center:true (w, w, wall_height +. 0.001)
            |> Scad.translate (0., 0., wall_height /. 2.)
          ; lugs
          ]
      in
      Scad.difference
        block
        [ Scad.translate (0., 0., -0.001) Bottom.scad; dome_cut; ramp_cut ]
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
        Scad.cube (snap_len, width, snap_slot_h -. snap_clearance)
        |> Scad.translate (Bottom.x /. 2., width /. -2., z)
      in
      let neck =
        Scad.difference
          Bottom.ellipse
          [ Scad.square (Bottom.ellipse_inset_x_rad, width)
            |> Scad.translate (Bottom.x /. 2., width /. -2., 0.)
          ]
        |> Scad.linear_extrude ~height:(z -. wall_height)
        |> Scad.translate (0., 0., wall_height +. snap_slot_h -. snap_clearance)
      in
      Scad.union
        [ tab; Scad.mirror (1., 0., 0.) tab; neck; Scad.mirror (1., 0., 0.) neck ]
    in
    { config
    ; wall_height
    ; scad =
        Scad.union [ base; Scad.translate (0., 0., -0.001) walls; pillars; snap_heads ]
    }
end

let example_cross_section =
  let platform = Platform.(make default_config)
  and keyhole = KeyHole.make hole_config in
  Scad.difference
    (Scad.union
       [ Scad.translate
           (0., 0., platform.wall_height +. (keyhole.config.thickness /. 2.))
           keyhole.scad
       ; platform.scad
       ] )
    [ Scad.cube ~center:true (25., 15., 20.) |> Scad.translate (0., -7.5, 0.) ]
