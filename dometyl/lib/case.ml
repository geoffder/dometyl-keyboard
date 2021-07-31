open! Base
open! Scad_ml
open! Infix

type 'k t =
  { scad : Model.t
  ; plate : 'k Plate.t
  ; walls : Walls.t
  }

let keyhole = KeyHole.make Niz.hole_config
let plate = Plate.make keyhole

let skel =
  let walls = Walls.{ body = Body.make plate; thumb = Thumb.make plate } in
  { scad =
      Model.union
        [ plate.scad
        ; Walls.to_scad walls
        ; Connect.skeleton ~height:7. ~snake_scale:1.5 ~snake_d:5. ~snake_height:10. walls
        ; Plate.skeleton_bridges plate
        ]
  ; plate
  ; walls
  }

let closed =
  let walls =
    Walls.
      { body =
          Body.make
            ~west_lookup:(fun _ -> true)
            ~east_lookup:(fun _ -> true)
            ~n_steps:(`PerZ 3.5)
            plate
      ; thumb = Thumb.make ~east:true plate
      }
  in
  { scad =
      Model.union
        [ plate.scad
        ; Walls.to_scad walls
        ; Connect.closed ~n_steps:4 walls
        ; Plate.column_joins plate
        ]
  ; plate
  ; walls
  }

let t = skel
let niz_sensor = Sensor.(make Config.a3144)

let niz_platform =
  (* NOTE: BKE ~= 0.95mm; DES ~= 0.73mm. A value of 1.15 seems to fit both without
   * being too tight or loose on either. *)
  Niz.Platform.(
    make
      { w = 20.
      ; dome_w = 19.
      ; dome_waist = 15. (* width at narrow point, ensure enough space at centre *)
      ; dome_thickness = 1.15
      ; base_thickness = 2.25
      ; sensor_depth = 1.5
      ; snap_clearance = 0.3
      ; snap_len = 0.7
      ; lug_height = 1.5
      ; sensor_config = Sensor.Config.a3144_print
      })

let niz_cross_section =
  Model.difference
    (Model.union
       [ Model.translate
           (0., 0., niz_platform.wall_height +. (keyhole.config.thickness /. 2.))
           keyhole.scad
       ; niz_platform.scad
       ] )
    [ Model.cube ~center:true (25., 15., 20.) |> Model.translate (0., -7.5, 0.) ]
