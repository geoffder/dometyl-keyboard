open! Base
open! Scad_ml
open! Infix

type 'k t =
  { scad : Model.t
  ; plate : 'k Plate.t
  ; walls : Walls.t
  }

let cap = Model.import "../things/SA-R3.stl" |> Model.color Color.DarkSlateBlue
let keyhole = KeyHole.make ~cap Mx.hole_config
let plate = Plate.make ~clearance:Mx.plate_clearance keyhole

let skel =
  (* NOTE: temporary inclusion of east wall on thumb, and the incl east thumb param.
     Need handle very vertical keys better. Smarter clearance, as well as not becoming
     very thin / losing sturdiness. *)
  let walls =
    Walls.
      { body = Body.make plate
      ; thumb =
          Thumb.make
            ~south_lookup:(fun i -> if i = 1 then Screw else Yes)
            ~east:No
            ~n_steps:(`Flat 3)
            plate
      }
  in
  let connections =
    Connect.skeleton
      ~height:7.
      ~thumb_height:11.
      ~snake_scale:2.
      ~snake_d:1.
      ~cubic_d:4.
      ~cubic_scale:1.5
      ~join_steps:4
      ~fudge_factor:8.
      ~close_thumb:true
      ~close_pinky:false
      walls
  in
  { scad =
      Model.union
        [ plate.scad
        ; Walls.to_scad walls
        ; connections.scad
        ; Plate.skeleton_bridges plate (* ; Plate.column_joins plate *)
        ; List.map ~f:Vec3.to_vec2 connections.outline
          |> Model.polygon
          |> Model.linear_extrude ~height:2.
          |> Model.translate (0., 0., -20.)
        ]
  ; plate
  ; walls
  }

let closed =
  let walls =
    Walls.
      { body =
          Body.make
            ~west_lookup:(fun i -> if i = 0 then Screw else Yes)
            ~east_lookup:(fun _ -> Yes)
            ~n_steps:(`PerZ 3.5)
            plate
      ; thumb =
          Thumb.make ~east:No ~south_lookup:(fun i -> if i = 1 then Screw else Yes) plate
      }
  in
  let connections = Connect.closed ~n_steps:4 walls in
  { scad =
      Model.union
        [ plate.scad
        ; Walls.to_scad walls
        ; connections.scad
        ; Plate.column_joins plate
        ; List.map ~f:Vec3.to_vec2 connections.outline
          |> Model.polygon
          |> Model.linear_extrude ~height:2.
          |> Model.translate (0., 0., -20.)
        ]
  ; plate
  ; walls
  }

let all_caps =
  let collect ~key:_ ~data acc =
    Option.value_map ~default:acc ~f:(fun c -> c :: acc) data.KeyHole.cap
  in
  let body_caps =
    Map.fold
      ~init:[]
      ~f:(fun ~key:_ ~data acc -> Map.fold ~init:acc ~f:collect data.Column.keys)
      plate.columns
  in
  Model.union @@ Map.fold ~init:body_caps ~f:collect plate.thumb.keys

let t = skel
let ports = Ports.make t.walls
let t = { t with scad = Model.difference t.scad [ ports ] }

(* let t = { t with scad = Model.union [ t.scad; all_caps ] } *)
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
      ; snap_len = 0.8 (* Try 1.2, see if it's possiblle *)
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
