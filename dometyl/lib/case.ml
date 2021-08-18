open! Base
open! Scad_ml
open! Infix

type 'k t =
  { scad : Model.t
  ; plate : 'k Plate.t
  ; walls : Walls.t
  ; connections : Connect.t
  }

let translate p t =
  { scad = Model.translate p t.scad
  ; plate = Plate.translate p t.plate
  ; walls = Walls.translate p t.walls
  ; connections = Connect.translate p t.connections
  }

let rotate r t =
  { scad = Model.rotate r t.scad
  ; plate = Plate.rotate r t.plate
  ; walls = Walls.rotate r t.walls
  ; connections = Connect.rotate r t.connections
  }

let rotate_about_pt r p t =
  { scad = Model.rotate_about_pt r p t.scad
  ; plate = Plate.rotate_about_pt r p t.plate
  ; walls = Walls.rotate_about_pt r p t.walls
  ; connections = Connect.rotate_about_pt r p t.connections
  }

let cap = Model.import "../things/SA-R3.stl" |> Model.color Color.DarkSlateBlue
let keyhole = KeyHole.make ~cap Mx.hole_config
let plate = Plate.make ~n_rows:3 ~n_cols:5 ~clearance:Mx.plate_clearance keyhole

let skel =
  let walls =
    Walls.
      { body = Body.make ~n_steps:(`Flat 3) ~clearance:1.5 plate
      ; thumb =
          Thumb.make
            ~south_lookup:(fun _ -> Yes)
            ~east:No
            ~west:Screw
            ~clearance:1.5
            ~n_steps:(`Flat 3)
            plate
      }
  in
  let connections =
    Connect.skeleton
      ~height:7.
      ~thumb_height:11.
      ~snake_scale:1.3
      ~snake_d:1.4
      ~cubic_d:2.
      ~cubic_scale:1.
      ~thumb_cubic_d:1.
      ~thumb_cubic_scale:1.25
      ~join_steps:3
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
        ; Bridge.cols ~columns:plate.columns 1 2
        ]
  ; plate
  ; walls
  ; connections
  }

let closed =
  let walls =
    Walls.
      { body =
          Body.make
            ~west_lookup:(fun i -> if i = 0 then Screw else Yes)
            ~east_lookup:(fun _ -> Yes)
            ~n_steps:(`PerZ 6.)
            plate
      ; thumb =
          Thumb.make
            ~east:No
            ~south_lookup:(fun i -> if i = 1 then Screw else Yes)
            ~n_steps:(`PerZ 6.)
            plate
      }
  in
  let connections = Connect.closed ~n_steps:4 walls in
  { scad =
      Model.union
        [ plate.scad; Walls.to_scad walls; connections.scad; Plate.column_joins plate ]
  ; plate
  ; walls
  ; connections
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
