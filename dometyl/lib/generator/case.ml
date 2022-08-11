open! Base
open! Scad_ml
open! Infix

type 'k t =
  { scad : Scad.d3
  ; plate : 'k Plate.t
  ; walls : Walls.t
  ; connections : Connect.t
  }
[@@deriving scad]

let make
    ?(right_hand = true)
    ~plate_builder
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter
    keyhole
  =
  let plate =
    plate_builder (if right_hand then keyhole else KeyHole.mirror_internals keyhole)
  in
  let walls = wall_builder plate in
  let connections = base_connector walls in
  let t =
    { scad =
        Scad.difference
          (Scad.union
             [ Plate.to_scad plate
             ; Walls.to_scad walls
             ; Connect.to_scad connections
             ; plate_welder plate
             ] )
          [ Plate.collect_cutouts plate ]
        |> Ports.apply (ports_cutter ~walls ~connections)
    ; plate
    ; walls
    ; connections
    }
  in
  if right_hand then t else mirror (v3 1. 0. 0.) t

let to_scad ?(show_caps = false) ?(show_cutouts = false) t =
  let caps = if show_caps then Some (Plate.collect_caps t.plate) else None
  and cutouts =
    if show_cutouts
    then Some (Scad.color Color.Black (Plate.collect_cutouts t.plate))
    else None
  in
  [ t.scad ] |> Util.prepend_opt caps |> Util.prepend_opt cutouts |> Scad.union
