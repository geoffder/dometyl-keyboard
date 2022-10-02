open! Scad_ml
open! Syntax

type t =
  { scad : Scad.d3
  ; plate : Plate.t
  ; plate_glue : Scad.d3
  ; walls : Walls.t
  ; connections : Connect.t
  ; eyelets : Eyelet.t list
  }
[@@deriving scad]

let make
    ?(right_hand = true)
    ?(eyelet_config = Eyelet.m4_config)
    ?(wall_eyelets = Eyelet.default_wall_locs)
    ?(free_eyelets = [])
    ~plate_builder
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter
    keyhole
  =
  let plate =
    plate_builder (if right_hand then keyhole else Key.mirror_internals keyhole)
  in
  let plate_glue = plate_welder plate
  and walls = wall_builder plate in
  let connections = base_connector walls in
  let eyelets =
    let locs = List.append free_eyelets (Eyelet.wall_locations ~walls wall_eyelets) in
    List.map
      (Eyelet.place
         ~config:eyelet_config
         ~inline:connections.Connect.inline
         ~outline:connections.outline )
      locs
  and scad =
    Scad.sub
      (Scad.union
         [ Plate.to_scad plate
         ; plate_glue
         ; Walls.to_scad walls
         ; Connect.to_scad connections
         ] )
      (Plate.collect_cutouts plate)
    |> Ports.apply (ports_cutter ~walls ~connections)
  in
  let t =
    { scad = List.fold_left (fun s eye -> Eyelet.apply eye s) scad eyelets
    ; plate
    ; plate_glue
    ; walls
    ; connections
    ; eyelets
    }
  in
  if right_hand then t else mirror (v3 1. 0. 0.) t

let plate_scad t = Scad.add (Plate.to_scad t.plate) t.plate_glue

let to_scad ?(show_caps = false) ?(show_cutouts = false) t =
  let caps = if show_caps then Some (Plate.collect_caps t.plate) else None
  and cutouts =
    if show_cutouts
    then Some (Scad.color Color.Black (Plate.collect_cutouts t.plate))
    else None
  in
  [ t.scad ] |> Util.prepend_opt caps |> Util.prepend_opt cutouts |> Scad.union
