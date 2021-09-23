open! Base
open! Scad_ml
open! Infix

type 'k t =
  { scad : Scad.t
  ; plate : 'k Plate.t
  ; walls : Walls.t
  ; connections : Connect.t
  }

let translate p t =
  { scad = Scad.translate p t.scad
  ; plate = Plate.translate p t.plate
  ; walls = Walls.translate p t.walls
  ; connections = Connect.translate p t.connections
  }

let mirror ax t =
  { scad = Scad.mirror ax t.scad
  ; plate = Plate.mirror ax t.plate
  ; walls = Walls.mirror ax t.walls
  ; connections = Connect.mirror ax t.connections
  }

let rotate r t =
  { scad = Scad.rotate r t.scad
  ; plate = Plate.rotate r t.plate
  ; walls = Walls.rotate r t.walls
  ; connections = Connect.rotate r t.connections
  }

let rotate_about_pt r p t =
  { scad = Scad.rotate_about_pt r p t.scad
  ; plate = Plate.rotate_about_pt r p t.plate
  ; walls = Walls.rotate_about_pt r p t.walls
  ; connections = Connect.rotate_about_pt r p t.connections
  }

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
  if right_hand then t else mirror (1., 0., 0.) t

let to_scad ?(show_caps = false) ?(show_cutouts = false) t =
  let caps = if show_caps then Some (Plate.collect_caps t.plate) else None
  and cutouts =
    if show_cutouts
    then Some (Scad.color Color.Black (Plate.collect_cutouts t.plate))
    else None
  in
  [ t.scad ] |> Util.prepend_opt caps |> Util.prepend_opt cutouts |> Scad.union
