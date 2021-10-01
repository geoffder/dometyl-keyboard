open! Base
open! Scad_ml

(* TODO:
   - remaining basic foot positions
   - configuration of foot inset positions?
      -> sum type with manual coordinates or key position lookup
         (big improvement over the current situation at least) *)

let make ?(thickness = 1.65) ?fastener ?(bumpon_rad = 5.5) ?(bumpon_inset = 0.5) case =
  let inset_loc KeyHole.{ origin = x, y, _; _ } = x, y, 0. in
  let screws = Walls.collect_screws case.Case.walls in
  let screw_config = (List.hd_exn screws).config in
  let thickness, screw_hole =
    let fastener =
      match fastener with
      | None          ->
        ( match screw_config with
        | { hole = Through; _ } -> Screw.screw_fastener ()
        | _                     -> Magnet )
      | Some fastener -> fastener
    in
    match fastener with
    | Screw { head_rad; shaft_rad; sink = Counter; _ } ->
      ( thickness
      , Scad.circle head_rad
        |> Scad.linear_extrude ~height:thickness ~scale:(shaft_rad /. head_rad) )
    | Screw { head_rad; shaft_rad; sink = Pan depth; _ } ->
      ( thickness
      , Scad.union
          [ Scad.cylinder ~fn:32 head_rad depth
          ; Scad.cylinder ~fn:32 shaft_rad thickness
          ] )
    | Magnet ->
      let Screw.{ inner_rad; thickness = thick; hole; _ } = screw_config in
      let h =
        match hole with
        | Screw.Inset inset -> inset
        | _                 -> 0.
      and thickness = Float.max thickness thick in
      ( thickness
      , Scad.union
          [ Scad.translate (0., 0., thickness -. h) @@ Scad.cylinder ~fn:32 inner_rad h
          ; Scad.cylinder ~fn:32 (inner_rad /. 2.) (thickness -. h)
          ] )
  and insets =
    let cut = Scad.cylinder bumpon_rad bumpon_inset in
    let top_left =
      let%bind.Option c = Map.find case.plate.columns 0 in
      let%map.Option _, k = Map.max_elt c.keys in
      k
    and top_ring =
      let%bind.Option c = Map.find case.plate.columns 3 in
      let%map.Option _, k = Map.max_elt c.keys in
      k
    and bot_left = Map.find case.plate.thumb.keys 0
    and right =
      match Map.max_elt case.plate.columns with
      | Some (_, c) -> [ Map.max_elt c.keys |> Option.map ~f:snd; Map.find c.keys 0 ]
      | None        -> []
    in
    bot_left :: top_left :: top_ring :: right
    |> List.filter_opt
    |> List.map ~f:(fun k -> Scad.translate (inset_loc k) cut)
    |> Scad.union
  in
  let plate =
    Scad.polygon (Connect.outline_2d case.connections)
    |> Scad.linear_extrude ~height:thickness
  in
  Scad.difference
    plate
    (insets :: List.map ~f:(fun l -> Scad.translate l.centre screw_hole) screws)
