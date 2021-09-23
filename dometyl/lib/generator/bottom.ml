open! Base
open! Scad_ml

(* TODO:
   - remaining basic foot positions
   - configuration of foot inset positions? *)
let make
    ?(thickness = 1.65)
    ?(outer_screw_rad = 4.1)
    ?(inner_screw_rad = 2.0)
    ?(bumpon_rad = 5.)
    ?(bumpon_inset = 0.5)
    case
  =
  let inset_loc KeyHole.{ origin = x, y, _; _ } = x, y, 0. in
  let screws = Walls.collect_screws case.Case.walls
  and screw_hole =
    Scad.circle outer_screw_rad
    |> Scad.linear_extrude ~height:thickness ~scale:(inner_screw_rad /. outer_screw_rad)
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
  and plate =
    Scad.polygon (Connect.outline_2d case.connections)
    |> Scad.linear_extrude ~height:thickness
  in
  Scad.difference
    plate
    (insets :: List.map ~f:(fun l -> Scad.translate l.centre screw_hole) screws)
