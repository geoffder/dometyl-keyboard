open! Base
open! Scad_ml

type hole =
  | Through
  | Inset of float

type sink =
  | Pan of float
  | Counter

type fastener =
  | SameMagnet
  | Magnet of
      { rad : float
      ; thickness : float
      }
  | Screw of
      { head_rad : float
      ; shaft_rad : float
      ; sink : sink
      ; height : float
      ; clearance : float
      }

type placement =
  | Normal of V2.t
  | Point of V2.t

type config =
  { outer_rad : float
  ; inner_rad : float
  ; thickness : float
  ; hole : hole
  }

type t =
  { scad : Scad.d3
  ; cut : Scad.d3 option
  ; centre : V3.t
  ; config : config [@scad.ignore]
  }
[@@deriving scad]

let translate p t = { t with scad = Scad.translate p t.scad; centre = V3.add p t.centre }
let mirror ax t = { t with scad = Scad.mirror ax t.scad; centre = V3.mirror ax t.centre }

let rotate ?about r t =
  { t with scad = Scad.rotate ?about r t.scad; centre = V3.rotate ?about r t.centre }

let screw_fastener
    ?(head_rad = 4.5)
    ?(shaft_rad = 2.)
    ?(sink = Counter)
    ?(height = 2.)
    ?(clearance = 0.)
    ()
  =
  Screw { head_rad; shaft_rad; sink; height; clearance }

let default_config = { outer_rad = 4.0; inner_rad = 2.0; thickness = 4.0; hole = Through }
let m4_config = { outer_rad = 5.; inner_rad = 2.7; thickness = 4.0; hole = Through }
let bumpon_config = { outer_rad = 5.8; inner_rad = 5.; thickness = 2.4; hole = Inset 0.6 }

let magnet_6x3_config =
  { outer_rad = 4.4; inner_rad = 3.15; thickness = 4.6; hole = Inset 3.2 }

let m4_countersunk_fastener = screw_fastener ()

let make ?(fn = 7) ~placement ({ outer_rad; inner_rad; thickness; hole } as config) p1 p2 =
  let base_centre = V2.mid p1 p2 in
  let hole_offset, foot_offset =
    match placement with
    | Normal n -> V2.smul n outer_rad, V2.smul n (-0.1)
    | Point p  ->
      let diff = V2.(p -@ base_centre) in
      diff, V2.smul (V2.normalize diff) (-0.1)
  in
  let hole_centre = V2.(base_centre +@ hole_offset) in
  let outer = Scad.circle ~fn:16 outer_rad |> Scad.translate hole_centre
  and inner = Scad.circle ~fn:16 inner_rad |> Scad.translate hole_centre
  and swoop p =
    let rad_offset = V2.(normalize (p -@ base_centre) *$ outer_rad) in
    hole_centre
    :: base_centre
    :: V2.add p foot_offset
    :: Bezier2.(
         curve
           ~fn
           (make V2.[ p; mid (base_centre +@ rad_offset) p; hole_centre +@ rad_offset ]))
    |> Scad.polygon
  in
  let outline = Scad.union [ outer; swoop p1; swoop p2 ] in
  let scad, cut =
    match hole with
    | Through     ->
      Scad.difference outline [ inner ] |> Scad.linear_extrude ~height:thickness, None
    | Inset depth ->
      let inset =
        Scad.union
          [ Scad.linear_extrude ~height:depth inner
          ; Scad.cylinder ~fn:16 ~height:(thickness -. depth) (inner_rad /. 2.)
            |> Scad.translate (V3.of_v2 ~z:depth hole_centre)
          ]
      and foot = Scad.linear_extrude ~height:thickness outline in
      Scad.difference foot [ inset ], Some inset
  in
  { scad; cut; centre = V3.of_v2 hole_centre; config }

let to_scad t = t.scad
