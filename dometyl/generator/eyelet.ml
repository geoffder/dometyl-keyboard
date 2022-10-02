open Scad_ml
open Syntax

type hole =
  | Through
  | Inset of
      { depth : float
      ; punch : [ `Rel of float | `Abs of float ] option
      }

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
      ; clearance : float option
      }

type wall_loc =
  | Body of [ `N | `E | `S | `W ] * Idx.t
  | Thumb of [ `N | `E | `S | `W ] * Idx.t

type placement =
  | Normal of V3.t
  | Point of V3.t

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

let inset ?punch depth = Inset { depth; punch }

let screw_fastener
    ?(head_rad = 4.5)
    ?(shaft_rad = 2.)
    ?(sink = Counter)
    ?(height = 2.)
    ?clearance
    ()
  =
  Screw { head_rad; shaft_rad; sink; height; clearance }

let default_config = { outer_rad = 4.0; inner_rad = 2.0; thickness = 4.0; hole = Through }
let m4_config = { outer_rad = 5.; inner_rad = 2.7; thickness = 4.0; hole = Through }
let bumpon_config = { outer_rad = 5.8; inner_rad = 5.; thickness = 2.4; hole = inset 0.6 }

let magnet_6x3_config =
  { outer_rad = 4.4
  ; inner_rad = 3.15
  ; thickness = 4.6
  ; hole = inset ~punch:(`Rel 0.5) 3.2
  }

let m4_countersunk_fastener = screw_fastener ()

let make ?(fn = 32) ~placement ({ outer_rad; inner_rad; thickness; hole } as config) ps =
  let p1 = List.hd ps
  and p2 = Util.last ps in
  let base_centre = (Path3.to_continuous ps) 0.5 in
  let normal, hole_offset =
    match placement with
    | Normal n -> n, V3.smul n (outer_rad +. 0.1)
    | Point p ->
      let diff = V3.(p -@ base_centre) in
      V3.normalize diff, diff
  in
  let hole_centre = V3.(base_centre +@ hole_offset) in
  let plane = Plane.make p1 p2 hole_centre in
  let proj = Plane.project plane
  and lift = Plane.to_affine ~op:`Lift plane in
  let hole_centre' = proj hole_centre
  and base_centre' = proj base_centre
  and p1' = proj p1
  and p2' = proj p2
  and hole_offset' = proj hole_offset in
  let l = V2.(hole_centre' +@ (v2 normal.y (-.normal.x) *$ outer_rad))
  and r = V2.(hole_centre' -@ (v2 normal.y (-.normal.x) *$ outer_rad)) in
  let outer = List.tl @@ Path2.arc_about_centre ~dir:`CW ~centre:hole_centre' ~fn l r
  and inner = List.rev_map (V2.add hole_centre') @@ Path2.circle ~fn inner_rad in
  let outline =
    let swoop_l =
      let rad_offset = V2.(normalize (p1' -@ base_centre') *$ outer_rad) in
      let c = V2.(base_centre' +@ rad_offset +@ (hole_offset' /$ 5.)) in
      List.tl @@ Bezier2.(curve ~fn (make [ p1'; c; l ]))
    and swoop_r =
      let rad_offset = V2.(normalize (p2' -@ base_centre') *$ outer_rad) in
      let c = V2.(base_centre' +@ rad_offset +@ (hole_offset' /$ 5.)) in
      List.tl @@ Bezier2.(curve ~fn (make [ r; c; p2' ]))
    and fudge = V2.(proj normal *$ -0.01) in
    let wall_ps = List.tl @@ List.rev_map (fun p -> V2.add fudge (proj p)) ps in
    List.concat [ wall_ps; swoop_l; outer; swoop_r ]
  in
  let scad, cut =
    try
      match hole with
      | Through ->
        ( Poly2.make ~holes:[ inner ] outline
          |> Poly2.to_scad
          |> Scad.extrude ~height:thickness
        , None )
      | Inset { depth = d; punch } ->
        let inset =
          Poly2.validation (Poly2.make ~holes:[ inner ] outline);
          let s = Scad.(ztrans (-0.01) @@ extrude ~height:(d +. 0.01) (polygon inner)) in
          match punch with
          | Some punch ->
            let rad =
              match punch with
              | `Abs r -> r
              | `Rel r -> inner_rad *. r
            in
            Scad.cylinder ~fn ~height:(thickness -. d +. 0.02) rad
            |> Scad.translate (V3.of_v2 ~z:(d -. 0.01) hole_centre')
            |> Scad.add s
          | None -> s
        and foot = Scad.extrude ~height:thickness (Scad.polygon outline) in
        Scad.affine lift @@ Scad.difference foot [ inset ], Some (Scad.affine lift inset)
    with
    | Poly2.(SelfIntersection _ | CrossIntersection _) ->
      failwith
      @@ Printf.sprintf
           "Outline of eyelet at %s contains intersections. Consider decreasing the \
            width of the foot points or burying them deeper."
           (V3.to_string hole_centre)
  in
  { scad; cut; centre = hole_centre; config }

let place ?(fn = 16) ?width ?(bury = 0.4) ?(config = m4_config) ~inline ~outline loc =
  let half_width =
    Util.value_map_opt ~default:(config.outer_rad +. 1.25) (( *. ) 0.5) width
  in
  let len_inline = Path3.length ~closed:true inline
  and cont_inline = Path3.to_continuous ~closed:true inline
  and cont_outline = Path3.to_continuous ~closed:true outline in
  let loc, u, relocate =
    let closest = Path3.continuous_closest_point ~closed:true ~n_steps:100 cont_inline in
    match loc with
    | `Loc loc -> loc, closest loc, false
    | `Reloc loc -> loc, closest loc, true
    | `U u -> cont_inline u, u, true
  in
  let in_pt = cont_inline u in
  let u' = Path3.continuous_closest_point ~closed:true ~n_steps:100 cont_outline in_pt in
  let shift = half_width /. len_inline in
  let lu =
    let u = u -. shift in
    if u < 0. then 1. +. u else if u > 1. then u -. 1. else u
  in
  let step = shift *. 2. /. Float.of_int fn in
  let n = V3.(cont_outline u' -@ in_pt) in
  let move = V3.(add (n *$ bury)) in
  let ps =
    List.init (fn + 1) (fun i -> move @@ cont_inline (lu +. (Float.of_int i *. step)))
  and placement =
    if relocate
    then Point V3.((normalize (neg n) *$ (config.outer_rad +. 0.1)) +@ in_pt)
    else Point loc
  in
  make ~placement config ps

let default_wall_locs =
  Idx.
    [ Body (`W, First)
    ; Body (`N, Idx 2)
    ; Body (`N, Last)
    ; Body (`S, Idx 3)
    ; Thumb (`W, Last)
    ]

let wall_locations ~walls locs =
  let f loc =
    let+ w =
      match loc with
      | Thumb (s, i) -> Idx.to_find i Walls.(Sides.get walls.thumb s)
      | Body (s, i) -> Idx.to_find i Walls.(Sides.get walls.body s)
    in
    `Reloc
      V3.(
        mid w.foot.bot_left w.foot.bot_right
        +@ (normalize (w.foot.bot_left -@ w.foot.top_left) *$ 1.))
  in
  List.filter_map f locs

let to_scad t = t.scad

let apply t scad =
  match t.cut with
  | Some cut -> Scad.sub (Scad.add scad t.scad) cut
  | None -> Scad.add scad t.scad
