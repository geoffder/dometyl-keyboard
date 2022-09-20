open! Scad_ml

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
  let base_centre = (Path2.to_continuous ps) 0.5 in
  let normal, hole_offset, _foot_offset =
    match placement with
    | Normal n -> n, V2.smul n outer_rad, V2.smul n (-0.1)
    | Point p ->
      let diff = V2.(p -@ base_centre) in
      V2.normalize diff, diff, V2.smul (V2.normalize diff) (-0.1)
  in
  let hole_centre = V2.(base_centre +@ hole_offset) in
  let l = V2.(hole_centre +@ (v2 normal.y (-.normal.x) *$ outer_rad))
  and r = V2.(hole_centre -@ (v2 normal.y (-.normal.x) *$ outer_rad)) in
  let outer = List.tl @@ Path2.arc_about_centre ~dir:`CW ~centre:hole_centre ~fn l r
  and inner = List.rev_map (V2.add hole_centre) @@ Path2.circle ~fn inner_rad in
  let outline =
    let swoop_l =
      let rad_offset = V2.(normalize (p1 -@ base_centre) *$ outer_rad) in
      List.tl @@ Bezier2.(curve ~fn (make V2.[ p1; base_centre +@ rad_offset; l ]))
    and swoop_r =
      let rad_offset = V2.(normalize (p2 -@ base_centre) *$ outer_rad) in
      List.tl @@ Bezier2.(curve ~fn (make V2.[ r; base_centre +@ rad_offset; p2 ]))
    and fudge = V2.(normal *$ -0.01) in
    List.concat [ List.tl @@ List.rev_map (V2.add fudge) ps; swoop_l; outer; swoop_r ]
  in
  let scad, cut =
    match hole with
    | Through ->
      ( Poly2.make ~holes:[ inner ] outline
        |> Poly2.to_scad
        |> Scad.extrude ~height:thickness
      , None )
    | Inset { depth = d; punch } ->
      let inset =
        let s = Scad.(ztrans (-0.01) @@ extrude ~height:(d +. 0.01) (polygon inner)) in
        match punch with
        | Some punch ->
          let rad =
            match punch with
            | `Abs r -> r
            | `Rel r -> inner_rad *. r
          in
          Scad.cylinder ~fn ~height:(thickness -. d +. 0.02) rad
          |> Scad.translate (V3.of_v2 ~z:(d -. 0.01) hole_centre)
          |> Scad.add s
        | None -> s
      and foot = Scad.extrude ~height:thickness (Scad.polygon outline) in
      Scad.difference foot [ inset ], Some inset
  in
  (* let () = *)
  (*   Path2.show_points *)
  (*     (fun i -> Scad.extrude ~height:0.1 (Scad.text ~size:0.05 (Int.to_string i))) *)
  (*     outline *)
  (*   |> Scad.ztrans (-2.) *)
  (*   |> Scad.add scad *)
  (*   |> Scad.to_file "bumpon.scad" *)
  (* in *)
  { scad; cut; centre = V3.of_v2 hole_centre; config }

let to_scad t = t.scad

let apply t scad =
  match t.cut with
  | Some cut -> Scad.sub (Scad.add scad t.scad) cut
  | None -> Scad.add scad t.scad
