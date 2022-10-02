open! Scad_ml
open! Generator
open! Boards

(* NOTE: If you aren't using hotswap holders, you can simply mirror the generated case stl,
   but if you are, you will need to make a left-hand case like so. The bottom plate and
   tenting base will of course still be reversible, so you can mirror those in your slicer
   as you would a case with plain switch holes. Though you can also make bottoms/tent for
   left cases directly here as well. *)

let splaytyl_right = Splaytyl.build ()

(* let splaytyl_left = Splaytyl.build ~right_hand:false () *)
let skeletyl_right = Skeletyl.build ()
let choc_skeletyl_right = Choc_skeletyl.build ()
let closed_right = Closed.build ()
let deractyl_right = Deractyl.build ()

let () =
  Scad.to_file "skeletyl_right.scad" (Case.to_scad ~show_caps:false @@ skeletyl_right);
  (* Scad.to_file "skeletyl_compare.scad" (Skeletyl.bastard_compare ()); *)
  Scad.to_file "splaytyl_right.scad" (Case.to_scad ~show_caps:false splaytyl_right);
  Scad.to_file "splaytyl_right_plate.scad" (Plate.to_scad splaytyl_right.plate);
  (* Scad.to_file "splaytyl_left.scad" (Case.to_scad splaytyl_left); *)
  Scad.to_file "bottom_plate_right.scad" (Bottom.make splaytyl_right);
  Scad.to_file "closed_right.scad" (Case.to_scad ~show_caps:false @@ closed_right);
  Scad.to_file "deractyl_right.scad" (Case.to_scad ~show_caps:true @@ deractyl_right);
  Scad.to_file
    "closed_bottom_plate_right.scad"
    (Bottom.make ~bump_locs:Closed.bump_locs_right closed_right);
  Scad.to_file "skeletyl_bottom_plate_right.scad" (Skeletyl.bottom skeletyl_right);
  Scad.to_file
    "choc_skeletyl_right.scad"
    (Case.to_scad ~show_caps:false @@ choc_skeletyl_right)
(* Write.thing *)
(*   "splaytyl_right_with_plate" *)
(*   ( Case.to_scad ~show_caps:false splaytyl_right *)
(*   |> Scad.add (Scad.ztrans (-8.) (Bottom.make splaytyl_right)) ); *)
(* Tent.(make splaytyl_right) *)

let k =
  Key.(
    make
      ~render:false
      { outer_w = 19.
      ; outer_h = 19.
      ; inner_w = 13.95
      ; inner_h = 13.95
      ; thickness = 4.
      ; clip = Fun.id
      ; cap_height = 6.25
      ; clearance = 3.
      ; corner = Some (Path3.Round.circ (`Cut 0.5))
      ; fn = Some 5
      })

let old_mx = Key.ztrans 10. (Mx.make_hole ())

let () =
  Scad.to_file "key_test.scad"
  @@ Scad.union
       [ k.scad
       ; old_mx.scad
       ; Points.mark k.faces.east.points
       ; Points.mark old_mx.faces.east.points
       ; Points.mark k.faces.east.bounds
         (* ; Path3.show_points *)
         (*     (fun i -> if i = 0 then Scad.sphere 1. else Scad.sphere 0.1) *)
         (*     k.faces.east.path *)
       ]

let () =
  let plate = splaytyl_right.plate in
  let points =
    let north, south =
      let f i (col : Column.t) (north, south) =
        let n = (snd @@ IMap.max_binding col.keys).faces.north.points
        and s = (snd @@ IMap.min_binding col.keys).faces.south.points in
        let north = n.top_right :: n.top_left :: north
        and south = if i > 1 then s.top_left :: s.top_right :: south else south in
        north, south
      in
      let north, south = IMap.fold f plate.body ([], []) in
      List.rev north, south
    and west =
      let f _ (key : Key.t) ps =
        key.faces.west.points.top_right :: key.faces.west.points.top_left :: ps
      in
      List.rev @@ IMap.fold f (snd @@ IMap.min_binding plate.body).keys []
    and east =
      let f _ (key : Key.t) ps =
        key.faces.east.points.top_left :: key.faces.east.points.top_right :: ps
      in
      IMap.fold f (snd @@ IMap.max_binding plate.body).keys []
    and thumb_east, thumb_west =
      let f _ (col : Column.t) (east, west) =
        let e = (snd @@ IMap.max_binding col.keys).faces.east.points
        and w = (snd @@ IMap.min_binding col.keys).faces.west.points in
        let east = e.top_right :: e.top_left :: east
        and west = w.top_left :: w.top_right :: west in
        east, west
      in
      let east, west = IMap.fold f plate.thumb ([], []) in
      List.rev east, west
    and thumb_north =
      let f i (key : Key.t) ps =
        if i < 1
        then key.faces.north.points.top_right :: key.faces.north.points.top_left :: ps
        else ps
      in
      List.rev @@ IMap.fold f (snd @@ IMap.min_binding plate.thumb).keys []
    and thumb_south =
      let f _ (key : Key.t) ps =
        key.faces.south.points.top_left :: key.faces.south.points.top_right :: ps
      in
      IMap.fold f (snd @@ IMap.min_binding plate.thumb).keys []
    in
    List.concat
      [ north; east; south; thumb_east; thumb_south; thumb_west; thumb_north; west ]
  in
  let () =
    Path3.show_points (fun _ -> Scad.(color Color.Red @@ sphere 1.)) points
    |> Scad.to_file "perimeter_points.scad"
  in
  let path =
    Bezier3.(curve ~fn:256 @@ of_path ~closed:true ~size:(`Flat (`Abs 3.)) points)
  in
  let perim_sweep =
    (* Mesh.path_extrude ~path (Poly2.circle 1.) |> Mesh.to_scad *)
    Mesh.path_extrude ~euler:true ~path (Poly2.square (v2 1. 1.)) |> Mesh.to_scad
  in
  let () = Scad.to_file "perimeter_sweep.scad" perim_sweep in
  let transforms =
    Path3.to_transforms ~mode:`Euler (List.map (fun { x; y; z = _ } -> v3 x y 0.) path)
  in
  let transforms =
    let f p t = Affine3.(t %> scale (v3 1. 1. (p.z +. 1.))) in
    List.map2 f path transforms
  in
  Mesh.sweep ~transforms Poly2.(ytrans 0.5 @@ square ~center:true (v2 4. 1.))
  |> Mesh.to_scad
  |> Scad.add (Scad.color Color.Red perim_sweep)
  |> Scad.to_file "perimeter_wall.scad"
