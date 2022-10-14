open OCADml
open OSCADml
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
    (Case.to_scad ~show_caps:false @@ choc_skeletyl_right);
  Scad.to_file
    "splaytyl_right_tent.scad"
    (Tent.make ~style:(Tent.prison ()) splaytyl_right)

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
  (* TODO: try using normal to sink the points into face. There is a major
    discrepancy between the sides wthere points are sunk in (compared to
    bounds), and the ends where the points are as far out as bounds.
    Alternatively/additionally I have not done any plate perimeter extending,
    which might be important for achieving the desired effect. *)
  let add_line ?(ccw = false) acc (face : Key.Face.t) =
    let pts = face.points in
    (* if ccw *)
    (* then pts.top_left :: pts.top_right :: acc *)
    (* else pts.top_right :: pts.top_left :: acc *)
    if ccw
    then V3.(mid pts.top_left pts.bot_left :: mid pts.top_right pts.bot_right :: acc)
    else V3.(mid pts.top_right pts.bot_right :: mid pts.top_left pts.bot_left :: acc)
  in
  let points =
    let north, south =
      let f i (col : Column.t) (north, south) =
        let n = (snd @@ IMap.max_binding col.keys).faces.north
        and s = (snd @@ IMap.min_binding col.keys).faces.south in
        let north = add_line north n
        and south = if i > 1 then add_line ~ccw:true south s else south in
        north, south
      in
      let north, south = IMap.fold f plate.body ([], []) in
      List.rev north, south
    and west =
      let f _ (key : Key.t) ps = add_line ps key.faces.west in
      List.rev @@ IMap.fold f (snd @@ IMap.min_binding plate.body).keys []
    and east =
      let f _ (key : Key.t) ps = add_line ~ccw:true ps key.faces.east in
      IMap.fold f (snd @@ IMap.max_binding plate.body).keys []
    and thumb_east, thumb_west =
      let f _ (col : Column.t) (east, west) =
        let e = (snd @@ IMap.max_binding col.keys).faces.east
        and w = (snd @@ IMap.min_binding col.keys).faces.west in
        let east = add_line east e
        and west = add_line ~ccw:true west w in
        east, west
      in
      let east, west = IMap.fold f plate.thumb ([], []) in
      List.rev east, west
    and thumb_north =
      let f i (key : Key.t) ps = if i < 1 then add_line ps key.faces.north else ps in
      List.rev @@ IMap.fold f (snd @@ IMap.min_binding plate.thumb).keys []
    and thumb_south =
      let f _ (key : Key.t) ps = add_line ~ccw:true ps key.faces.south in
      IMap.fold f (snd @@ IMap.min_binding plate.thumb).keys []
    in
    List.concat
      [ north; east; south; thumb_east; thumb_south; thumb_west; thumb_north; west ]
    |> Path3.deduplicate_consecutive ~closed:true ~eq:(V3.approx ~eps:2.)
  in
  let () =
    Debug.show_path3 (fun _ -> Scad.(color Color.Red @@ sphere 1.)) points
    |> Scad.to_file "perimeter_points.scad"
  in
  let path =
    Bezier3.(
      curve ~endpoint:false ~fn:512
      @@ of_path ~closed:true ~size:(`Flat (`Rel 0.05)) points)
  in
  let perim_sweep =
    Mesh.path_extrude ~caps:`Looped ~euler:true ~path (Poly2.square (v2 1. 1.))
    |> Scad.of_mesh
  in
  let () = Scad.to_file "perimeter_sweep.scad" perim_sweep in
  let shape = Poly2.(square (v2 4. 1.)) in
  (* let shape = Poly2.(xtrans (-0.5) @@ square (v2 4. 1.)) in *)
  let transforms =
    Path3.to_transforms ~mode:`Euler (List.map (fun { x; y; z = _ } -> v3 x y 0.) path)
  in
  let transforms =
    let f p t = Affine3.(t %> scale (v3 1. 1. (p.z +. 1.))) in
    List.map2 f path transforms
    |> Util.prune_transforms ~shape:(Fun.const (Path3.of_path2 shape.outer))
    |> List.map snd
    (* let f acc p t = Affine3.(t %> scale (v3 1. 1. (p.z +. 1.))) :: acc in *)
    (* List.(rev @@ tl @@ fold_left2 f [] path transforms) *)
  in
  Mesh.sweep ~caps:`Looped ~transforms shape
  |> Scad.of_mesh (* |> Scad.add (Scad.color Color.Red perim_sweep) *)
  |> Scad.add splaytyl_right.plate.scad
  |> Scad.to_file "perimeter_wall.scad"
