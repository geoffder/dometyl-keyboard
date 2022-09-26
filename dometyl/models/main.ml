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
  (* Write.thing *)
  (*   "splaytyl_right_with_plate" *)
  (*   ( Case.to_scad ~show_caps:false splaytyl_right *)
  (*   |> Scad.add (Scad.ztrans (-8.) (Bottom.make splaytyl_right)) ); *)
  Scad.to_file
    "tent_right.scad"
    Tent.(make ~style:(prison ~corner:(Path3.Round.bez (`Joint 1.)) ()) splaytyl_right)
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
