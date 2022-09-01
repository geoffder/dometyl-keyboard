open! Scad_ml
open! Generator
open! Boards

(* NOTE: If you aren't using hotswap holders, you can simply mirror the generated case stl,
   but if you are, you will need to make a left-hand case like so. The bottom plate and
   tenting base will of course still be reversible, so you can mirror those in your slicer
   as you would a case with plain switch holes. Though you can also make bottoms/tent for
   left cases directly here as well. *)

let splaytyl_right = Splaytyl.build ~hotswap:`South ()
let splaytyl_left = Splaytyl.build ~right_hand:false ~hotswap:`South ()

let () =
  print_endline "Building keyboards...";
  Write.thing "splaytyl_right" (Case.to_scad ~show_caps:false splaytyl_right);
  Write.thing "splaytyl_left" (Case.to_scad splaytyl_left);
  Write.thing "bottom_plate_right" (Bottom.make splaytyl_right);
  Write.thing "tent_right" (Tent.make splaytyl_right);
  print_endline "Done!"
