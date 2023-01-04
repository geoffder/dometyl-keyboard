open! OCADml
open OSCADml
open! Dometyl
open! Examples

(* It is recommended to use the "<include> trick" for models produced by dometyl
    for OpenSCAD editor related performance reasons. This will generate an
    additional file prefixed with "incl_" that contains the actual model, while
    the script with the given base name simply <include>'s it. *)
let to_file = Scad.to_file ~incl:true

(* NOTE: If you aren't using hotswap holders, you can simply mirror the generated case stl,
   but if you are, you will need to make a left-hand case like so. The bottom plate and
   tenting base will of course still be reversible, so you can mirror those in your slicer
   as you would a case with plain switch holes. Though you can also make bottoms/tent for
   left cases directly here as well. *)

let splaytyl_right = Splaytyl.build ~hotswap:`South ()
let splaytyl_left = Splaytyl.build ~right_hand:false ~hotswap:`South ()

let () =
  to_file "splaytyl_right" (Case.to_scad ~show_caps:false splaytyl_right);
  to_file "splaytyl_left" (Case.to_scad splaytyl_left);
  to_file "bottom_plate_right" (Bottom.make splaytyl_right);
  to_file "tent_right" (Tent.make splaytyl_right)
