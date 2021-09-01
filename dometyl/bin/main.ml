open! Base
open! Scad_ml
open Generator
open Boards

let write_thing ?(export = false) name scad =
  let filename ext = Printf.sprintf "../things/%s.%s" name ext in
  Stdio.printf "- %s => scad" name;
  Stdio.(Out_channel.flush stdout);
  let oc = Stdio.Out_channel.create (filename "scad") in
  Scad_ml.Core.write oc scad;
  if export
  then (
    Stdio.printf " => stl\n";
    Stdio.(Out_channel.flush stdout);
    try
      Printf.sprintf
        ( if Sys.unix
        then "openscad -q -o %s --export-format binstl %s"
        else "openscad.com -q -o %s --export-format binstl %s" )
        (filename "stl")
        (filename "scad")
      |> Caml.Sys.command
      |> function
      | 0 -> ()
      | _ -> failwith ""
    with
    | _ -> Stdio.print_endline "Openscad export shell command failed." )
  else Stdio.print_endline ""

let case = Splaytyl.build ()

let () =
  Stdio.print_endline "Building (and maybe exporting) scads...";
  (* write_thing "mx_keyhole" (KeyHole.make Mx.hole_config).scad; *)
  (* write_thing "niz_keyhole" (KeyHole.make Niz.hole_config).scad; *)
  write_thing "case" (Case.to_scad ~show_cutouts:false ~show_caps:false case);
  write_thing "choctyl" (Case.to_scad ~show_caps:false (Choctyl.build ()));
  write_thing "deractyl" (Case.to_scad ~show_caps:false (Deractyl.ports_build ()));
  write_thing "compactyl_mimic" (Deractyl.compactyl_compare ());
  write_thing "skeletyl" (Case.to_scad (Skeletyl.build ()));
  write_thing "tester" (Case.to_scad (Tester.build ()));
  write_thing "skeletyl_hotswap" (Case.to_scad (Skeletyl.build ~hotswap:`South ()));
  write_thing "bottom_plate" (Bottom.make case);
  write_thing "deractyl_holder_ex" (Deractyl.carbonfet_ex ());
  (* write_thing
   *   "case"
   *   (Model.union [ case.scad; Bottom.make case |> Model.translate (0., 0., -10.) ]); *)
  (* write_thing "niz_bottom" Niz.Bottom.scad; *)
  write_thing "tent" (Tent.make case);
  (* write_thing "deractyl_tent" (Tent.make (Deractyl.build ())); *)
  (* write_thing "niz_platform" Niz.Platform.(make default_config).scad; *)
  (* write_thing "niz_cross_section" Niz.example_cross_section; *)
  (* write_thing "niz_sensor" Sensor.(make Config.a3144).scad; *)
  (* write_thing "hotswap_ex" (Mx.make_hole ~hotswap:`South ()).scad; *)
  Stdio.print_endline "Done!"
