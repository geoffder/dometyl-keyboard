open! Base
open! Scad_ml
open Dometyl

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
  write_thing ~export:false "case" case.scad;
  (* write_thing
   *   ~export:false
   *   "case"
   *   (Model.union [ case.scad; Bottom.make case |> Model.translate (0., 0., -10.) ]); *)
  (* write_thing "niz_bottom" Niz.Bottom.scad; *)
  write_thing "tent" (Tent.make case);
  (* write_thing "niz_platform" Niz.Platform.(make default_config).scad; *)
  (* write_thing "niz_cross_section" Niz.example_cross_section; *)
  (* write_thing "niz_sensor" Sensor.(make Config.a3144).scad; *)
  (* write_thing "hotswap_ex" Hotswap.combo_ex; *)
  Stdio.print_endline "Done!"
