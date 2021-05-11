open! Base
open Dometyl

let write_thing name scad =
  let oc = Stdio.Out_channel.create (Printf.sprintf "../things/%s.scad" name) in
  Scad_ml.Util.write oc scad

let () =
  Stdio.print_endline "Building scads...";
  write_thing "keyhole" Case.Key.t.scad;
  write_thing "column" Case.Col.t.scad;
  write_thing "thumb" Case.Thumb.t.scad;
  write_thing "plate" Case.Plate.t.scad;
  write_thing "niz_bottom" Niz.Bottom.scad;
  write_thing "niz_platform" Case.NizPlatform.scad;
  write_thing "niz_cross_section" Case.niz_cross_section;
  write_thing "niz_sensor" Case.A3144Cutout.scad;
  Stdio.print_endline "Done!"
