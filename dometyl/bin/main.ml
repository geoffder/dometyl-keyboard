open! Base
open Dometyl

let write_thing ?(export = false) name scad =
  let filename ext = Printf.sprintf "../things/%s.%s" name ext in
  let oc = Stdio.Out_channel.create (filename "scad") in
  Scad_ml.Core.write oc scad;
  if export
  then (
    try
      Printf.sprintf
        ( if Sys.unix
        then "openscad -q -o %s -D 'quality=\"production\"' %s"
        else "openscad.com -q -o %s -D 'quality=\"production\"' %s" )
        (filename "stl")
        (filename "scad")
      |> Caml.Sys.command
      |> function
      | 0 -> ()
      | _ -> failwith ""
    with
    | _ -> Stdio.print_endline "Openscad export shell command failed." )

let () =
  Stdio.print_endline "Building (and maybe exporting) scads...";
  write_thing "keyhole" Case.keyhole.scad;
  write_thing "column" Case.column.scad;
  write_thing "thumb" Case.thumb.scad;
  write_thing ~export:true "plate" Case.Plate.t.scad;
  write_thing "niz_bottom" Niz.Bottom.scad;
  write_thing "niz_platform" Case.niz_platform.scad;
  write_thing "niz_cross_section" Case.niz_cross_section;
  write_thing "niz_sensor" Case.niz_sensor.scad;
  Stdio.print_endline "Done!"
