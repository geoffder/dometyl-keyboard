open Dometyl

let () =
  print_endline "Building scads...";
  Scad_ml.Util.write (open_out "keyhole.scad") Case.Key.t.scad;
  Scad_ml.Util.write (open_out "column.scad") Case.Col.t.scad;
  Scad_ml.Util.write (open_out "thumb.scad") Case.Thumb.t.scad;
  print_endline "Done!"
