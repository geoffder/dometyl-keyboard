let () =
  print_endline "Building scads...";
  Scad_ml.Util.write (open_out "keyhole.scad") Dometyl.Case.KeyHole.scad;
  Scad_ml.Util.write (open_out "column.scad") Dometyl.Case.Column.scad;
  print_endline "Done!"
