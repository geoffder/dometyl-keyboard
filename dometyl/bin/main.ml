let () =
  print_endline "Building scads...";
  Scad_ml.Util.write (open_out "case.scad") Dometyl.Case.scad;
  print_endline "Done!"
