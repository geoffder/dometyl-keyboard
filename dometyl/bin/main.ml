open Dometyl

let () =
  print_endline "Building scads...";
  Scad_ml.Util.write (open_out "keyhole.scad") Case.Key.t.scad;
  Scad_ml.Util.write (open_out "column.scad") Case.Col.t.scad;
  Scad_ml.Util.write (open_out "thumb.scad") Case.Thumb.t.scad;
  Scad_ml.Util.write (open_out "plate.scad") Case.Plate.t.scad;
  Scad_ml.Util.write (open_out "niz_bot.scad") Case.NizBot.t.scad;
  Scad_ml.Util.write (open_out "niz_bottom.scad") Niz.Bottom.scad;
  Scad_ml.Util.write (open_out "niz_platform.scad") Niz.Platform.scad;
  Scad_ml.Util.write (open_out "niz_combo.scad") Case.niz_combo;
  Scad_ml.Util.write (open_out "niz_sensor.scad") Niz.Sensor.scad;
  print_endline "Done!"
