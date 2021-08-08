open! Base
open! Scad_ml

(* TODO:
   - add a square inset around the jack to thin the wall. Make sure jack goes
   through.
   - unfortunately this is going to mean either taking a Wall/Walls.t, or a
   thickness and a normal value. So will just go with the coupling I guess...
   - so if I take walls, along with a length of jack "cuff" then I can make the
   neccesary inset into the wall at the correct angle to make sure that the jack
   and usb are flush and sufficiently through respectively. Due to the tilt parameters
   etc for the inner index, this wall is not actually "flat" and evenly angled, so the
   inset thinning will have to accomplish this somehow
*)
let make
    ?(jack_radius = 2.65)
    ?(usb_height = 3.6)
    ?(usb_width = 9.5)
    ?(dist = 15.5)
    ?(x_off = 4.)
    ?(z_off = 6.)
    origin
  =
  let jack =
    Model.cylinder ~center:true ~fn:16 jack_radius 20.
    |> Model.rotate (Float.pi /. 2., 0., 0.)
  and usb =
    let rad = usb_height /. 2. in
    let cyl =
      Model.cylinder ~center:true ~fn:16 rad 20. |> Model.rotate (Float.pi /. 2., 0., 0.)
    in
    Model.hull
      [ Model.translate ((usb_width /. 2.) -. rad, 0., 0.) cyl
      ; Model.translate ((usb_width /. -2.) +. rad, 0., 0.) cyl
      ]
    |> Model.translate (dist, 0., 0.)
  in
  Model.union [ jack; usb ] |> Model.translate (Vec3.add origin (x_off, 0., z_off))
