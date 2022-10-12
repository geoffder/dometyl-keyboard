open OCADml
open OSCADml

type style =
  | Solid
  | Prison of
      { n_pillars : int option
      ; width : float option
      ; tilt : float option
      ; tilt_ez : (v2 * v2) option
      ; fn : int option
      ; slices : Wall.Steps.t option
      ; phase_shift : float option
      ; corner : Path3.Round.corner option
      ; corner_fn : int option
      ; fillet_d : [ `Abs of float | `Rel of float ] option
      ; fillet_w : float option
      }

val prison
  :  ?n_pillars:int
  -> ?width:float
  -> ?tilt:float
  -> ?tilt_ez:v2 * v2
  -> ?fn:int
  -> ?slices:Wall.Steps.t
  -> ?phase_shift:float
  -> ?fillet_d:[ `Abs of float | `Rel of float ]
  -> ?fillet_w:float
  -> ?corner:Path3.Round.corner
  -> ?corner_fn:int
  -> unit
  -> style

val default_bumps : float list

val make
  :  ?degrees:float
  -> ?fastener:Eyelet.fastener
  -> ?foot_thickness:float
  -> ?foot_rad:float
  -> ?bumpon_rad:float
  -> ?bumpon_inset:float
  -> ?bump_locs:float list
  -> ?style:style
  -> Case.t
  -> Scad.d3
