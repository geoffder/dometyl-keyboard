open! Base
open! Scad_ml

type presence =
  | No
  | Yes
  | Screw

module Body : sig
  module Cols : sig
    type col =
      { north : Wall.t option
      ; south : Wall.t option
      }

    val map_col : f:(Wall.t -> Wall.t) -> col -> col

    type t = col Map.M(Int).t

    val translate : Vec3.t -> t -> t
    val rotate : Vec3.t -> t -> t
    val rotate_about_pt : Vec3.t -> Vec3.t -> t -> t

    val make
      :  ?d1:float
      -> ?d2:float
      -> ?z_off:float
      -> ?thickness:float
      -> ?clearance:float
      -> ?n_steps:[< `Flat of int | `PerZ of float > `Flat ]
      -> ?north_lookup:(int -> presence)
      -> ?south_lookup:(int -> presence)
      -> ?screw_config:Screw.config
      -> 'k Plate.t
      -> t

    val get : col -> [< `N | `S ] -> Wall.t option
    val col_to_scad : col -> Model.t
    val to_scad : t -> Model.t
    val collect_screws : ?init:Screw.t list -> t -> Screw.t list
  end

  module Sides : sig
    type t =
      { west : Wall.t Map.M(Int).t
      ; east : Wall.t Map.M(Int).t
      }

    val map : f:(Wall.t -> Wall.t) -> t -> t
    val translate : Vec3.t -> t -> t
    val rotate : Vec3.t -> t -> t
    val rotate_about_pt : Vec3.t -> Vec3.t -> t -> t

    val make
      :  ?d1:float
      -> ?d2:float
      -> ?z_off:float
      -> ?thickness:float
      -> ?clearance:float
      -> ?n_steps:[< `Flat of int | `PerZ of float > `Flat ]
      -> ?west_lookup:(int -> presence)
      -> ?east_lookup:(int -> presence)
      -> ?screw_config:Screw.config
      -> 'a Plate.t
      -> t

    val to_scad : t -> Model.t
    val collect_screws : ?init:Screw.t list -> t -> Screw.t list
  end

  type t =
    { cols : Cols.t
    ; sides : Sides.t
    }

  val translate : Vec3.t -> t -> t
  val rotate : Vec3.t -> t -> t
  val rotate_about_pt : Vec3.t -> Vec3.t -> t -> t

  val make
    :  ?d1:float
    -> ?d2:float
    -> ?z_off:float
    -> ?thickness:float
    -> ?clearance:float
    -> ?n_steps:[< `Flat of int | `PerZ of float > `Flat ]
    -> ?north_lookup:(int -> presence)
    -> ?south_lookup:(int -> presence)
    -> ?west_lookup:(int -> presence)
    -> ?east_lookup:(int -> presence)
    -> ?screw_config:Screw.config
    -> 'a Plate.t
    -> t

  val to_scad : t -> Model.t
  val collect_screws : ?init:Screw.t list -> t -> Screw.t list
end

module Thumb : sig
  type key =
    { north : Wall.t option
    ; south : Wall.t option
    }

  val map_key : f:(Wall.t -> Wall.t) -> key -> key

  type sides =
    { west : Wall.t option
    ; east : Wall.t option
    }

  val map_sides : f:(Wall.t -> Wall.t) -> sides -> sides

  type t =
    { keys : key Map.M(Int).t
    ; sides : sides
    }

  val map : f:(Wall.t -> Wall.t) -> t -> t
  val translate : Vec3.t -> t -> t
  val rotate : Vec3.t -> t -> t
  val rotate_about_pt : Vec3.t -> Vec3.t -> t -> t

  val make
    :  ?d1:float
    -> ?d2:float
    -> ?z_off:float
    -> ?thickness:float
    -> ?clearance:float
    -> ?n_steps:[ `Flat of int | `PerZ of float ]
    -> ?north_lookup:(int -> presence)
    -> ?south_lookup:(int -> presence)
    -> ?west:presence
    -> ?east:presence
    -> ?screw_config:Screw.config
    -> 'a Plate.t
    -> t

  val to_scad : t -> Model.t
  val collect_screws : ?init:Screw.t list -> t -> Screw.t list
end

type t =
  { body : Body.t
  ; thumb : Thumb.t
  }

val translate : Vec3.t -> t -> t
val rotate : Vec3.t -> t -> t
val rotate_about_pt : Vec3.t -> Vec3.t -> t -> t
val to_scad : t -> Model.t
val collect_screws : t -> Screw.t list
