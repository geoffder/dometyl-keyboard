open Scad_ml

module type Rotatable = sig
  type t

  val rotate : Core.rotate_t -> t -> t
  val rotate_about_pt : Core.rotate_t -> Core.pos_t -> t -> t
end

module type Transformable = sig
  type t

  val translate : Core.pos_t -> t -> t

  include Rotatable with type t := t
end
