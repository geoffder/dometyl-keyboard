open Scad_ml

module type Transformable = sig
  type t

  val translate : V3.t -> t -> t
  val mirror : V3.t -> t -> t
  val rotate : V3.t -> t -> t
  val rotate_about_pt : V3.t -> V3.t -> t -> t
end

module type Transformable' = sig
  type 'a t

  val translate : V3.t -> 'a t -> 'a t
  val mirror : V3.t -> 'a t -> 'a t
  val rotate : V3.t -> 'a t -> 'a t
  val rotate_about_pt : V3.t -> V3.t -> 'a t -> 'a t
end
