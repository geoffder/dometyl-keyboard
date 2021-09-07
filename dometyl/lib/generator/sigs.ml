open Scad_ml

module type Transformable = sig
  type t

  val translate : Vec3.t -> t -> t
  val mirror : Vec3.t -> t -> t
  val rotate : Vec3.t -> t -> t
  val rotate_about_pt : Vec3.t -> Vec3.t -> t -> t
end

module type Transformable' = sig
  type 'a t

  val translate : Vec3.t -> 'a t -> 'a t
  val mirror : Vec3.t -> 'a t -> 'a t
  val rotate : Vec3.t -> 'a t -> 'a t
  val rotate_about_pt : Vec3.t -> Vec3.t -> 'a t -> 'a t
end
