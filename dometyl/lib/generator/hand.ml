open! Base
open! Scad_ml

module Bone = struct
  type t =
    { scad : Scad.t
    ; base : Vec3.t
    ; tip : Vec3.t
    ; joint : Vec3.t
    ; normal : Vec3.t
    }

  let translate p t =
    { t with
      scad = Model.translate p t.scad
    ; base = Vec3.add p t.base
    ; tip = Vec3.add p t.tip
    }

  let rotate r t =
    { scad = Model.rotate r t.scad
    ; base = Vec3.rotate r t.base
    ; tip = Vec3.rotate r t.tip
    ; joint = Vec3.rotate r t.joint
    ; normal = Vec3.rotate r t.normal
    }

  let rotate_about_pt r p t =
    { scad = Model.rotate_about_pt r p t.scad
    ; base = Vec3.rotate_about_pt r p t.base
    ; tip = Vec3.rotate_about_pt r p t.tip
    ; joint = Vec3.rotate r t.joint
    ; normal = Vec3.rotate r t.normal
    }

  let quaternion q t =
    { scad = Model.quaternion q t.scad
    ; base = Quaternion.rotate_vec3 q t.base
    ; tip = Quaternion.rotate_vec3 q t.tip
    ; joint = Quaternion.rotate_vec3 q t.joint
    ; normal = Quaternion.rotate_vec3 q t.normal
    }

  let quaternion_about_pt q p t =
    { scad = Model.quaternion_about_pt q p t.scad
    ; base = Quaternion.rotate_vec3_about_pt q p t.base
    ; tip = Quaternion.rotate_vec3_about_pt q p t.tip
    ; joint = Quaternion.rotate_vec3 q t.joint
    ; normal = Quaternion.rotate_vec3 q t.normal
    }

  let bend a t =
    let q = Quaternion.make t.joint a
    and p = Vec3.negate t.base in
    { t with
      scad = Model.quaternion_about_pt q p t.scad
    ; tip = Quaternion.rotate_vec3_about_pt q p t.tip
    ; normal = Quaternion.rotate_vec3 q t.normal
    }

  let splay a t =
    let q = Quaternion.make t.normal a
    and p = Vec3.negate t.base in
    { t with
      scad = Model.quaternion_about_pt q p t.scad
    ; tip = Quaternion.rotate_vec3_about_pt q p t.tip
    ; joint = Quaternion.rotate_vec3 q t.joint
    }

  let make ?(fn = 5) ?alpha ?colour ?(angle = 0.) ~rad len =
    let scad =
      Model.cylinder ~fn rad len
      |> Option.value_map ~default:Fn.id ~f:(Model.color ?alpha) colour
    in
    { scad
    ; base = 0., 0., 0.
    ; tip = 0., 0., len
    ; joint = 1., 0., 0.
    ; normal = 0., -1., 0.
    }
    |> bend (Float.pi /. -2.)
    |> splay angle

  let to_scad t = t.scad
end

module Finger = struct
  type t =
    { prox : Bone.t
    ; mid : Bone.t
    ; dist : Bone.t
    }

  let map ~f t = { prox = f t.prox; mid = f t.mid; dist = f t.dist }
  let translate p = map ~f:(Bone.translate p)
  let rotate r = map ~f:(Bone.rotate r)
  let rotate_about_pt r p = map ~f:(Bone.rotate_about_pt r p)
  let quaternion q = map ~f:(Bone.quaternion q)
  let quaternion_about_pt q p = map ~f:(Bone.quaternion_about_pt q p)

  let splay a t =
    quaternion_about_pt (Quaternion.make t.prox.normal a) (Vec3.negate t.prox.base) t

  let extend ?mult a t =
    let p, m, d = Option.value ~default:(1., 1., 1.) mult in
    let t' =
      quaternion_about_pt
        (Quaternion.make t.prox.joint (a *. p))
        (Vec3.negate t.prox.base)
        t
    in
    let mid_bend =
      Bone.quaternion_about_pt
        (Quaternion.make t'.mid.joint (a *. m))
        (Vec3.negate t'.mid.base)
    in
    { prox = t'.prox
    ; mid = mid_bend t'.mid
    ; dist = Bone.bend (a *. d) (mid_bend t'.dist)
    }

  let flex ?mult a = extend ?mult (-.a)

  let make ?splay ?(offset = Vec3.zero) ?rads (lp, lm, ld) =
    let rp, rm, rd = Option.value ~default:(6., 5., 4.) rads in
    let prox = Bone.make ?angle:splay ~rad:rp lp
    and mid = Bone.make ?angle:splay ~rad:rm lm
    and dist = Bone.make ?angle:splay ~rad:rd ld in
    { prox
    ; mid = Bone.translate prox.tip mid
    ; dist = Bone.translate (Vec3.add prox.tip mid.tip) dist
    }
    |> translate offset

  let to_scad { prox; mid; dist } =
    Model.union [ Bone.to_scad prox; Bone.to_scad mid; Bone.to_scad dist ]
end

(* TODO: remove thumb, and place in own type with duct axis? Don't want to flex it with
the other fingers really most of the time, diminishing the usefullness of the helpers. *)
module Fingers = struct
  type t =
    { index : Finger.t
    ; middle : Finger.t
    ; ring : Finger.t
    ; pinky : Finger.t
    }

  let map ~f t =
    { index = f t.index; middle = f t.middle; ring = f t.ring; pinky = f t.pinky }

  let fold ~f ~init { index; middle; ring; pinky } =
    let flipped = Fn.flip f in
    f init index |> flipped middle |> flipped ring |> flipped pinky

  let translate p = map ~f:(Finger.translate p)
  let rotate r = map ~f:(Finger.rotate r)
  let rotate_about_pt r p = map ~f:(Finger.rotate_about_pt r p)
  let quaternion q = map ~f:(Finger.quaternion q)
  let quaternion_about_pt q p = map ~f:(Finger.quaternion_about_pt q p)
  let extend ?mult a = map ~f:(Finger.extend ?mult a)
  let flex ?mult a = map ~f:(Finger.flex ?mult a)
  let to_scad t = Model.union @@ (fold ~init:[] ~f:(fun l a -> Finger.to_scad a :: l)) t
end

module Thumb = struct
  type t =
    { bones : Finger.t
    ; duct : Vec3.t
    }

  let make ?splay ?offset ?(rads = 9., 8., 7.) lengths =
    let bones = Finger.make ?splay ?offset ~rads lengths in
    { bones =
        Finger.quaternion_about_pt
          (Quaternion.make (Vec3.sub bones.prox.base bones.prox.tip) (Float.pi /. 2.))
          (Vec3.negate bones.prox.base)
          bones
    ; duct = 0., 1., 0.
    }

  let translate p t = { t with bones = Finger.translate p t.bones }
  let rotate r t = { bones = Finger.rotate r t.bones; duct = Vec3.rotate r t.duct }

  let rotate_about_pt r p t =
    { bones = Finger.rotate_about_pt r p t.bones; duct = Vec3.rotate r t.duct }

  let quaternion q t =
    { bones = Finger.quaternion q t.bones; duct = Quaternion.rotate_vec3 q t.duct }

  let quaternion_about_pt q p t =
    { bones = Finger.quaternion_about_pt q p t.bones
    ; duct = Quaternion.rotate_vec3 q t.duct
    }

  let extend ?mult a t = { t with bones = Finger.extend ?mult a t.bones }
  let flex ?mult a = extend ?mult (-.a)
  let splay a t = { t with bones = Finger.splay a t.bones }

  let adduction a t =
    { t with
      bones =
        Finger.quaternion_about_pt
          (Quaternion.make t.duct a)
          (Vec3.negate t.bones.prox.base)
          t.bones
    }

  let abduction a = adduction (-.a)
  let to_scad t = Finger.to_scad t.bones
end

type t =
  { fingers : Fingers.t
  ; thumb : Thumb.t
  ; carpals : Scad.t
  ; knuckle_rad : float
  ; origin : Vec3.t
  ; wrist : Vec3.t
  ; heading : Vec3.t
  ; normal : Vec3.t
  }

let translate p t =
  { t with
    fingers = Fingers.translate p t.fingers
  ; thumb = Thumb.translate p t.thumb
  ; carpals = Model.translate p t.carpals
  ; origin = Vec3.add p t.origin
  }

let rotate r t =
  { t with
    fingers = Fingers.rotate r t.fingers
  ; thumb = Thumb.rotate r t.thumb
  ; carpals = Model.rotate r t.carpals
  ; origin = Vec3.rotate r t.origin
  ; wrist = Vec3.rotate r t.wrist
  ; heading = Vec3.rotate r t.heading
  ; normal = Vec3.rotate r t.normal
  }

let rotate_about_pt r p t =
  { t with
    fingers = Fingers.rotate_about_pt r p t.fingers
  ; thumb = Thumb.rotate_about_pt r p t.thumb
  ; carpals = Model.rotate_about_pt r p t.carpals
  ; origin = Vec3.rotate_about_pt r p t.origin
  ; wrist = Vec3.rotate r t.wrist
  ; heading = Vec3.rotate r t.heading
  ; normal = Vec3.rotate r t.normal
  }

let quaternion q t =
  { t with
    fingers = Fingers.quaternion q t.fingers
  ; thumb = Thumb.quaternion q t.thumb
  ; carpals = Model.quaternion q t.carpals
  ; origin = Quaternion.rotate_vec3 q t.origin
  ; wrist = Quaternion.rotate_vec3 q t.wrist
  ; heading = Quaternion.rotate_vec3 q t.heading
  ; normal = Quaternion.rotate_vec3 q t.normal
  }

let quaternion_about_pt q p t =
  { t with
    fingers = Fingers.quaternion_about_pt q p t.fingers
  ; thumb = Thumb.quaternion_about_pt q p t.thumb
  ; carpals = Model.quaternion_about_pt q p t.carpals
  ; origin = Quaternion.rotate_vec3_about_pt q p t.origin
  ; wrist = Quaternion.rotate_vec3 q t.wrist
  ; heading = Quaternion.rotate_vec3 q t.heading
  ; normal = Quaternion.rotate_vec3 q t.normal
  }

let flex_fingers ?mult a t = { t with fingers = Fingers.flex ?mult a t.fingers }
let extend_fingers ?mult a t = { t with fingers = Fingers.extend ?mult a t.fingers }
let flex_thumb ?mult a t = { t with thumb = Thumb.flex ?mult a t.thumb }
let extend_thumb ?mult a t = { t with thumb = Thumb.extend ?mult a t.thumb }
let extend a t = quaternion_about_pt (Quaternion.make t.wrist a) (Vec3.negate t.origin) t
let flex a = extend (-.a)
let adduction a t = { t with thumb = Thumb.adduction a t.thumb }
let abduction a = adduction (-.a)

let suppinate a t =
  quaternion_about_pt (Quaternion.make t.heading a) (Vec3.negate t.origin) t

let pronate a = suppinate (-.a)

let radial_dev a t =
  quaternion_about_pt (Quaternion.make t.normal a) (Vec3.negate t.origin) t

let ulnar_dev a = radial_dev (-.a)

(* TODO: obviously the flex_fingers start isn't super great if I need to adjust almost
   all the fingers. Should I bother with the two steps? *)
let home_curl t =
  let t' = flex_fingers ~mult:(0., 1.0, 0.5) (Float.pi /. 2.8) t in
  { t' with
    fingers =
      { index = Finger.flex ~mult:(1., 0., -1.) (Float.pi /. 60.) t'.fingers.index
      ; ring = Finger.flex ~mult:(-0.2, 1., -0.2) (Float.pi /. 16.) t'.fingers.ring
      ; middle = Finger.flex ~mult:(-0.1, 1., 0.1) (Float.pi /. 20.) t'.fingers.middle
      ; pinky = Finger.flex ~mult:(-0.5, 1., -0.2) (Float.pi /. 30.) t'.fingers.pinky
      }
  }
  |> flex_thumb ~mult:(-0.5, 1.0, 0.2) (Float.pi /. 8.)

let make ?(carpal_len = 58.) ?(knuckle_rad = 5.) (fingers : Fingers.t) (thumb : Thumb.t) =
  let carpals =
    Model.cylinder knuckle_rad carpal_len
    |> Model.rotate (0., Float.pi /. 2., 0.)
    |> Model.translate thumb.bones.prox.base
  in
  let mid_x, mid_y, _ = fingers.middle.prox.base
  and _, y, _ = thumb.bones.prox.base
  and z =
    Fingers.fold
      ~init:(Vec3.get_z thumb.bones.prox.base)
      ~f:(fun m a -> Float.min m (Vec3.get_z a.prox.base))
      fingers
  in
  let heading = Vec3.normalize (0., mid_y, -.z)
  and origin = mid_x, y, z in
  { fingers
  ; thumb
  ; carpals
  ; knuckle_rad
  ; origin
  ; wrist = 1., 0., 0.
  ; heading
  ; normal = Vec3.rotate (Float.pi /. 2., 0., 0.) heading
  }
  |> translate (Vec3.negate origin)

let to_scad { fingers; thumb; carpals; knuckle_rad; _ } =
  let palm =
    Model.hull
      [ carpals
      ; Fingers.fold
          ~init:[]
          ~f:(fun l fgr -> Model.translate fgr.prox.base (Model.sphere knuckle_rad) :: l)
          fingers
        |> Model.union
      ; thumb.bones.prox.scad
      ]
  in
  Model.union [ Fingers.to_scad fingers; Thumb.to_scad thumb; palm ]

let home ?(hover = 18.) Plate.{ columns; config; _ } t =
  let key = Columns.key_exn columns config.centre_col config.centre_row in
  let pos = Vec3.(add key.origin (mul_scalar (KeyHole.normal key) hover))
  and aligned =
    let column_vec =
      Vec3.(
        normalize
          (mul
             ( (Columns.key_exn columns config.centre_col (config.centre_row + 1)).origin
             <-> key.origin )
             (1., 1., 0.) ))
    in
    let tented = suppinate config.tent t in
    quaternion_about_pt
      (Quaternion.alignment
         Vec3.(normalize @@ tented.heading <*> (1., 1., 0.))
         column_vec )
      (Vec3.negate tented.origin)
      tented
  in
  translate (Vec3.sub pos aligned.fingers.middle.dist.tip) aligned

(* TODO: create a finger config type, and a hand config. This way this can be bundled
   up a bit better and be a bit more approachable. Also, record updating can be used to
   modify the provided default configs. *)
let default =
  let thumb = Thumb.make ~offset:(15., 0., -25.) ~splay:(Float.pi /. 8.) (52., 30., 28.8)
  and index = Finger.make ~offset:(21., 60., 0.) (47.5, 27., 21.)
  and middle = Finger.make ~offset:(41., 62., 0.) (55., 31., 27.)
  and ring = Finger.make ~offset:(55., 60., 0.) ~splay:(Float.pi /. -25.) (50., 31., 24.)
  and pinky =
    Finger.make ~offset:(70., 52., 0.) ~splay:(Float.pi /. -11.) (37.5, 26., 22.)
  in
  make Fingers.{ index; middle; ring; pinky } thumb
