open! Base
open! Scad_ml

module Bone = struct
  type t =
    { scad : Scad.t
    ; base : Vec3.t
    ; tip : Vec3.t
    ; joint : Vec3.t [@unit]
    ; normal : Vec3.t [@unit]
    }
  [@@deriving scad]

  let bend a t =
    let q = Quaternion.make t.joint a
    and p = Vec3.negate t.base in
    { t with
      scad = Scad.quaternion_about_pt q p t.scad
    ; tip = Vec3.quaternion_about_pt q p t.tip
    ; normal = Vec3.quaternion q t.normal
    }

  let splay a t =
    let q = Quaternion.make t.normal a
    and p = Vec3.negate t.base in
    { t with
      scad = Scad.quaternion_about_pt q p t.scad
    ; tip = Vec3.quaternion_about_pt q p t.tip
    ; joint = Vec3.quaternion q t.joint
    }

  let make ?(fn = 5) ?alpha ?colour ?(angle = 0.) ~rad len =
    let scad =
      Scad.cylinder ~fn rad len
      |> Option.value_map ~default:Fn.id ~f:(Scad.color ?alpha) colour
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
  [@@deriving scad]

  type config =
    { offset : Vec3.t option
    ; lengths : float * float * float
    ; radii : (float * float * float) option
    ; splay : float option
    }

  let config ?offset ?radii ?splay lengths = { offset; lengths; radii; splay }
  let map ~f t = { prox = f t.prox; mid = f t.mid; dist = f t.dist }

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

  let make ?splay ?(offset = Vec3.zero) ?radii (lp, lm, ld) =
    let rp, rm, rd = Option.value ~default:(6., 5., 4.) radii in
    let prox = Bone.make ?angle:splay ~rad:rp lp
    and mid = Bone.make ?angle:splay ~rad:rm lm
    and dist = Bone.make ?angle:splay ~rad:rd ld in
    { prox
    ; mid = Bone.translate prox.tip mid
    ; dist = Bone.translate (Vec3.add prox.tip mid.tip) dist
    }
    |> translate offset

  let of_config { offset; lengths; radii; splay } = make ?splay ?offset ?radii lengths

  let to_scad { prox; mid; dist } =
    Scad.union [ Bone.to_scad prox; Bone.to_scad mid; Bone.to_scad dist ]
end

module Fingers = struct
  type t =
    { index : Finger.t
    ; middle : Finger.t
    ; ring : Finger.t
    ; pinky : Finger.t
    }
  [@@deriving scad]

  let map ~f t =
    { index = f t.index; middle = f t.middle; ring = f t.ring; pinky = f t.pinky }

  let fold ~f ~init { index; middle; ring; pinky } =
    let flipped = Fn.flip f in
    f init index |> flipped middle |> flipped ring |> flipped pinky

  let update ?(index = Fn.id) ?(ring = Fn.id) ?(middle = Fn.id) ?(pinky = Fn.id) t =
    { index = index t.index
    ; middle = middle t.middle
    ; ring = ring t.ring
    ; pinky = pinky t.pinky
    }

  let extend ?mult a = map ~f:(Finger.extend ?mult a)
  let flex ?mult a = map ~f:(Finger.flex ?mult a)

  let of_configs ~index ~middle ~ring ~pinky =
    { index = Finger.of_config index
    ; middle = Finger.of_config middle
    ; ring = Finger.of_config ring
    ; pinky = Finger.of_config pinky
    }

  let to_scad t = Scad.union @@ (fold ~init:[] ~f:(fun l a -> Finger.to_scad a :: l)) t
end

module Thumb = struct
  type t =
    { bones : Finger.t
    ; duct : Vec3.t
    }
  [@@deriving scad]

  let make ?splay ?offset ?(radii = 9., 8., 7.) lengths =
    let bones = Finger.make ?splay ?offset ~radii lengths in
    { bones =
        Finger.quaternion_about_pt
          (Quaternion.make (Vec3.sub bones.prox.base bones.prox.tip) (Float.pi /. 2.))
          (Vec3.negate bones.prox.base)
          bones
    ; duct = 0., 1., 0.
    }

  let of_config Finger.{ offset; lengths; radii; splay } =
    make ?splay ?offset ?radii lengths

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

type config =
  { index : Finger.config
  ; middle : Finger.config
  ; ring : Finger.config
  ; pinky : Finger.config
  ; thumb : Finger.config
  ; carpal_len : float option
  ; knuckle_rad : float option
  }

type t =
  { fingers : Fingers.t
  ; thumb : Thumb.t
  ; carpals : Scad.t
  ; knuckle_rad : float [@scad.ignore]
  ; origin : Vec3.t
  ; wrist : Vec3.t [@scad.unit]
  ; heading : Vec3.t [@scad.unit]
  ; normal : Vec3.t [@scad.unit]
  }
[@@deriving scad]

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

let update_digits
    ?(index = Fn.id)
    ?(ring = Fn.id)
    ?(middle = Fn.id)
    ?(pinky = Fn.id)
    ?(thumb = Fn.id)
    t
  =
  { t with
    fingers = Fingers.update ~index ~ring ~middle ~pinky t.fingers
  ; thumb = { t.thumb with bones = thumb t.thumb.bones }
  }

let make ?(carpal_len = 58.) ?(knuckle_rad = 5.) (fingers : Fingers.t) (thumb : Thumb.t) =
  let carpals =
    Scad.cylinder knuckle_rad carpal_len
    |> Scad.rotate (0., Float.pi /. 2., 0.)
    |> Scad.translate thumb.bones.prox.base
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

let of_config { index; middle; ring; pinky; thumb; carpal_len; knuckle_rad } =
  let fingers = Fingers.of_configs ~index ~middle ~ring ~pinky
  and thumb = Thumb.of_config thumb in
  make ?carpal_len ?knuckle_rad fingers thumb

let to_scad
    ?(alpha = 0.5)
    ?(color = Color.Pink)
    { fingers; thumb; carpals; knuckle_rad; _ }
  =
  let palm =
    Scad.hull
      [ carpals
      ; Fingers.fold
          ~init:[]
          ~f:(fun l fgr -> Scad.translate fgr.prox.base (Scad.sphere knuckle_rad) :: l)
          fingers
        |> Scad.union
      ; thumb.bones.prox.scad
      ]
  in
  Scad.union [ Fingers.to_scad fingers; Thumb.to_scad thumb; palm ]
  |> Scad.color ~alpha color

let home ?(hover = 18.) Plate.{ columns; config; _ } t =
  let centre_row = config.row_centres config.centre_col in
  let key = Columns.key_exn columns config.centre_col centre_row in
  let pos = Vec3.(add key.origin (mul_scalar (KeyHole.normal key) hover))
  and aligned =
    let column_vec =
      Vec3.(
        normalize
          (mul
             ( (Columns.key_exn columns config.centre_col (centre_row + 1)).origin
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
  |> flex_thumb ~mult:(-0.4, 1.0, 0.2) (Float.pi /. 8.)

let default_config =
  { index = Finger.config ~offset:(21., 60., 0.) (47.5, 27., 21.)
  ; middle = Finger.config ~offset:(41., 62., 0.) (55., 31., 27.)
  ; ring = Finger.config ~offset:(55., 60., 0.) ~splay:(Float.pi /. -25.) (50., 31., 24.)
  ; pinky = Finger.config ~offset:(70., 52., 0.) ~splay:(Float.pi /. -11.) (37.5, 26., 22.)
  ; thumb = Finger.config ~offset:(15., 0., -25.) ~splay:(Float.pi /. 8.) (52., 30., 28.8)
  ; carpal_len = None
  ; knuckle_rad = None
  }

let example = of_config default_config
