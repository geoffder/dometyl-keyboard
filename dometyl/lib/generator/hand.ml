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
    let q = Quaternion.make t.normal a in
    { t with
      scad = Model.quaternion_about_pt q t.base t.scad
    ; tip = Quaternion.rotate_vec3_about_pt q t.base t.tip
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
  let splay a t = quaternion_about_pt (Quaternion.make t.prox.normal a) t.prox.base t

  let bend ?mult a t =
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

  let flex ?mult a = bend ?mult (-.a)

  let make ?(oppose = false) ?splay ?(offset = Vec3.zero) ?rads (lp, lm, ld) =
    let rp, rm, rd = Option.value ~default:(6., 5., 4.) rads in
    let prox = Bone.make ?angle:splay ~rad:rp lp
    and mid = Bone.make ?angle:splay ~rad:rm lm
    and dist = Bone.make ?angle:splay ~rad:rd ld in
    let t =
      { prox
      ; mid = Bone.translate prox.tip mid
      ; dist = Bone.translate (Vec3.add prox.tip mid.tip) dist
      }
    in
    translate offset
    @@
    if oppose
    then quaternion (Quaternion.make (Vec3.sub prox.base prox.tip) (Float.pi /. 2.)) t
    else t

  let to_scad { prox; mid; dist } =
    Model.union [ Bone.to_scad prox; Bone.to_scad mid; Bone.to_scad dist ]
end

module Fingers = struct
  type t =
    { thumb : Finger.t
    ; index : Finger.t
    ; middle : Finger.t
    ; ring : Finger.t
    ; pinky : Finger.t
    }

  let map ~f t =
    { thumb = f t.thumb
    ; index = f t.index
    ; middle = f t.middle
    ; ring = f t.ring
    ; pinky = f t.pinky
    }

  let fold ~f ~init { thumb; index; middle; ring; pinky } =
    let flipped = Fn.flip f in
    f init thumb |> flipped index |> flipped middle |> flipped ring |> flipped pinky

  let translate p = map ~f:(Finger.translate p)
  let rotate r = map ~f:(Finger.rotate r)
  let rotate_about_pt r p = map ~f:(Finger.rotate_about_pt r p)
  let quaternion q = map ~f:(Finger.quaternion q)
  let quaternion_about_pt q p = map ~f:(Finger.quaternion_about_pt q p)
  let bend ?mult a = map ~f:(Finger.bend ?mult a)
  let flex ?mult a = map ~f:(Finger.flex ?mult a)
  let extend = bend
  let to_scad t = Model.union @@ (fold ~init:[] ~f:(fun l a -> Finger.to_scad a :: l)) t
end

type t =
  { fingers : Fingers.t
  ; carpals : Scad.t
  ; knuckle_rad : float
  }

let translate p t =
  { t with
    fingers = Fingers.translate p t.fingers
  ; carpals = Model.translate p t.carpals
  }

let rotate r t =
  { t with fingers = Fingers.rotate r t.fingers; carpals = Model.rotate r t.carpals }

let rotate_about_pt r p t =
  { t with
    fingers = Fingers.rotate_about_pt r p t.fingers
  ; carpals = Model.rotate_about_pt r p t.carpals
  }

let quaternion q t =
  { t with
    fingers = Fingers.quaternion q t.fingers
  ; carpals = Model.quaternion q t.carpals
  }

let quaternion_about_pt q p t =
  { t with
    fingers = Fingers.quaternion_about_pt q p t.fingers
  ; carpals = Model.quaternion_about_pt q p t.carpals
  }

let flex ?mult a t = { t with fingers = Fingers.flex ?mult a t.fingers }
let extend ?mult a t = { t with fingers = Fingers.extend ?mult a t.fingers }

let make ?(carpal_len = 54.) ?(knuckle_rad = 5.) (fingers : Fingers.t) =
  let carpals =
    Model.cylinder knuckle_rad carpal_len
    |> Model.rotate (0., Float.pi /. 2., 0.)
    |> Model.translate fingers.thumb.prox.base
  in
  let x, _, _ = fingers.middle.prox.base
  and _, y, _ = fingers.thumb.prox.base
  and z =
    Fingers.fold
      ~init:Float.max_value
      ~f:(fun m a -> Float.min m (Vec3.get_z a.prox.base))
      fingers
  in
  { fingers; carpals; knuckle_rad } |> translate (-.x, -.y, -.z)

let to_scad { fingers; carpals; knuckle_rad } =
  let palm =
    Model.hull
      [ carpals
      ; Fingers.fold
          ~init:[]
          ~f:(fun l fgr -> Model.translate fgr.prox.base (Model.sphere knuckle_rad) :: l)
          fingers
        |> Model.union
      ; fingers.thumb.prox.scad
      ]
  in
  Model.union [ Fingers.to_scad fingers; palm ]
