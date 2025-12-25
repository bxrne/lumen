open Ray 
open Vec

(* Sphere *)
type sphere = {
  center: vec;
  radius: float;
} 

let create_sphere ~center ~radius = {
  center;
  radius;
}

(* Plane *)
type plane = {
  point: vec;     (* Point on the plane *)
  normal: vec;    (* Normal vector (should be normalized) *)
}

let create_plane ~point ~normal = {
  point;
  normal = normalize normal;
}

(* If roots exist, the ray hits the sphere *)
let hit_sphere (s: sphere) (r: ray) : float =
  let oc = sub r.origin s.center in
  let a = dot r.direction r.direction in
  let b = 2.0 *. dot oc r.direction in
  let c = dot oc oc -. s.radius *. s.radius in
  let discriminant = b *. b -. 4.0 *. a *. c in
  if discriminant < 0.0 then -1.0 else (-.b -. sqrt discriminant) /. (2.0 *. a)

(* Compute normal at point p on the sphere's surface *)
let sphere_normal (s: sphere) (p: vec) : vec =
  normalize (Vec.sub p s.center)

(* Hit detection for plane *)
let hit_plane (pl: plane) (r: ray) : float =
  let denom = dot pl.normal r.direction in
  (* Ray is parallel to plane or pointing away *)
  if abs_float denom < 1e-6 then -1.0
  else
    let t = dot (sub pl.point r.origin) pl.normal /. denom in
    if t > 0.0 then t else -1.0

(* Get normal for plane (just return the stored normal) *)
let plane_normal (pl: plane) (_p: vec) : vec =
  pl.normal

