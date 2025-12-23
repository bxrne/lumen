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

