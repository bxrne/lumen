(** 3D Vector & Ops **)

(* vec type: color, direction or offset *)
type vec = {
  x: float;
  y: float;
  z: float;
}

(* Create a new vector *)
let create x y z = { x; y; z }

(* Vector operations *)
let add v1 v2 = 
  { x = v1.x +. v2.x; y = v1.y +. v2.y; z = v1.z +. v2.z }

let sub v1 v2 = 
  { x = v1.x -. v2.x; y = v1.y -. v2.y; z = v1.z -. v2.z }

let scale v s = 
  { x = v.x *. s; y = v.y *. s; z = v.z *. s }

let dot v1 v2 = 
  v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z 

let magnitude v = 
  sqrt (dot v v)

let normalize v = 
  let mag = magnitude v in 
  if mag = 0.0 then v
  else scale v (1.0 /. mag)

