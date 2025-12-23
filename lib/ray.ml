(** Ray for tracing in space **)

open Vec

(* Ray can be P(t) = A + t*B
   where A is the origin, B is the direction, and t >= 0 *)
type ray = {
  origin: vec;
  direction: vec;
}

(* Create a new ray with given origin and direction *)
let create o d =
  {origin= o; direction= d}

(* Compute color at parameter t along the ray *)
let at ray t =
  scale ray.direction t |> add ray.origin
