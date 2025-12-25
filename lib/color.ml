(** Color calculation for ray tracing *)

open Vec
open Hittable

(* Calculate color for a ray based on what it hits *)
let calculate_ray_color 
    ~(hit_record: hit_record option) 
    ~(ray_dir: vec) : vec =
  match hit_record with
  | None ->
      (* Sky gradient for background *)
      let t = 0.5 *. (ray_dir.y +. 1.0) in
      let c1 = Vec.scale (Vec.create 1.0 1.0 1.0) (1.0 -. t) in
      let c2 = Vec.scale (Vec.create 0.5 0.7 1.0) t in
      Vec.add c1 c2
      
  | Some hit ->
      (* Map normal to RGB color *)
      let normal = hit.normal in
      (* Map normal components from [-1,1] to [0,1] and scale by 0.5 *)
      Vec.scale (Vec.create (normal.x +. 1.0) (normal.y +. 1.0) (normal.z +. 1.0)) 0.5