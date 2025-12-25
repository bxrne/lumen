(** Hittable objects and hit detection *)

open Ray
open Vec

(* Information about a hit point *)
type hit_record = {
  point: vec;        (* Point of intersection *)
  normal: vec;       (* Normal at intersection point *)
  t: float;          (* Parameter t along the ray *)
  front_face: bool;  (* Whether ray hit the front face *)
}

(* Types of hittable objects *)
type hittable =
  | Sphere of Shapes.sphere
  | Plane of Shapes.plane

type hittable_list = hittable list

(* Determine if the ray hit the front face *)
let set_face_normal r outward_normal =
  let front_face = dot r.direction outward_normal < 0.0 in
  let normal = if front_face then outward_normal else scale outward_normal (-1.0) in
  (front_face, normal)

(* Hit detection for a single hittable object *)
let hit (hittable: hittable) (r: ray) (t_min: float) (t_max: float) : hit_record option =
  match hittable with
  | Sphere sphere ->
      let t = Shapes.hit_sphere sphere r in
      if t >= t_min && t <= t_max then
        let point = Ray.at r t in
        let outward_normal = Shapes.sphere_normal sphere point in
        let (front_face, normal) = set_face_normal r outward_normal in
        Some { point; normal; t; front_face }
      else
        None

  | Plane plane ->
      let t = Shapes.hit_plane plane r in
      if t >= t_min && t <= t_max then
        let point = Ray.at r t in
        let outward_normal = Shapes.plane_normal plane point in
        let (front_face, normal) = set_face_normal r outward_normal in
        Some { point; normal; t; front_face }
      else
        None

(* Hit detection for a list of hittable objects *)
let hit_list (hittables: hittable_list) (r: ray) (t_min: float) (t_max: float) : hit_record option =
  let rec find_closest = function
    | [] -> None
    | hittable :: rest ->
        match hit hittable r t_min t_max with
        | None -> find_closest rest
        | Some hit_rec ->
            match find_closest rest with
            | None -> Some hit_rec
            | Some closest_hit ->
                if hit_rec.t < closest_hit.t then
                  Some hit_rec
                else
                  Some closest_hit
  in
  find_closest hittables