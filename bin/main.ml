open Lumen
open View
open Shapes

let ray_for_pixel viewport camera_center i j =
  let pixel_center =
    viewport.upper_left
    |> Vec.add (Vec.scale viewport.du (float_of_int i +. 0.5))
    |> Vec.add (Vec.scale viewport.dv (float_of_int j +. 0.5))
  in

  pixel_center
  |> Vec.sub camera_center
  |> Vec.normalize

let () =
  let image =
    let aspect_ratio = 16.0 /. 9.0 in
    let width = 400 in
    let height = int_of_float (float_of_int width /. aspect_ratio) in
    { width; height }
  in

  let camera_center = Vec.create 0.0 0.0 0.0 in
  let viewport = make_viewport ~image ~focal_length:1.0 in
  
  (* Debug viewport *)
  Printf.printf "Viewport upper_left: (%.3f, %.3f, %.3f)\n" 
    viewport.upper_left.x viewport.upper_left.y viewport.upper_left.z;

  (* Create a sphere at origin (0,0,-2) with radius 0.5 *)
  let sphere = Shapes.create_sphere ~center:(Vec.create 0.0 0.0 (-2.0)) ~radius:0.5 in
  
  Printf.printf "Sphere center: (%.3f, %.3f, %.3f), radius: %.3f\n" 
    sphere.center.x sphere.center.y sphere.center.z sphere.radius;

  let img = Ppm.create image.width image.height in
  let hit_count = ref 0 in

  (* Test center ray specifically *)
  let center_i = image.width / 2 in
  let center_j = image.height / 2 in
  let center_dir = ray_for_pixel viewport camera_center center_i center_j in
  let center_ray = Ray.create camera_center center_dir in
  let center_hit = hit_sphere sphere center_ray in
  
  Printf.printf "Center pixel (%d, %d): ray=(%.3f,%.3f,%.3f) hit=%b\n" 
    center_i center_j center_dir.x center_dir.y center_dir.z center_hit;

  for j = 0 to image.height - 1 do
    for i = 0 to image.width - 1 do
      let dir = ray_for_pixel viewport camera_center i j in
      let ray = Ray.create camera_center dir in

      let hit = hit_sphere sphere ray in
      
      if hit then
        hit_count := !hit_count + 1;

      let color = 
        if hit then
          (* Red sphere *)
          Vec.create 1.0 0.0 0.0
        else
          (* simple sky gradient *)
          let t = 0.5 *. (dir.y +. 1.0) in
          let c1 = Vec.scale (Vec.create 1.0 1.0 1.0) (1.0 -. t) in
          let c2 = Vec.scale (Vec.create 0.5 0.7 1.0) t in
          Vec.add c1 c2
      in

      Ppm.edit_px img i j color
    done
  done;

  Printf.printf "Total hits: %d out of %d pixels (%.2f%%)\n" 
    !hit_count (image.width * image.height) 
    (100.0 *. float_of_int !hit_count /. float_of_int (image.width * image.height));

  Ppm.save "output.ppm" img;
  Printf.printf "Image saved to output.ppm\n"