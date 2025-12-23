open Lumen
open Shapes
open Camera

let () =
  let image =
    let aspect_ratio = 16.0 /. 9.0 in
    let width = 400 in
    let height = int_of_float (float_of_int width /. aspect_ratio) in
    { width; height }
  in

  let camera = Camera.make_camera ~focal_length:1.0 () in
  let viewport = make_viewport camera image in
  
  (* Create a sphere at origin (0,0,-2) with radius 0.5 *)
  let sphere = Shapes.create_sphere ~center:(Vec.create 0.0 0.0 (-2.0)) ~radius:0.5 in

  let img = Ppm.create image.width image.height in
  let hit_count = ref 0 in



  for j = 0 to image.height - 1 do
    for i = 0 to image.width - 1 do
      let dir = ray_for_pixel viewport camera i j in
      let ray = Ray.create camera.center dir in

      let hit_dist = hit_sphere sphere ray in
      let hit = hit_dist >= 0.0 in

      if hit then
        hit_count := !hit_count + 1;

      let color = 
        if hit then
          (* Calculate normal at hit point and map to RGB *)
          let hit_point = Ray.at ray hit_dist in
          let normal = Shapes.sphere_normal sphere hit_point in
          (* Map normal components from [-1,1] to [0,1] and scale by 0.5 *)
          Vec.scale (Vec.create (normal.x +. 1.0) (normal.y +. 1.0) (normal.z +. 1.0)) 0.5
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