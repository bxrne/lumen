open Lumen
open View
open Shapes

let ray_for_pixel viewport camera_center i j =
  let pixel_center =
    viewport.upper_left
    |> Vec.add (Vec.scale viewport.du (float_of_int i +. 0.5))
    |> Vec.add (Vec.scale viewport.dv (float_of_int j +. 0.5))
  in
  Vec.normalize (Vec.sub pixel_center camera_center)

let () =
  let image =
    let aspect_ratio = 16.0 /. 9.0 in
    let width = 400 in
    let height = int_of_float (float_of_int width /. aspect_ratio) in
    { width; height }
  in

  let camera_center = Vec.create 0.0 0.0 0.0 in
  let viewport = make_viewport ~image ~focal_length:1.0 in
  
  (* Create a sphere at origin (0,0,-2) with radius 0.5 *)
  let sphere = Shapes.create_sphere ~center:(Vec.create 0.0 0.0 (-2.0)) ~radius:0.5 in

  let img = Ppm.create image.width image.height in
  let hit_count = ref 0 in



  for j = 0 to image.height - 1 do
    for i = 0 to image.width - 1 do
      let dir = ray_for_pixel viewport camera_center i j in
      let ray = Ray.create camera_center dir in

      let hit_dist = hit_sphere sphere ray in
      let hit = hit_dist >= 0.0 in
      

      
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