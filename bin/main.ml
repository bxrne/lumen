open Lumen
open Shapes
open Camera
open Hittable
open Color

let () =
  let image =
    let aspect_ratio = 16.0 /. 9.0 in
    let width = 400 in
    let height = int_of_float (float_of_int width /. aspect_ratio) in
    { width; height }
  in

  let camera = Camera.make_camera ~focal_length:1.0 () in
  let viewport = make_viewport camera image in
  
  (* Create hittable objects *)
  let sphere = create_sphere ~center:(Vec.create 0.0 0.0 (-2.0)) ~radius:0.5 in
  let ground = create_plane ~point:(Vec.create 0.0 (-0.5) 0.0) ~normal:(Vec.create 0.0 1.0 0.0) in
  
  let world = [
    Sphere sphere;
    Plane ground;
  ] in

  let img = Ppm.create image.width image.height in
  let hit_count = ref 0 in



  for j = 0 to image.height - 1 do
    for i = 0 to image.width - 1 do
      let dir = ray_for_pixel viewport camera i j in
      let ray = Ray.create camera.center dir in

      (* Test ray against all hittable objects *)
      let hit_record = hit_list world ray 0.0 Float.infinity in
      
      (* Count hits *)
      if Option.is_some hit_record then
        hit_count := !hit_count + 1;

      (* Calculate color using new color module *)
      let color = calculate_ray_color ~hit_record ~ray_dir:dir in

      Ppm.edit_px img i j color
    done
  done;

  Printf.printf "Total hits: %d out of %d pixels (%.2f%%)\n" 
    !hit_count (image.width * image.height) 
    (100.0 *. float_of_int !hit_count /. float_of_int (image.width * image.height));

  Ppm.save "output.ppm" img;
  Printf.printf "Image saved to output.ppm\n"