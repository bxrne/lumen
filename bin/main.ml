open Lumen
open View

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

  let img = Ppm.create image.width image.height in

  for j = 0 to image.height - 1 do
    for i = 0 to image.width - 1 do
      let dir = ray_for_pixel viewport camera_center i j in
      let _ray = Ray.create camera_center dir in

      (* simple sky gradient *)
      let t = 0.5 *. (dir.y +. 1.0) in
      let c1 = Vec.scale (Vec.create 1.0 1.0 1.0) (1.0 -. t) in
      let c2 = Vec.scale (Vec.create 0.5 0.7 1.0) t in

      Ppm.edit_px img i j (Vec.add c1 c2)
    done
  done;

  Ppm.save "output.ppm" img

