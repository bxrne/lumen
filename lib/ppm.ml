(** PPM Image format **)

open Vec

(* Simple representation using 3D vectors for colors *)
type color = vec
type image = { width: int; height: int; pixels: color array array }

(* Create a new image with given width and height, initialized to black *)
let create width height =
  let pixels = Array.init height (fun _ -> Array.init width (fun _ -> { x = 0.0; y = 0.0; z = 0.0 })) in
  { width; height; pixels }

(* Edit the pixel at (x, y) to the given color *)
let edit_px img x y color =
  if x >= 0 && x < img.width && y >= 0 && y < img.height then
    img.pixels.(y).(x) <- color 
  else
    failwith "Pixel coordinates out of bounds"

(* Convert a float in [0.0, 1.0] to an int in [0, 255] *)
let to_255 x =
  int_of_float (max 0.0 (min 1.0 x) *. 255.0)

(* Save the image to a PPM file *)
let save filename img =
  let oc = open_out filename in
  Printf.fprintf oc "P3\n%d %d\n255\n" img.width img.height;
  for y = 0 to img.height - 1 do
    for x = 0 to img.width - 1 do
      let c = img.pixels.(y).(x) in
      Printf.fprintf oc "%d %d %d "
        (to_255 c.x)
        (to_255 c.y)
        (to_255 c.z)
    done;
    Printf.fprintf oc "\n"
  done;
  close_out oc

