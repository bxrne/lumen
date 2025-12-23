(* Represent image via the PPM format *)

type color = { r: int; g: int; b: int }
type image = { width: int; height: int; pixels: color array array }

let create width height =
  let pixels = Array.init height (fun _ -> Array.init width (fun _ -> { r = 0; g = 0; b = 0 })) in
  { width; height; pixels }

let edit_px img x y color =
  if x >= 0 && x < img.width && y >= 0 && y < img.height then
    img.pixels.(y).(x) <- color 
  else
    failwith "Pixel coordinates out of bounds"

let save filename img =
  let oc = open_out filename in 
  Printf.fprintf oc "P3\n%d %d\n255\n" img.width img.height;
  for y = 0 to img.height - 1 do 
    for x = 0 to img.width - 1 do 
      let c = img.pixels.(y).(x) in
      Printf.fprintf oc "%d %d %d " c.r c.g c.b
    done;
    Printf.fprintf oc "\n"
  done;
  close_out oc 

