open Lumen 

let gradient x y =
  let r = (x * 255) / 99 in
  let g = (y * 255) / 99 in
  let b = 128 in
  { Ppm.r = r; g = g; b = b }

let () = 
  let img = Ppm.create_image 100 100 in 
  for y = 0 to img.height - 1 do 
    for x = 0 to img.width - 1 do 
      let color = gradient x y in
      Ppm.set_pixel img x y color
    done
  done;
  Ppm.write_ppm "output.ppm" img

