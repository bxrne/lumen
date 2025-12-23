open Lumen 

 let () =  
  let img = Ppm.create 100 100 in 
  (* Quadrants 1=green, 2=red, 3=blue, 4=white *)
  for y = 0 to 99 do 
    for x = 0 to 99 do 
      let color = 
        if x < 50 && y < 50 then Vec.create 0.0 1.0 0.0
        else if x >= 50 && y < 50 then Vec.create 1.0 0.0 0.0
        else if x < 50 && y >= 50 then Vec.create 0.0 0.0 1.0
        else Vec.create 1.0 1.0 1.0
      in
      Ppm.edit_px img x y color
    done
  done;
  Ppm.save "output.ppm" img

