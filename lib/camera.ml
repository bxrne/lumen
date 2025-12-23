open Vec
open View

type camera = {
  center : vec;
  focal_length : float;
}

type image = {
  width : int;
  height : int;
}

let make_camera ?(center = Vec.create 0.0 0.0 0.0) ?(focal_length = 1.0) () =
  { center; focal_length }

let make_viewport camera image =
  let aspect_ratio =
    float_of_int image.width /. float_of_int image.height
  in

  let viewport_height = 2.0 in
  let viewport_width = aspect_ratio *. viewport_height in

  (* Viewport axes: u is horizontal (right), v is vertical (down) *)
  let u = create viewport_width 0.0 0.0 in
  let v = create 0.0 viewport_height 0.0 in

  (* Upper left corner: subtract half width, half height, and move forward by focal_length *)
  let upper_left =
    camera.center
    |> add (create (-.viewport_width /. 2.0) (-.viewport_height /. 2.0) (-.camera.focal_length))
  in

  let du = scale u (1.0 /. float_of_int image.width) in
  let dv = scale v (1.0 /. float_of_int image.height) in

  {
    width = viewport_width;
    height = viewport_height;
    focal_length = camera.focal_length;
    upper_left;
    u;
    v;
    du;
    dv;
  }

let ray_for_pixel viewport camera i j =
  let pixel_center =
    viewport.upper_left
    |> add (scale viewport.du (float_of_int i +. 0.5))
    |> add (scale viewport.dv (float_of_int j +. 0.5))
  in
  normalize (sub pixel_center camera.center)