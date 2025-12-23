open Vec 

type viewport = {
  width : float;
  height : float;
  focal_length : float;
  upper_left : vec;
  u : vec;          (* horizontal axis *)
  v : vec;          (* vertical axis *)
  du : vec;         (* pixel delta in u *)
  dv : vec;         (* pixel delta in v *)
}

type image = {
  width : int;
  height : int;
}

let make_viewport ~image ~focal_length =
  let aspect_ratio =
    float_of_int image.width /. float_of_int image.height
  in

  let viewport_height = 2.0 in
  let viewport_width = aspect_ratio *. viewport_height in

  let camera_center = create 0.0 0.0 0.0 in

  (* Viewport axes: u is horizontal (right), v is vertical (down) *)
  let u = create viewport_width 0.0 0.0 in
  let v = create 0.0 viewport_height 0.0 in

  (* Upper left corner: subtract half width, half height, and move forward by focal_length *)
  let upper_left =
    camera_center
    |> add (Vec.create (-.viewport_width /. 2.0) (-.viewport_height /. 2.0) (-.focal_length))
  in

  let du = scale u (1.0 /. float_of_int image.width) in
  let dv = scale v (1.0 /. float_of_int image.height) in

  {
    width = viewport_width;
    height = viewport_height;
    focal_length;
    upper_left;
    u;
    v;
    du;
    dv;
  }


