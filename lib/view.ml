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


