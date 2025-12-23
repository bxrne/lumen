open OUnit2
open Lumen.Vec

let test_create = 
  "create" >::: [
    "zero_vector" >:: (fun _ -> 
      assert_equal {x=0.;y=0.;z=0.;} (create 0. 0. 0.));
    "positive_values" >:: (fun _ -> 
      assert_equal {x=1.;y=2.;z=3.;} (create 1. 2. 3.));
    "mixed_values" >:: (fun _ -> 
      assert_equal {x=(-1.);y=0.;z=4.;} (create (-1.) 0. 4.));
  ]

let test_add =
  "add" >::: [
    "positive_vectors" >:: (fun _ ->
      let v1 = create 1. 2. 3. in
      let v2 = create 4. 5. 6. in
      assert_equal {x=5.;y=7.;z=9.;} (add v1 v2));
    "with_zero" >:: (fun _ ->
      let v1 = create 1. 2. 3. in
      let v2 = create 0. 0. 0. in
      assert_equal v1 (add v1 v2));
    "negative_values" >:: (fun _ ->
      let v1 = create 3. 4. 5. in
      let v2 = create (-1.) (-2.) (-3.) in
      assert_equal {x=2.;y=2.;z=2.;} (add v1 v2));
  ]

let test_sub =
  "sub" >::: [
    "positive_vectors" >:: (fun _ ->
      let v1 = create 5. 7. 9. in
      let v2 = create 1. 2. 3. in
      assert_equal {x=4.;y=5.;z=6.;} (sub v1 v2));
    "from_zero" >:: (fun _ ->
      let v1 = create 0. 0. 0. in
      let v2 = create 1. 2. 3. in
      assert_equal {x=(-1.);y=(-2.);z=(-3.);} (sub v1 v2));
    "same_vector" >:: (fun _ ->
      let v1 = create 2. 4. 6. in
      let v2 = create 2. 4. 6. in
      assert_equal {x=0.;y=0.;z=0.;} (sub v1 v2));
  ]

let test_scale =
  "scale" >::: [
    "positive_multiplier" >:: (fun _ ->
      let v = create 1. 2. 3. in
      assert_equal {x=2.;y=4.;z=6.;} (scale v 2.));
    "zero_multiplier" >:: (fun _ ->
      let v = create 1. 2. 3. in
      assert_equal {x=0.;y=0.;z=0.;} (scale v 0.));
    "negative_multiplier" >:: (fun _ ->
      let v = create 1. 2. 3. in
      assert_equal {x=(-1.);y=(-2.);z=(-3.);} (scale v (-1.)));
    "fractional_multiplier" >:: (fun _ ->
      let v = create 2. 4. 6. in
      assert_equal {x=1.;y=2.;z=3.;} (scale v 0.5));
  ]

let test_dot =
  "dot" >::: [
    "same_direction" >:: (fun _ ->
      let v1 = create 1. 2. 3. in
      let v2 = create 2. 4. 6. in
      assert_equal 28. (dot v1 v2));
    "orthogonal_vectors" >:: (fun _ ->
      let v1 = create 1. 0. 0. in
      let v2 = create 0. 1. 0. in
      assert_equal 0. (dot v1 v2));
    "with_zero" >:: (fun _ ->
      let v1 = create 1. 2. 3. in
      let v2 = create 0. 0. 0. in
      assert_equal 0. (dot v1 v2));
    "negative_components" >:: (fun _ ->
      let v1 = create 1. (-2.) 3. in
      let v2 = create (-1.) 2. (-3.) in
      assert_equal (-14.) (dot v1 v2));
  ]

let test_magnitude =
  "magnitude" >::: [
    "unit_vectors" >:: (fun _ ->
      let vx = create 1. 0. 0. in
      let vy = create 0. 1. 0. in
      let vz = create 0. 0. 1. in
      assert_equal 1. (magnitude vx);
      assert_equal 1. (magnitude vy);
      assert_equal 1. (magnitude vz));
    "zero_vector" >:: (fun _ ->
      let v = create 0. 0. 0. in
      assert_equal 0. (magnitude v));
    "known_vector" >:: (fun _ ->
      let v = create 3. 4. 0. in
      assert_equal 5. (magnitude v));
    "three_dimensional" >:: (fun _ ->
      let v = create 1. 2. 2. in
      assert_equal 3. (magnitude v));
  ]

let test_normalize =
  "normalize" >::: [
    "unit_vectors" >:: (fun _ ->
      let vx = create 1. 0. 0. in
      let vy = create 0. 1. 0. in
      let vz = create 0. 0. 1. in
      assert_equal vx (normalize vx);
      assert_equal vy (normalize vy);
      assert_equal vz (normalize vz));
    "zero_vector" >:: (fun _ ->
      let v = create 0. 0. 0. in
      assert_equal v (normalize v));
    "scale_vector" >:: (fun _ ->
      let v = create 2. 0. 0. in
      let expected = create 1. 0. 0. in
      assert_equal expected (normalize v));
    "general_vector" >:: (fun _ ->
      let v = create 3. 4. 0. in
      let normalized = normalize v in
      assert_equal 1. (magnitude normalized));
    "negative_components" >:: (fun _ ->
      let v = create (-3.) (-4.) 0. in
      let normalized = normalize v in
      assert_equal 1. (magnitude normalized));
  ]

let suite = 
  "test suite for vec" >::: [
    test_create;
    test_add;
    test_sub;
    test_scale;
    test_dot;
    test_magnitude;
    test_normalize;
  ]

let () = run_test_tt_main suite