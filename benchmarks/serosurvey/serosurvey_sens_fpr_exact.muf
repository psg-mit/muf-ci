(* 
  inference strategy
  betas - approx
  sigma - approx
  sens - exact
  fpr/spec - exact
  true_pos - approx
  eta - approx
*)

val hh = fun () -> 728
val n_survey = fun () -> 1000
val n_pos_control = fun () -> 181
val n_neg_control = fun () -> 176
val control_tp_result = fun () -> 154
val control_fp_result = fun () -> 0

(* sex = 0 is female, 1 is male *)
(* age_cat[20, 50) is encoded by all 0s; likewise for week2 *)
val b_names = fun () -> 
  List.cons ("intercept", 
  (List.cons ("sex",
  (List.cons ("age_cat[5,10)",
  (List.cons ("age_cat[10,20)",
  (List.cons ("age_cat[50,65)",
  (List.cons ("age_cat[65,105)",
  (List.cons ("week1",
  (List.cons ("week3",
  (List.cons ("week4",
  (List.cons ("week5",
  (List.nil))))))))))))))))))))
  
val true_b = fun () ->
  List.cons (2.162511,
  (List.cons (0.4638138,
  (List.cons (0.050155,
  (List.cons ( -0.0583128,
  (List.cons (0.6693432,
  (List.cons (0.1790346,
  (List.cons (0.2492106,
  (List.cons (-0.2200088,
  (List.cons (-0.0593874,
  (List.cons (0.3817401,
  (List.nil))))))))))))))))))))

val true_sigma = fun () -> 0.9161439
val true_sens = fun () -> 0.808969
val true_spec = fun () -> 0.9941081

val se = fun (x, y) -> pow(sub_float(x, y), 2.)

val print_pair = fun (x, y) ->
  let () = print_string (" ") in
  let () = print_string (x) in
  let () = print_string ("_mse:") in
  let () = print_float (y) in
  () 

val print_expr = fun x -> 
  let () = print_float (eval (x)) in
  print_string (" ")

val print_item = fun x -> 
  let () = print_float (x) in
  print_string (" ")

val print_dist = fun d -> 
  let () = print_float (mean_float (d)) in
  print_string (" ")

val mean = fun (acc, x) -> div_float(add_float(acc, x), 2.)

val mse = fun ((true_b, true_sigma, true_sens, true_spec), distr) ->
  let (b_d', r) = split (distr) in
  let b_d = split_list(b_d') in
  let (sigma_d, rr) = split (r) in
  let (sens_d, spec_d) = split (rr) in

  let b = List.map (mean_float, b_d) in
  let b_mse = List.map (se, List.zip(b, true_b)) in
  let total_b_mse = List.fold (mean, 0., b_mse) in

  let sigma_mse = se(mean_float(sigma_d), true_sigma) in  
  let sens_mse = se(mean_float(sens_d), true_sens) in
  let spec_mse = se(mean_float(spec_d), true_spec) in

  let total_mse = div_float(add_float(add_float(total_b_mse, sigma_mse), 
                            add_float(sens_mse, spec_mse)), 4.) in

  (* let () = print_string ("b: ") in *)
  (* let () = List.iter (print_dist, b_d) in *)
  (* let () = print_string ("sigma: ") in *)
  (* let () = print_dist (sigma_d) in *)
  (* let () = print_string ("sens: ") in *)
  (* let () = print_dist (sens_d) in *)
  (* let () = print_string (" spec: ") in *)
  (* let () = print_dist (spec_d) in *)

  (* let () = print_string (" b_mse: ") in *)
	let () = print_string ("total_mse:") in
  let () = print_float (total_mse) in
  let () = List.iter (print_pair, List.zip(b_names(()), b_mse)) in
  let () = print_string (" sigma_mse:") in
  let () = print_float (sigma_mse) in
  let () = print_string (" sens_mse:") in
  let () = print_float (sens_mse) in
  let () = print_string (" spec_mse:") in
  let () = print_float (spec_mse) in
  ()

val wrap_x = fun x -> const (x)

val extract_data = fun entry ->
  (* pos,new_household_id,sex,"age_cat[5,10)","age_cat[10,20)","age_cat[50,65)","age_cat[65,105)",week_1,week_3,week_4,week_5 *)
  let survey_res = if eq_det(List.hd(entry), 1.) then true else false in
  let h = List.hd(List.tl(entry)) in
  let x' = List.cons(1., List.tl(List.tl(entry))) in
  let x = List.map (wrap_x, x') in
  (x, h, survey_res)

val sigmoid = fun x -> div(const (1.), add(const (1.), expp(subtract((const (0.)), x))))
  
val init_b = fun name -> sample (name, gaussian(const(0.), const(1.)))
val init_eta = fun i -> sample (concat("eta_", (string_of_int (i))), gaussian(const(0.), const(1.)))
val dot = fun (acc, (x, b)) -> add(acc, mult(x, b))

val serosurvey = stream {
  init = (true, List.nil, Array.empty, const(0.), const(0.), const(0.));
  step ((first, b, eta, sigma, sens, fpr),
        (x, h, survey_result, control_tp_result, control_fp_result)) =
    let (sens, fpr) =
      if first then
        (sample ("sens", beta (1., 1.)), sample ("fpr", beta(1., 1.)))
      else
        (sens, fpr)
    in

    let b = if first then List.map(init_b, b_names (())) else b in

    let sigma = 
      if first then
        sample ("sigma_h", gaussian(const(0.), const(1.))) 
      else 
        sigma 
    in

    (* Half-gaussian *)
    let sigma_h = ite(lt(const(0.), sigma), sigma, subtract(const(0.), sigma)) in

    let eta = if first then Array.init(hh(()), init_eta) else eta in
    let eta_h = Array.get(eta, int_of_float(h)) in

    let p' = add(
      List.fold (dot, const(0.), List.zip(x, b)),
      mult(sigma_h, eta_h)
    ) in
    let p = sigmoid(p') in
    
    let true_pos = approx(sample("true_pos", bernoulli(p))) in

    let () = observe(bernoulli(ite(true_pos, sens, fpr)), survey_result) in

    let () = if first then observe(binomial(n_pos_control(()), sens), control_tp_result) else () in
    let () = if first then observe(binomial(n_neg_control(()), fpr), control_fp_result) else () in

    let spec = subtract(const (1.), fpr) in

    (pair (lst (b), pair (const(eval(sigma_h)), pair (sens, spec))), (false, b, eta, sigma, sens, fpr))
}

val data = stream {
  (* (x, h, survey_result, control_tp_result, control_fp_result)) *)
  init = (true, List.nil);
  step ((first, data), ()) =
    let data = if first then read ("data/processed_data.csv") else data in
    let is_done = eq_det (List.length(data), 1) in
    let (x, h, survey_res) = extract_data (List.hd(data)) in
    let data' = List.tl(data) in
    ((is_done, x, h, survey_res, control_tp_result(()), control_fp_result(())), (false, data'))
}

val main = stream {
  init = (init (data), infer (1, serosurvey));
  step ((data, serosurvey), ()) =
    let (obs_data, data') = unfold (data, ()) in
    let (is_done, x, h, survey_result, control_tp_result, control_fp_result) = obs_data in

    (* let () = print_string ("x: ") in
    let () = List.iter (print_expr, x) in
    let () = print_string ("h: ") in
    let () = print_int (h) in
    let () = print_string (" survey_result: ") in
    let () = if survey_result then print_string ("true") else print_string ("false") in
    let () = print_string (" control_tp_result: ") in
    let () = print_int (control_tp_result) in
    let () = print_string (" control_fp_result: ") in
    let () = print_int (control_fp_result) in
    let () = print_newline (()) in *)

    let (d, serosurvey') = unfold (serosurvey, (x, h, survey_result, control_tp_result, control_fp_result)) in

    (* let () = print_any_t (d) in *)
    (* let () = pp_approx_status (true) in *)
    let () = if is_done then 
      mse((true_b(()), true_sigma(()), true_sens(()), true_spec(())), d) else () in
    (* let () = print_newline (()) in *)
    let () = if is_done then print_newline (()) else () in
    let () = if is_done then exit(0) else () in
    (* let () = exit(0) in *)
    ((), (data', serosurvey'))
}
