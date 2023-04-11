(* 
  inference strategy
  outlier_prob - exact
  xt - exact
  is_outlier - approx
*)

val debug = fun () -> false
val true_outlier_prob = fun () -> 0.0922259583958

val se = fun (x, y) -> pow(sub_float(x, y), 2.)

val print_float_d = fun d -> 
  let () = print_float (mean_float (d)) in
  print_string (" ")

val mse = stream {
  init = (true, 1., 0., 0., 0.);
  step ((first, t, total_x_error, total_outlier_prob_error, total_error), (true_x, d)) =
    let t = if first then 1. else add_float(t, 1.) in
    let (x_d, outlier_prob_d) = split(d) in
    let estimated_x = mean_float (x_d) in
    let estimated_outlier_prob = mean_float (outlier_prob_d) in
    let x_error = se(estimated_x, true_x) in
    let outlier_prob_error = se(estimated_outlier_prob, true_outlier_prob(())) in
    let total_x_error' = add_float(total_x_error, x_error) in
    let total_outlier_prob_error' = add_float(total_outlier_prob_error, outlier_prob_error) in
    let total_error' = add_float(add_float(x_error, outlier_prob_error), total_error) in
    let x_mse = div_float(total_x_error', t) in
    let outlier_prob_mse = div_float(total_outlier_prob_error', t) in
    let total_mse = div_float(total_error', t) in
    ((total_mse, x_mse, outlier_prob_mse), (false, t, total_error', total_x_error', total_outlier_prob_error'))
}

val done_ = fun (total_mse, xt_mse, outlier_prob_mse) ->
  let () = print_string ("total_mse:") in
  let () = print_float(total_mse) in
  let () = print_string (" xt_mse:") in
  let () = print_float(xt_mse) in
  let () = print_string (" outlier_prob_mse:") in
  let () = print_float(outlier_prob_mse) in
  let () = print_newline(()) in
  let () = if debug(()) then pp_approx_status (true) else () in
  let () = exit(0) in
  ()

val outlier = stream {
  init = (true, const (0.), const (0.));
  step ((first, xt, outlier_prob), yobs) =
    let xt = sample ("xt", gaussian (if first then (const (0.), const(2500.)) else (xt, const(1.)))) in
    let outlier_prob = if first then sample ("outlier_prob", beta (100., 1000.)) else outlier_prob in
    let is_outlier = approx(sample ("is_outlier", bernoulli (outlier_prob))) in
    let () = observe (gaussian (
        ite(is_outlier, const (0.), xt), 
        ite(is_outlier, const(10000.), const(1.))
      ), yobs) in
    (pair(xt, outlier_prob), (false, xt, outlier_prob))
}

val data = stream {
  init = (true, List.nil);
  step ((first, data), ()) = 
    let data = if first then read ("data/processed_data.csv") else data in
    let is_done = eq_det(List.length(data), 1) in
    let true_x = List.hd(List.hd(data)) in 
    let obs = List.hd(List.tl(List.hd(data))) in
    let data' = List.tl(data) in
    ((is_done, true_x, obs), (false, data'))
}

val main = stream {
  init = (init(data), init(mse), infer(10, outlier));
  step ((data, mse, outlier), ()) = 
    let (obs_data, data') = unfold (data, ()) in
    let (is_done, true_x, obs) = obs_data in
    let (d, outlier') = unfold (outlier, obs) in
    let (cur_mse, mse') = unfold (mse, (true_x, d)) in
    let () = if is_done then done_(cur_mse) else () in
    ((), (data', mse', outlier'))
}