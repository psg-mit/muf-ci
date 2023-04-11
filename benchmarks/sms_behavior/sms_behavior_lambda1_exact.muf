(* 
  alt inference strategy
  lambda1 - exact
  lambda2 - approx
  tau - approx
*)
val n_data = fun () -> 74
val alpha = fun () -> const(div_float (1.0, float_of_int(n_data(()))))
val true_lambda1 = fun () -> 17.749241151314294
val true_lambda2 = fun () -> 22.72325027312415
val true_tau = fun () -> 44.308125

val se = fun (x, y) -> pow(sub_float(x, y), 2.)

val print_float_d = fun d -> 
  let () = print_float (mean_float (d)) in
  print_string (" ")

val print_int_d = fun d -> 
  let () = print_float (mean_int (d)) in
  print_string (" ")

val mse = fun ((true_lambda1, true_lambda2, true_tau), d) ->
  let (lambda1_d, d1) = split(d) in
  let (lambda2_d, tau_d) = split(d1) in
  let lambda1_mse = se(mean_float(lambda1_d), true_lambda1) in
  let lambda2_mse = se(mean_float(lambda2_d), true_lambda2) in
  let tau_mse = se(mean_int(tau_d), true_tau) in
  let total_mse = add_float(add_float(lambda1_mse, lambda2_mse), tau_mse) in
  (* let () = print_string ("lambda1: ") in
  let () = print_float_d(lambda1_d) in
  let () = print_string (" lambda2: ") in
  let () = print_float_d(lambda2_d) in
  let () = print_string (" tau: ") in
  let () = print_int_d(tau_d) in *)
  let () = print_string ("total_mse:") in
  let () = print_float(total_mse) in
  let () = print_string (" lambda1_mse:") in
  let () = print_float(lambda1_mse) in
  let () = print_string (" lambda2_mse:") in
  let () = print_float(lambda2_mse) in
  let () = print_string (" tau_mse:") in
  let () = print_float(tau_mse) in
  ()

val done_ = fun d ->
  let () = mse((true_lambda1(()), true_lambda2(()), true_tau(())), d) in
  let () = print_newline(()) in
  (* let () = pp_approx_status (true) in *)
  let () = exit(0) in
  ()

val sms_behavior = stream {
  init = (true, const(0.), const(0.), const(0));
  step ((first, lambda1, lambda2, tau), (i, count)) = 
    let (lambda1, lambda2) = 
      if first then
        (sample("lambda1", exponential(alpha(()))), approx(sample("lambda2", exponential(alpha(())))))
      else
        (lambda1, lambda2)
    in
    let tau = if first then approx(sample("tau", uniform_int(0, n_data(())))) else tau in
    let lambda = ite(lt(const(i), tau), lambda1, lambda2) in

    let () = observe(poisson(lambda), count) in
    (pair (lambda1, pair (lambda2, tau)), (false, lambda1, lambda2, tau))
}

val data = stream {
  init = (true, 0, List.nil);
  step ((first, i, data), ()) = 
    let i = if first then 0 else i in
    let data = if first then read ("data/processed_data.csv") else data in
    let is_done = eq_det(i, sub_int(n_data(()), 1)) in
    let count = int_of_float (List.hd(List.hd(data))) in 
    let data' = List.tl(data) in
    ((is_done, i, count), (false, add_int(i, 1), data'))
}

val main = stream {
  init = (init(data), infer(100, sms_behavior));
  step ((data, sms_behavior), ()) = 
    let (obs_data, data') = unfold (data, ()) in
    let (is_done, i, count) = obs_data in
    let (d, sms_behavior') = unfold (sms_behavior, (i, count)) in
    (* let () = print_int (i) in
    let () = print_newline (()) in *)
    let () = if is_done then done_(d) else () in
    ((), (data', sms_behavior'))
}