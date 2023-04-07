
(* val n_pos_control = 181
val n_neg_control = 176
val n_survey = 152
val true_p = 0.032
val true_sens = 0.856
val true_spec = 0.998
val control_tp_result = 154
val control_fp_result = 0 *)

(* n_pos = 4
n_neg = 148 *)

val sigmoid = fun x -> div(const (1.), add(const (1.), expp(subtract((const (0.)), x))))

val se = fun (x, y) -> pow(sub_float(x, y), 2.)

val mse = fun ((true_p, true_sens, true_spec), distr) ->
  let (p_d, s) = split (distr) in
  let (sens_d, spec_d) = split (s) in
  let p = mean_float (p_d) in
  let sens = mean_float (sens_d) in
  let spec = mean_float (spec_d) in
  let p_mse = se(p, true_p) in
  let sens_mse = se(sens, true_sens) in
  let spec_mse = se(spec, true_spec) in
  let () = print_string ("p: ") in
  let () = print_float (p) in
  let () = print_string (" sens: ") in
  let () = print_float (sens) in
  let () = print_string (" spec: ") in
  let () = print_float (spec) in
  let () = print_string (" p_mse: ") in
  let () = print_float (p_mse) in
  let () = print_string (" sens_mse: ") in
  let () = print_float (sens_mse) in
  let () = print_string (" spec_mse: ") in
  let () = print_float (spec_mse) in

  (p_mse, sens_mse, spec_mse) 


val serosurvey = stream {
  init = (true, const(0.), const(0.));
  step ((first, sens, fpr), (survey_result, control_tp_result, control_fp_result)) =
    let (sens, fpr) =
      if first then
        (sample (beta (1., 1.)), sample (beta(1., 1.)))
      else
        (sens, fpr)
    in
    let p = sample(gaussian(const (0.), 1.)) in
    let p' = sigmoid(p) in
    let true_pos = add(mult(p', sens), mult(subtract(const(1.), p'), fpr)) in
    (* let true_pos = sample(bernoulli(sigmoid(p))) in *)
    (* let () = observe(bernoulli(ite(const(eval(true_pos)), sens, fpr)), survey_result) in *)
    let () = observe(bernoulli(true_pos), survey_result) in
    let n_pos_control = 181 in
    let n_neg_control = 176 in
    let () = if first then observe(binomial(n_pos_control, sens), control_tp_result) else () in
    let () = if first then observe(binomial(n_neg_control, fpr), control_fp_result) else () in
    let spec = subtract(const (1.), fpr) in
    (pair (p, pair (sens, spec)), (false, sens, fpr))
}

val survey_result = stream {
  init = (true, 0, 0);
  step ((first, n_pos, n_neg), ()) =
    let (n_pos, n_neg) = if first then (4, 148) else (n_pos, n_neg) in
    let (res, n_pos, n_neg) = 
      if lt(0, n_pos) then
        (true, sub_int(n_pos, 1), n_neg)
      else if lt(0, n_neg) then
        (false, n_pos, sub_int(n_neg, 1))
      else
        (* let () = print_string ("DONE") in *)
        exit(0)
    in
    (res, (false, n_pos, n_neg))
}

val main = stream {
  init = (init (survey_result), infer (5000, serosurvey));
  step ((survey_result, serosurvey), ()) =
    let (survey_res, survey_result') = unfold (survey_result, ()) in
    let control_tp_result = 154 in
    let control_fp_result = 0 in
    let (d, serosurvey') = unfold (serosurvey, (survey_res, control_tp_result, control_fp_result)) in
    (* let () = print_any_t (d) in *)
    let true_p = 0.032 in
    let true_sens = 0.856 in
    let true_spec = 0.998 in
    let _ = mse((true_p, true_sens, true_spec), d) in
    let () = print_newline (()) in
    ((), (survey_result', serosurvey'))
}
