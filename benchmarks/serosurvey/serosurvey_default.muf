(* 
  inference strategy
  betas - approx
  sigma - approx
  sens - approx
  fpr/spec - approx
  true_pos - exact
  eta - approx
*)

(* helper functions *)

(* [intercept; sex; age_cat_5_10; age_cat_10_20; age_cat_50_65; age_cat_65_105; week1;  week3; week4; week5; sigma_h; sens; spec] *)
val output = fun l ->
  Print.print_float_list3 (l)

val preprocess_data = fun entry ->
  (* pos,new_household_id,sex,"age_cat[5,10)","age_cat[10,20)","age_cat[50,65)","age_cat[65,105)",week_1,week_3,week_4,week_5 *)
  let survey_res = eq_det(List.hd(entry), 1.) in
  (* let h = int_of_float_det(List.hd(List.tl(entry))) in *)
  let x = List.cons(1., List.tl(entry)) in
  (x, survey_res)

val dot = fun (acc, p) -> 
  let (x, b) = split (p) in
  add(acc, mul(x, b))
  
val sigmoid = fun x -> div(1., add(1., exp(sub(1., x))))

(* model *)
(* val init_eta = fun i ->
  let eta <- gaussian(0., 1.) in
  eta  *)

val make_observations = fun (params, data) ->
  let (b, params) = split(params) in
  let (sens, fpr) = split(params) in
  let (x, survey_result) = split(data) in

  (* let eta_h = Array.get(eta, h) in *)
  let p' = List.fold (dot, List.zip(x, b), 0.) in
  let p = sigmoid(p') in
  
  let true_pos <- bernoulli(p) in
  let pos_prob = if true_pos then sens else fpr in
  
  let () = observe(bernoulli(pos_prob), survey_result) in
  (b, sens, fpr)

(* parameters *)
(* let hh = 98 in *)
let n_survey = 100 in
let n_pos_control = 181 in
let n_neg_control = 176 in
  
(* observations *)
let control_tp_result = 154 in
let control_fp_result = 0 in

let data = List.map(preprocess_data, read ("data/processed_data.csv")) in

let sens <- beta (1., 1.) in
let fpr <- beta (1., 1.) in
(* let sigma <- gaussian(0., 0.25) in *)
(* Half-gaussian *)
(* let sigma_h = if lt(0., sigma) then sigma else sub(0., sigma) in *)

(* let eta = Array.init(hh, init_eta) in *)

(* b coefficients *)
(* sex = 0 is female, 1 is male *)
(* age_cat[20, 50) is encoded by all 0s; likewise for week2 *)
let intercept <- gaussian (0., 1.) in
let sex <- gaussian (0., 1.) in
let age_cat_5_10 <- gaussian (0., 1.) in
let age_cat_10_20 <- gaussian (0., 1.) in
let age_cat_50_65 <- gaussian (0., 1.) in
let age_cat_65_105 <- gaussian (0., 1.) in
let week1 <- gaussian (0., 1.) in
let week3 <- gaussian (0., 1.) in
let week4 <- gaussian (0., 1.) in
let week5 <- gaussian (0., 1.) in
let b = [intercept; sex; age_cat_5_10; age_cat_10_20; age_cat_50_65; age_cat_65_105;
week1; week3; week4; week5] in

let _ = List.fold_resample(make_observations, data, (b, sens, fpr)) in
let () = observe(binomial(n_pos_control, sens), control_tp_result) in
let () = observe(binomial(n_neg_control, fpr), control_fp_result) in

(* anything after fold is done after the particle filter is done *)
let spec = sub(1., fpr) in

[intercept; sex; age_cat_5_10; age_cat_10_20; age_cat_50_65; age_cat_65_105; week1; week3; week4; week5; sens; spec]