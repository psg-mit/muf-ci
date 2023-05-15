(* 
  inference strategy
  betas - approx
  sigma - approx
  sens - exact
  fpr/spec - exact
  true_pos - approx
  eta - approx
*)

(* helper functions *)
val wrap_x = fun x -> const (x)

val preprocess_data = fun entry ->
  (* pos,new_household_id,sex,"age_cat[5,10)","age_cat[10,20)","age_cat[50,65)","age_cat[65,105)",week_1,week_3,week_4,week_5 *)
  let survey_res = if const(eq_det(List.hd(entry), 1.)) then const(true) else const(false) in
  let h = int_of_float(List.hd(List.tl(entry))) in
  let x' = List.cons(1., List.tl(List.tl(entry))) in
  let x = List.map (wrap_x, x') in
  (x, h, survey_res)

val dot = fun (acc, (x, b)) -> add(acc, mult(x, b))
val sigmoid = fun x -> div(const (1.), add(const (1.), expp(subtract((const (0.)), x))))

(* model *)
val init_eta = fun i ->
  let eta <- gaussian(const(0.), const(1.)) in
  eta 

val make_observations = fun ((b, eta, sigma_h, sens, fpr), (x, h, survey_result)) ->
  let eta_h = Array.get(eta, h) in
  let p' = add(
    List.fold (dot, const(0.), List.zip(x, b)),
    mult(sigma_h, eta_h)
    ) in
  let p = sigmoid(p') in
  
  let approx true_pos <- bernoulli(p) in
  let pos_prob = if true_pos then sens else fpr in
  
  let () = observe(bernoulli(pos_prob), eval(survey_result)) in
  (b, eta, sigma_h, sens, fpr)

(* parameters *)
let hh = 728 in
let n_survey = 1000 in
let n_pos_control = 181 in
let n_neg_control = 176 in
  
(* observations *)
let control_tp_result = 154 in
let control_fp_result = 0 in

let data = List.map(preprocess_data, read ("data/processed_data.csv")) in

let exact sens <- beta (1., 1.) in
let exact fpr <- beta (1., 1.) in
let sigma <- gaussian(const (0.), const (1.)) in
(* Half-gaussian *)
let sigma_h = if lt(const(0.), sigma) then sigma else subtract(const(0.), sigma) in

let eta = Array.init(hh, init_eta) in

(* b coefficients *)
(* sex = 0 is female, 1 is male *)
(* age_cat[20, 50) is encoded by all 0s; likewise for week2 *)
let intercept <- gaussian (const (0.), const (1.)) in
let sex <- gaussian (const (0.), const (1.)) in
let age_cat_5_10 <- gaussian (const (0.), const (1.)) in
let age_cat_10_20 <- gaussian (const (0.), const (1.)) in
let age_cat_50_65 <- gaussian (const (0.), const (1.)) in
let age_cat_65_105 <- gaussian (const (0.), const (1.)) in
let week1 <- gaussian (const (0.), const (1.)) in
let week3 <- gaussian (const (0.), const (1.)) in
let week4 <- gaussian (const (0.), const (1.)) in
let week5 <- gaussian (const (0.), const (1.)) in
let b = List.cons(intercept, List.cons(sex, List.cons(age_cat_5_10, List.cons(age_cat_10_20, 
            List.cons(age_cat_50_65, List.cons(age_cat_65_105, List.cons(week1, List.cons(week3, List.cons(week4, List.cons(week5, List.nil)))))))))) in

let _ = List.fold(make_observations, (b, eta, sigma_h, sens, fpr), data) in
let () = observe(binomial(n_pos_control, sens), control_tp_result) in
let () = observe(binomial(n_neg_control, fpr), control_fp_result) in

(* anything after fold is done after the particle filter is done *)
let spec = subtract(const (1.), fpr) in

lst(
List.cons (intercept,
List.cons (sex,
List.cons (age_cat_5_10,
List.cons (age_cat_10_20,
List.cons (age_cat_50_65,
List.cons (age_cat_65_105,
List.cons (week1,
List.cons (week3,
List.cons (week4,
List.cons (week5,
List.cons (const(eval(sigma_h)), (* SSI marginals can't be comparisons *)
List.cons (sens, 
List.cons (spec, List.nil))))))))))))))