(* 
  inference strategy
  outlier_prob - approx
  xt - approx
  is_outlier - approx
*)

val preprocess_data = fun entry -> List.hd(List.tl(entry)) in

val step = fun (prev, yobs) ->
  let (first, prev) = split(prev) in
  let (outlier_prob, prev) = split(prev) in
  let (prev_xt, xs) = split(prev) in

  let xt_mu = if first then 0. else prev_xt in
  let xt_var = if first then 2500. else 1. in

  let approx xt <- gaussian(xt_mu, xt_var) in
  let approx is_outlier <- bernoulli(outlier_prob) in
  let mu = if is_outlier then 0. else xt in
  let var = if is_outlier then 10000. else 1. in
  let () = observe(gaussian(mu, var), yobs) in
  (* let (mu, var) = split(if is_outlier then (0., 10000.) else (x, 1.)) in *)
  (* Note: Types prevent this. Observe has to take a distribution *)
  (* let y = if is_outlier then gaussian(const (0.), const (10000.)) else gaussian(x, const (1.)) in *)
  (* let () = observe(y, yobs) in *)
  
  (false, outlier_prob, xt, List.cons(xt, xs))
in

val output = fun out ->
  let (outlier_prob, xs) = split(out) in
  (* let () = Print.print_string (pp_mdistr(outlier_prob)) in *)
  let () = Print.print_float (mean_float(outlier_prob)) in
  let () = Print.print_endline () in
  let () = Print.print_float_list2 (xs) in
  ()
in

(* parameters *)
let n = 100 in

(* observations *)
let data = List.map(preprocess_data, read("data/processed_data.csv")) in

let approx outlier_prob <- beta(100., 1000.) in
let (_, res) = split(List.fold_resample(step, data, (true, outlier_prob, 0., [0.]))) in
let (outlier_prob, res) = split(res) in
let (_, xs) = split(res) in
let xs = List.rev(xs) in

(* TODO: this is a temporary hack due to the cps ite bug. can remove later *)
(* Remove first element of xs *)
let xs = List.tl(xs) in

(outlier_prob, xs)
