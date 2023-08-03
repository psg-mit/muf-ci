(* 
  inference strategy
  outlier_prob - exact
  xt - exact
  is_outlier - approx
*)

val preprocess_data = fun entry -> List.hd(List.tl(entry)) in

val step = fun (prev, yobs) ->
  let (first, prev) = split(prev) in
  let (outlier_prob, prev) = split(prev) in
  let (prev_xt, xs) = split(prev) in

  let xt_mu = if first then 0. else prev_xt in
  let xt_var = if first then 2500. else 1. in

  let exact xt <- gaussian(xt_mu, xt_var) in
  let approx is_outlier <- bernoulli(outlier_prob) in
  let mu = if is_outlier then 0. else xt in
  let var = if is_outlier then 10000. else 1. in
  let () = observe(gaussian(mu, var), yobs) in
  
  (false, outlier_prob, xt, List.cons(xt, xs))
in

val output = fun out ->
  let (outlier_prob, xs) = split(out) in
  let () = Print.print_float (mean_float(outlier_prob)) in
  let () = Print.print_endline () in
  let () = Print.print_float_list2 (xs) in
  ()
in

(* parameters *)
let n = 100 in

(* observations *)
let data = List.map(preprocess_data, read("data/processed_data.csv")) in

let exact outlier_prob <- beta(100., 1000.) in
let (_, res) = split(List.fold_resample(step, data, (true, outlier_prob, 0., [0.]))) in
let (outlier_prob, res) = split(res) in
let (_, xs) = split(res) in
let xs = List.rev(xs) in

(* Remove first element of xs *)
let xs = List.tl(xs) in

(outlier_prob, xs)
