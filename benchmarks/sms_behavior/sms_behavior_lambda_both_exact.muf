(* 
  alt inference strategy
  lambda1 - exact
  lambda2 - exact
  tau - approx
*)
val preprocess_data = fun entry -> int_of_float_det(List.hd(entry))

val step = fun (params, count_obs) ->
  let (tau, params) = split(params) in
  let (lambda1, params) = split(params) in
  let (lambda2, i) = split(params) in

  let lambda = if lt(i, tau) then lambda1 else lambda2 in

  let () = observe(poisson(lambda), count_obs) in
  (tau, lambda1, lambda2, add_int(i, 1))

val output = fun out ->
  let (tau, out) = split(out) in
  let (lambda1, lambda2) = split(out) in
  let () = Print.print_float (mean_int(tau)) in
  let () = Print.print_endline () in
  let () = Print.print_float (mean_float(lambda1)) in
  let () = Print.print_endline () in
  let () = Print.print_float (mean_float(lambda2)) in
  ()

(* parameters *)
let n_data = 74 in
let alpha = div_float (1.0, float_of_int_det(n_data)) in

(* observations *)
let data = List.map(preprocess_data, read("data/processed_data.csv")) in

let lambda1 <- exponential(alpha) in
let lambda2 <- exponential(alpha) in
let approx tau <- uniform_int(0, n_data) in
let res = List.fold_resample(step, data, (tau, lambda1, lambda2, 1)) in
let (tau, res) = split(res) in
let (lambda1, res) = split(res) in
let (lambda2, _) = split(res) in
(tau, lambda1, lambda2)