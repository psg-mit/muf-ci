(* 
  default inference strategy
  lambda1 - approx
  lambda2 - approx
  tau - approx
*)
val preprocess_data = fun entry -> int_of_float_det(List.hd(entry)) in

val add_data = fun (x, y) -> add(x, float_of_int_det(y)) in

val mean = fun data ->
  let sum = List.fold(add_data, data, 0.) in
  let n = float_of_int_det(List.length(data)) in
  div(sum, n)
in

val step = fun (params, count_obs) ->
  let (tau, params) = split(params) in
  let (lambda1, params) = split(params) in
  let (lambda2, i) = split(params) in

  let lambda = if lt(i, tau) then lambda1 else lambda2 in

  let () = observe(poisson(lambda), count_obs) in
  (tau, lambda1, lambda2, int_add(i, 1))
in

val output = fun out ->
  let (tau, out) = split(out) in
  let (lambda1, lambda2) = split(out) in
  let () = Print.print_float (mean_int(tau)) in
  let () = Print.print_endline () in
  let () = Print.print_float (mean_float(lambda1)) in
  let () = Print.print_endline () in
  let () = Print.print_float (mean_float(lambda2)) in
  ()
in

(* observations *)
let data = List.map(preprocess_data, read("data/processed_data.csv")) in

let n_data = List.length(data) in
let alpha = div (1.0, mean(data)) in

let approx lambda1 <- exponential(alpha) in
let exact lambda2 <- exponential(alpha) in
let exact tau <- uniform_int(0, sub_int(n_data, 1)) in
let res = List.fold_resample(step, data, (tau, lambda1, lambda2, 0)) in
let (tau, res) = split(res) in
let (lambda1, res) = split(res) in
let (lambda2, _) = split(res) in
(tau, lambda1, lambda2)