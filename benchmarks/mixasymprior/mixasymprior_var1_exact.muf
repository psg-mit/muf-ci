(* 
  inference strategy
  mu1 - approx
  mu2 - approx
  switch - approx
  var1 - exact
  var2 - approx
*)

val preprocess_data = fun entry -> List.hd(entry) in

val make_observations = fun (acc, yobs) ->
  let (mu1, acc) = split(acc) in
  let (mu2, acc) = split(acc) in
  let (var1, var2) = split(acc) in

  let approx switch <- bernoulli(0.3) in

  let mu = if switch then mu1 else mu2 in
  let var = if switch then var1 else var2 in
  let () = observe(gaussian(mu, div(1., var)), yobs) in
  (mu1, mu2, var1, var2)
in

val output = fun out ->
  let (mu1, out) = split(out) in
  let (mu2, out) = split(out) in
  let (var1, var2) = split(out) in
  let () = Print.print_float (mean_float(mu1)) in
  let () = Print.print_endline () in
  let () = Print.print_float (mean_float(mu2)) in
  let () = Print.print_endline () in
  let () = Print.print_float (mean_float(var1)) in
  let () = Print.print_endline () in
  let () = Print.print_float (mean_float(var2)) in
  ()
in

(* observations *)
let data = List.map(preprocess_data, read("data/processed_data.csv")) in

let approx mu1 <- gaussian(2.75, 0.5) in
let approx mu2 <- gaussian(-2.75, 0.5) in

(* Improper prior *)
let exact var1 <- gamma(1., 1.) in
let approx var2 <- gamma(1., 1.) in

let out = List.fold_resample(make_observations, data, (mu1, mu2, var1, var2)) in
out