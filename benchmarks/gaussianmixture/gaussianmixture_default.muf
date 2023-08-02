(* 
  inference strategy
  mu1 - approx
  mu2 - approx
  switch - approx
  theta - exact
*)

val preprocess_data = fun entry -> List.hd(entry)

val make_observations = fun (acc, yobs) ->
  let (theta, acc) = split(acc) in
  let (mu1, mu2) = split(acc) in

  let switch <- bernoulli(theta) in

  let mu = if switch then mu1 else mu2 in
  let () = observe(gaussian(mu, 1.), yobs) in
  (theta, mu1, mu2)


val output = fun out ->
  let (theta, out) = split(out) in
  let (mu1, mu2) = split(out) in
  let () = Print.print_float (mean_float(theta)) in
  let () = Print.print_endline () in
  let () = Print.print_float (mean_float(mu1)) in
  let () = Print.print_endline () in
  let () = Print.print_float (mean_float(mu2)) in
  ()

(* observations *)
let data = List.map(preprocess_data, read("data/processed_data.csv")) in

(* improper prior *)
let theta <- beta(1., 1.) in

let mu1 <- gaussian(-10., 1.) in
let mu2 <- gaussian(10., 1.) in

let out = List.fold_resample(make_observations, data, (theta, mu1, mu2)) in
out