val preprocess_data = fun entry -> List.hd(entry) in

val make_observations = fun (acc, yobs) ->
  let (theta, acc) = split(acc) in
  let (mu1, mu2) = split(acc) in

  let approx switch <- bernoulli(theta) in

  let mu = if switch then mu1 else mu2 in
  let () = observe(gaussian(mu, 1.), yobs) in
  (theta, mu1, mu2)
in

val output = fun out ->
  let (theta, out) = split(out) in
  let (mu1, mu2) = split(out) in
  let () = Print.print_float (mean_float(theta)) in
  let () = Print.print_endline () in
  let () = Print.print_float (mean_float(mu1)) in
  let () = Print.print_endline () in
  let () = Print.print_float (mean_float(mu2)) in
  ()
in

(* observations *)
let data = List.map(preprocess_data, read("data/processed_data.csv")) in

(* improper prior *)
let approx theta <- beta(1., 1.) in

let exact mu1 <- gaussian(-10., 1.) in
let approx mu2 <- gaussian(10., 1.) in

let out = List.fold_resample(make_observations, data, (theta, mu1, mu2)) in
out