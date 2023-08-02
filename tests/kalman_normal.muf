val output = fun final_x ->
  let mean_x = mean_float (final_x) in
  let () = Print.print_string (pp_mdistr(final_x)) in
  let () = Print.print_string (": ") in
  let () = Print.print_float (mean_x) in
  ()

val step = fun (pre_x, yobs) -> 
  let approx x <- gaussian (pre_x, 1.0) in
  let () = observe (gaussian (x, 1.0), yobs) in
  x

val init_y = fun i ->
  int_to_float(i)

let n = 100 in
let x = 0. in
let ys = List.rev(List.init(n, init_y)) in
(* Fold doesn't reach 100-ish *)
(* let final_x = List.fold(step, ys, x) in *)
(* Fold_resample does reach 100-ish *)
let final_x = List.fold_resample(step, ys, x) in
final_x