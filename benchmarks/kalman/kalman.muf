val output = fun out ->
  let (final_x, xs) = split(out) in
  let () = Print.print_float_list2 (xs) in
  ()

val step = fun (prev, yobs) -> 
  let (pre_x, xs) = split(prev) in
  let x <- gaussian (pre_x, 1.0) in
  let () = observe (gaussian (x, 1.0), yobs) in
  (x, List.cons(x, xs))

val init_y = fun i ->
  int_to_float(i)

let n = 100 in
let x0 <- gaussian(0., 100.) in
let x = (x0, [x0]) in
let ys = List.rev(List.init(n, init_y)) in
(* Fold doesn't reach 100-ish *)
(* let final_x = List.fold(step, ys, x) in *)
(* Fold_resample does reach 100-ish *)
let final_x = List.fold_resample(step, ys, x) in
final_x
