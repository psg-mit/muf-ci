(* 
  default inference strategy
  q - exact
  r - dynamic (both exact and approx)
  x - approx
*)

val preprocess_data = fun entry -> List.hd(List.tl(entry))

val step = fun (acc, zobs) ->
  let (xs, acc) = split(acc) in
  let (q, r) = split(acc) in

  let prev_x = List.hd(xs) in

  let h = 2. in
  let f = 1.001 in
  (* let g = 1. in *)
  (* let u = 0, so g not used *)

  let x <- gaussian(mul(f, prev_x), div(1., q)) in
  let () = observe(gaussian(mul(h, prev_x), div(1., r)), zobs) in
  
  (List.cons(x, xs), q, r)

val output = fun out ->
  let (xs, out) = split(out) in
  let (q, r) = split(out) in
  let () = Print.print_float (mean_float(q)) in
  let () = Print.print_endline () in
  let () = Print.print_float (mean_float(r)) in
  let () = Print.print_endline () in
  let () = Print.print_float_list2 (xs) in
  ()

let data = List.map(preprocess_data, read("data/processed_data.csv")) in

(* Improper prior *)
let q <- gamma (1., 1.) in
let r <- gamma (1., 1.) in
let x0 = 0. in

let out = List.fold_resample(step, data, ([x0], q, r)) in
let (xs, out) = split(out) in
let (q, r) = split(out) in

let xs = List.rev(xs) in

(* Drop x0 *)
let xs = List.tl(xs) in
(xs, q, r)
