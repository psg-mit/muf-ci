
val warm_up_iters = fun () -> 250
val n = fun () -> 501

val main = stream {
  init = (true, 0., 0., 0);
  step ((first, x, clutter_prob, i), ()) = 
    let i = if first then 0 else add_int(i, 1) in
    let x = if first then draw (Distribution.gaussian(0., 2500.)) else draw (Distribution.gaussian(x, 1.)) in
    let clutter_prob = if first then draw (Distribution.beta(100., 1000.)) else clutter_prob in

    let is_clutter = draw (Distribution.bernoulli (clutter_prob)) in
    let y = if is_clutter then draw (Distribution.gaussian(0., 10000.)) else draw (Distribution.gaussian(x, 1.)) in
    let () = if first then
      let () = print_string(string_of_float (clutter_prob)) in
      print_newline(()) else () in
    let () = if lt_det(warm_up_iters(()), i) then
      if lt_det(i, add_int(warm_up_iters(()), n(()))) then
        let () = print_string(concat(concat((string_of_float (x)), ", "), (string_of_float (y)))) in
        print_newline(())
      else exit(0)
    else
      ()
    in
    ((), (false, x, clutter_prob, i))
}