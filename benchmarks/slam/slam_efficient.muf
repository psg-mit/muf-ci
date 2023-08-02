(* 
  efficient inference strategy
  wheel_slip - approx
  x - exact
*)

val init_map = fun _ ->
  let color <- bernoulli(0.5) in
  color
in
  
val step = fun (acc, inp) ->
  let (prev_x, map) = split(acc) in

  let (cmd, obs) = split(inp) in

  let approx wheel_slip <- bernoulli(0.1) in
  let move = if wheel_slip then prev_x else int_add(prev_x, cmd) in
  let x = 
    if lt(move, 0) then 0
    else if lt(9, move) then 9
    else move
  in
  let o = Array.get(map, x) in
  let o_prob = if o then 0.9 else 0.1 in
  let () = observe(bernoulli(o_prob), obs) in

  (x, map)
in
  
val output = fun map ->
  let () = Print.print_bool_list2 (map) in
  ()
in

let map = Array.init(10, init_map) in
let x0 = 0 in

let cmd = 1 in
let obs = false in
let out = step((x0, map), (cmd, obs)) in

let (_, map) = split(out) in
let map = List.from_array(map) in
map


