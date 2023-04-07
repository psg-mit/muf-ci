open Probzelus
open Distribution
open Muf
open Infer_semi_symbolic
open Infer_muf

type coin = unit
let coin _ =
  {
    Muflib.init = (true, (const 0.));
    Muflib.step =
      (fun ((first, xt), yobs) ->
         let xt =
           if first
           then Muflib.prob_op sample prob ("xt", (beta (1., 1.)))
           else xt in
         let () = Muflib.prob_op observe prob ((bernoulli xt), yobs) in
         (xt, (false, xt)))
  }
type main = unit
let main _ =
  {
    Muflib.init =
      (Muflib.init
         (Muflib.muf_node_of_cnode
            (infer 1 (Muflib.cnode_of_muf_proba_node coin))));
    Muflib.step =
      (fun (coin, ()) ->
         let (d, s) = Muflib.step coin true in
         let () = print_any_t d in
         let () = print_newline () in
         let () = pp_approx_status false in let () = exit 1 in ((), s))
  }
