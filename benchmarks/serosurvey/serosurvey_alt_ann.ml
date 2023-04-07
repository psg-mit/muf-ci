open Probzelus
open Distribution
open Muf
open Infer_semi_symbolic
open Infer_muf

type main = unit
let main _ =
  {
    Muflib.init = ();
    Muflib.step =
      (fun ((), ()) ->
         let xt = Muflib.prob_op sample prob (beta (1., 1.)) in
         let () = Muflib.prob_op observe prob ((bernoulli xt), true) in
         (xt, ()))
  }
