open Probzelus
open Distribution
open Muf
open Infer_semi_symbolic
open Infer_muf

let n_data () = 74
let alpha () = const (div_float (1.0, (float_of_int (n_data ()))))
let true_lambda1 () = 17.749241151314294
let true_lambda2 () = 22.72325027312415
let true_tau () = 44.308125
let se (x, y) = pow ((sub_float (x, y)), 2.)
let print_float_d d = let () = print_float (mean_float d) in print_string " "
let print_int_d d = let () = print_float (mean_int d) in print_string " "
let mse ((true_lambda1, true_lambda2, true_tau), d) =
  let (lambda1_d, d1) = split d in
  let (lambda2_d, tau_d) = split d1 in
  let lambda1_mse = se ((mean_float lambda1_d), true_lambda1) in
  let lambda2_mse = se ((mean_float lambda2_d), true_lambda2) in
  let tau_mse = se ((mean_int tau_d), true_tau) in
  let total_mse = add_float ((add_float (lambda1_mse, lambda2_mse)), tau_mse) in
  let () = print_string "total_mse:" in
  let () = print_float total_mse in
  let () = print_string " lambda1_mse:" in
  let () = print_float lambda1_mse in
  let () = print_string " lambda2_mse:" in
  let () = print_float lambda2_mse in
  let () = print_string " tau_mse:" in let () = print_float tau_mse in ()
let done_ d =
  let () = mse (((true_lambda1 ()), (true_lambda2 ()), (true_tau ())), d) in
  let () = print_newline () in let () = exit 0 in ()
type sms_behavior = unit
let sms_behavior _ =
  {
    Muflib.init = (true, (const 0.), (const 0.), (const 0));
    Muflib.step =
      (fun ((first, lambda1, lambda2, tau), (i, count)) ->
         let (lambda1, lambda2) =
           if first
           then
             ((approx
                 (Muflib.prob_op sample prob
                    ("lambda1", (exponential (alpha ()))))),
               (Muflib.prob_op sample prob
                  ("lambda2", (exponential (alpha ())))))
           else (lambda1, lambda2) in
         let tau =
           if first
           then
             approx
               (Muflib.prob_op sample prob
                  ("tau", (uniform_int (0, (n_data ())))))
           else tau in
         let lambda = ite ((lt ((const i), tau)), lambda1, lambda2) in
         let () = Muflib.prob_op observe prob ((poisson lambda), count) in
         ((pair (lambda1, (pair (lambda2, tau)))),
           (false, lambda1, lambda2, tau)))
  }
type data = unit
let data _ =
  {
    Muflib.init = (true, 0, List.nil);
    Muflib.step =
      (fun ((first, i, data), ()) ->
         let i = if first then 0 else i in
         let data = if first then read "data/processed_data.csv" else data in
         let is_done = eq_det (i, (sub_int ((n_data ()), 1))) in
         let count = int_of_float (List.hd (List.hd data)) in
         let data' = List.tl data in
         ((is_done, i, count), (false, (add_int (i, 1)), data')))
  }
type main = unit
let main _ =
  {
    Muflib.init =
      ((Muflib.init data),
        (Muflib.init
           (Muflib.muf_node_of_cnode
              (infer 70 (Muflib.cnode_of_muf_proba_node sms_behavior)))));
    Muflib.step =
      (fun ((data, sms_behavior), ()) ->
         let (obs_data, data') = Muflib.step data () in
         let (is_done, i, count) = obs_data in
         let (d, sms_behavior') = Muflib.step sms_behavior (i, count) in
         let () = if is_done then done_ d else () in
         ((), (data', sms_behavior')))
  }
