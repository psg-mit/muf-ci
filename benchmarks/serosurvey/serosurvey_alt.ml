open Probzelus
open Distribution
open Muf
open Infer_semi_symbolic
open Infer_muf

let sigmoid x =
  div ((const 1.), (add ((const 1.), (expp (subtract ((const 0.), x))))))
let se (x, y) = pow ((sub_float (x, y)), 2.)
let mse ((true_p, true_sens, true_spec), distr) =
  let (p_d, s) = split distr in
  let (sens_d, spec_d) = split s in
  let p = mean_float p_d in
  let sens = mean_float sens_d in
  let spec = mean_float spec_d in
  let p_mse = se (p, true_p) in
  let sens_mse = se (sens, true_sens) in
  let spec_mse = se (spec, true_spec) in
  let () = print_string "p: " in
  let () = print_float p in
  let () = print_string " sens: " in
  let () = print_float sens in
  let () = print_string " spec: " in
  let () = print_float spec in
  let () = print_string " p_mse: " in
  let () = print_float p_mse in
  let () = print_string " sens_mse: " in
  let () = print_float sens_mse in
  let () = print_string " spec_mse: " in
  let () = print_float spec_mse in (p_mse, sens_mse, spec_mse)
type serosurvey = unit
let serosurvey _ =
  {
    Muflib.init = (true, (const 0.), (const 0.));
    Muflib.step =
      (fun
         ((first, sens, fpr),
          (survey_result, control_tp_result, control_fp_result))
         ->
         let (sens, fpr) =
           if first
           then
             ((Muflib.prob_op sample prob (beta (1., 1.))),
               (Muflib.prob_op sample prob (beta (1., 1.))))
           else (sens, fpr) in
         let p = Muflib.prob_op sample prob (gaussian ((const 0.), 1.)) in
         let true_pos = Muflib.prob_op sample prob (bernoulli (sigmoid p)) in
         let () =
           Muflib.prob_op observe prob
             ((bernoulli (ite ((const (eval true_pos)), sens, fpr))),
               survey_result) in
         let n_pos_control = 181 in
         let n_neg_control = 176 in
         let () =
           if first
           then
             Muflib.prob_op observe prob
               ((binomial (n_pos_control, sens)), control_tp_result)
           else () in
         let () =
           if first
           then
             Muflib.prob_op observe prob
               ((binomial (n_neg_control, fpr)), control_fp_result)
           else () in
         let spec = subtract ((const 1.), fpr) in
         ((pair (p, (pair (sens, spec)))), (false, sens, fpr)))
  }
type survey_result = unit
let survey_result _ =
  {
    Muflib.init = (true, 0, 0);
    Muflib.step =
      (fun ((first, n_pos, n_neg), ()) ->
         let (n_pos, n_neg) = if first then (4, 148) else (n_pos, n_neg) in
         let (res, n_pos, n_neg) =
           if lt (0, n_pos)
           then (true, (sub_int (n_pos, 1)), n_neg)
           else
             if lt (0, n_neg)
             then (false, n_pos, (sub_int (n_neg, 1)))
             else exit 0 in
         (res, (false, n_pos, n_neg)))
  }
type main = unit
let main _ =
  {
    Muflib.init =
      ((Muflib.init survey_result),
        (Muflib.init
           (Muflib.muf_node_of_cnode
              (infer 4500 (Muflib.cnode_of_muf_proba_node serosurvey)))));
    Muflib.step =
      (fun ((survey_result, serosurvey), ()) ->
         let (survey_res, survey_result') = Muflib.step survey_result () in
         let control_tp_result = 154 in
         let control_fp_result = 0 in
         let (d, serosurvey') =
           Muflib.step serosurvey
             (survey_res, control_tp_result, control_fp_result) in
         let true_p = 0.032 in
         let true_sens = 0.856 in
         let true_spec = 0.998 in
         let _ = mse ((true_p, true_sens, true_spec), d) in
         let () = print_newline () in ((), (survey_result', serosurvey')))
  }
