open Probzelus
open Distribution
open Muf
open Infer_semi_symbolic
open Infer_muf

let hh () = 728
let n_survey () = 1000
let n_pos_control () = 181
let n_neg_control () = 176
let control_tp_result () = 154
let control_fp_result () = 0
let b_names () =
  List.cons
    ("intercept",
      (List.cons
         ("sex",
           (List.cons
              ("age_cat[5,10)",
                (List.cons
                   ("age_cat[10,20)",
                     (List.cons
                        ("age_cat[50,65)",
                          (List.cons
                             ("age_cat[65,105)",
                               (List.cons
                                  ("week1",
                                    (List.cons
                                       ("week3",
                                         (List.cons
                                            ("week4",
                                              (List.cons ("week5", List.nil)))))))))))))))))))
let true_b () =
  List.cons
    (2.162511,
      (List.cons
         (0.4638138,
           (List.cons
              (0.050155,
                (List.cons
                   ((-0.0583128),
                     (List.cons
                        (0.6693432,
                          (List.cons
                             (0.1790346,
                               (List.cons
                                  (0.2492106,
                                    (List.cons
                                       ((-0.2200088),
                                         (List.cons
                                            ((-0.0593874),
                                              (List.cons
                                                 (0.3817401, List.nil)))))))))))))))))))
let true_sigma () = 0.9161439
let true_sens () = 0.808969
let true_spec () = 0.9941081
let se (x, y) = pow ((sub_float (x, y)), 2.)
let print_pair (x, y) =
  let () = print_string x in
  let () = print_string "_mse:" in let () = print_float y in print_string " "
let print_expr x = let () = print_float (eval x) in print_string " "
let print_item x = let () = print_float x in print_string " "
let print_dist d = let () = print_float (mean_float d) in print_string " "
let mse ((true_b, true_sigma, true_sens, true_spec), distr) =
  let (b_d', r) = split distr in
  let b_d = split_list b_d' in
  let (sigma_d, rr) = split r in
  let (sens_d, spec_d) = split rr in
  let b = List.map (mean_float, b_d) in
  let b_mse = List.map (se, (List.zip (b, true_b))) in
  let total_b_mse = List.fold (add_float, 0., b_mse) in
  let sigma_mse = se ((mean_float sigma_d), true_sigma) in
  let sens_mse = se ((mean_float sens_d), true_sens) in
  let spec_mse = se ((mean_float spec_d), true_spec) in
  let total_mse =
    add_float
      ((add_float (total_b_mse, sigma_mse)),
        (add_float (sens_mse, spec_mse))) in
  let () = List.iter (print_pair, (List.zip ((b_names ()), b_mse))) in
  let () = print_string "sigma_mse:" in
  let () = print_float sigma_mse in
  let () = print_string " sens_mse:" in
  let () = print_float sens_mse in
  let () = print_string " spec_mse:" in
  let () = print_float spec_mse in
  let () = print_string " total_mse:" in let () = print_float total_mse in ()
let wrap_x x = const (float_of_int x)
let extract_data entry =
  let survey_res = if eq_det ((List.hd entry), 1) then true else false in
  let h = List.hd (List.tl entry) in
  let x' = List.cons (1, (List.tl (List.tl entry))) in
  let x = List.map (wrap_x, x') in (x, h, survey_res)
let sigmoid x =
  div ((const 1.), (add ((const 1.), (expp (subtract ((const 0.), x))))))
let init_b name =
  Muflib.prob_op sample prob (name, (gaussian ((const 0.), (const 1.))))
let init_eta i =
  Muflib.prob_op sample prob
    ((concat ("eta_", (string_of_int i))),
      (gaussian ((const 0.), (const 1.))))
let dot (acc, (x, b)) = add (acc, (mult (x, b)))
type serosurvey = unit
let serosurvey _ =
  {
    Muflib.init =
      (true, List.nil, Array.empty, (const 0.), (const 0.), (const 0.));
    Muflib.step =
      (fun
         ((first, b, eta, sigma, sens, fpr),
          (x, h, survey_result, control_tp_result, control_fp_result))
         ->
         let (sens, fpr) =
           if first
           then
             ((Muflib.prob_op sample prob ("sens", (beta (1., 1.)))),
               (Muflib.prob_op sample prob ("fpr", (beta (1., 1.)))))
           else (sens, fpr) in
         let b = if first then List.map (init_b, (b_names ())) else b in
         let sigma =
           if first
           then
             Muflib.prob_op sample prob
               ("sigma_h", (gaussian ((const 0.), (const 1.))))
           else sigma in
         let sigma_h =
           if lt ((const 0.), sigma)
           then sigma
           else subtract ((const 0.), sigma) in
         let eta = if first then Array.init ((hh ()), init_eta) else eta in
         let eta_h = Array.get (eta, h) in
         let p' =
           add
             ((List.fold (dot, (const 0.), (List.zip (x, b)))),
               (mult (sigma_h, eta_h))) in
         let p = sigmoid p' in
         let true_pos =
           Muflib.prob_op sample prob ("true_pos", (bernoulli p)) in
         let () =
           Muflib.prob_op observe prob
             ((bernoulli (ite (true_pos, sens, fpr))), survey_result) in
         let () =
           if first
           then
             Muflib.prob_op observe prob
               ((binomial ((n_pos_control ()), sens)), control_tp_result)
           else () in
         let () =
           if first
           then
             Muflib.prob_op observe prob
               ((binomial ((n_neg_control ()), fpr)), control_fp_result)
           else () in
         let spec = subtract ((const 1.), fpr) in
         ((pair ((lst b), (pair (sigma_h, (pair (sens, spec)))))),
           (false, b, eta, sigma, sens, fpr)))
  }
type data = unit
let data _ =
  {
    Muflib.init = (true, List.nil);
    Muflib.step =
      (fun ((first, data), ()) ->
         let data = if first then read "data/processed_data.csv" else data in
         let is_done = eq_det ((List.length data), 1) in
         let (x, h, survey_res) = extract_data (List.hd data) in
         let data' = List.tl data in
         ((is_done, x, h, survey_res, (control_tp_result ()),
            (control_fp_result ())), (false, data')))
  }
type main = unit
let main _ =
  {
    Muflib.init =
      ((Muflib.init data),
        (Muflib.init
           (Muflib.muf_node_of_cnode
              (infer 1 (Muflib.cnode_of_muf_proba_node serosurvey)))));
    Muflib.step =
      (fun ((data, serosurvey), ()) ->
         let (obs_data, data') = Muflib.step data () in
         let (is_done, x, h, survey_result, control_tp_result,
              control_fp_result)
           = obs_data in
         let (d, serosurvey') =
           Muflib.step serosurvey
             (x, h, survey_result, control_tp_result, control_fp_result) in
         let () =
           if is_done
           then
             mse
               (((true_b ()), (true_sigma ()), (true_sens ()),
                  (true_spec ())), d)
           else () in
         let () = if is_done then print_newline () else () in
         let () = if is_done then exit 0 else () in
         ((), (data', serosurvey')))
  }
