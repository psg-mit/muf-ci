open Probzelus
open Distribution
open Muf
open Infer_muf

let preprocess_data entry muf_k =
  (fun v2 -> (List.tl v2) (fun v1 -> (List.hd v1) muf_k)) entry
let step (prev, yobs) muf_k =
  (fun v46 ->
     (fun v3 ->
        let (first, prev) = v3 in
        (fun v45 ->
           (fun v4 ->
              let (outlier_prob, prev) = v4 in
              (fun v44 ->
                 (fun v5 ->
                    let (prev_xt, xs) = v5 in
                    (fun v41 ->
                       (fun v42 ->
                          (fun v43 ->
                             (fun v6 ->
                                let xt_mu = v6 in
                                (fun v38 ->
                                   (fun v39 ->
                                      (fun v40 ->
                                         (fun v7 ->
                                            let xt_var = v7 in
                                            (fun v36 ->
                                               (fun v37 ->
                                                  (fun v35 ->
                                                     (fun v34 ->
                                                        (sample "step_xt" v34)
                                                          (fun v33 ->
                                                             (fun v8 ->
                                                                let xt = v8 in
                                                                (fun v32 ->
                                                                   (fun v31
                                                                    ->
                                                                    (sample
                                                                    "step_is_outlier"
                                                                    v31)
                                                                    (fun v9
                                                                    ->
                                                                    let is_outlier
                                                                    = v9 in
                                                                    (fun v28
                                                                    ->
                                                                    (fun v29
                                                                    ->
                                                                    (fun v30
                                                                    ->
                                                                    (fun v10
                                                                    ->
                                                                    let mu =
                                                                    v10 in
                                                                    (fun v25
                                                                    ->
                                                                    (fun v26
                                                                    ->
                                                                    (fun v27
                                                                    ->
                                                                    (fun v11
                                                                    ->
                                                                    let var =
                                                                    v11 in
                                                                    (fun v23
                                                                    ->
                                                                    (fun v24
                                                                    ->
                                                                    (fun v22
                                                                    ->
                                                                    (fun v20
                                                                    ->
                                                                    (fun v21
                                                                    ->
                                                                    (observe
                                                                    v20 v21)
                                                                    (fun v12
                                                                    ->
                                                                    let (_unit_v47
                                                                    :
                                                                    unit expr)
                                                                    = v12 in
                                                                    (fun v13
                                                                    ->
                                                                    (fun v14
                                                                    ->
                                                                    (fun v15
                                                                    ->
                                                                    (fun v18
                                                                    ->
                                                                    (fun v19
                                                                    ->
                                                                    (fun v17
                                                                    ->
                                                                    (List.cons
                                                                    v17)
                                                                    (fun v16
                                                                    ->
                                                                    muf_k
                                                                    (pair v13
                                                                    (pair v14
                                                                    (pair v15
                                                                    v16)))))
                                                                    (v18,
                                                                    v19)) xs)
                                                                    xt) xt)
                                                                    outlier_prob)
                                                                    (const
                                                                    false)))
                                                                    yobs)
                                                                    (gaussian
                                                                    v22))
                                                                    (v23,
                                                                    v24)) var)
                                                                    mu)
                                                                    (ite v25
                                                                    v26 v27))
                                                                    (const 1.))
                                                                    (const
                                                                    10000.))
                                                                    is_outlier)
                                                                    (ite v28
                                                                    v29 v30))
                                                                    xt)
                                                                    (const 0.))
                                                                    is_outlier))
                                                                    (bernoulli
                                                                    v32))
                                                                  outlier_prob)
                                                               (value v33)))
                                                       (gaussian v35))
                                                    (v36, v37)) xt_var) xt_mu)
                                           (ite v38 v39 v40)) (const 1.))
                                     (const 2500.)) first) (ite v41 v42 v43))
                            prev_xt) (const 0.)) first) (split v44)) prev)
             (split v45)) prev) (split v46)) prev
let output out =
  let (outlier_prob, xs) = split out in
  let (_unit_v50 : unit expr) = Print.print_float (mean_float outlier_prob) in
  let (_unit_v49 : unit expr) = Print.print_endline (const ()) in
  let (_unit_v48 : unit expr) = Print.print_float_list2 xs in const ()
let main _ muf_k =
  (fun v51 ->
     let n = v51 in
     (fun v81 ->
        (fun v83 ->
           (fun v82 ->
              (fun v80 ->
                 (List.map v80)
                   (fun v52 ->
                      let data = v52 in
                      (fun v78 ->
                         (fun v79 ->
                            (fun v77 ->
                               (fun v76 ->
                                  (sample "outlier_prob" v76)
                                    (fun v75 ->
                                       (fun v53 ->
                                          let outlier_prob = v53 in
                                          (fun v67 ->
                                             (fun v68 ->
                                                (fun v70 ->
                                                   (fun v71 ->
                                                      (fun v72 ->
                                                         (fun v74 ->
                                                            (fun v73 ->
                                                               (fun v69 ->
                                                                  (fun v66 ->
                                                                    (List.fold_resample
                                                                    v66)
                                                                    (fun v65
                                                                    ->
                                                                    (fun v54
                                                                    ->
                                                                    let 
                                                                    (_, res)
                                                                    = v54 in
                                                                    (fun v64
                                                                    ->
                                                                    (fun v55
                                                                    ->
                                                                    let 
                                                                    (outlier_prob,
                                                                    res) =
                                                                    v55 in
                                                                    (fun v63
                                                                    ->
                                                                    (fun v56
                                                                    ->
                                                                    let 
                                                                    (_, xs) =
                                                                    v56 in
                                                                    (fun v62
                                                                    ->
                                                                    (List.rev
                                                                    v62)
                                                                    (fun v57
                                                                    ->
                                                                    let xs =
                                                                    v57 in
                                                                    (fun v61
                                                                    ->
                                                                    (List.tl
                                                                    v61)
                                                                    (fun v58
                                                                    ->
                                                                    let xs =
                                                                    v58 in
                                                                    (fun v59
                                                                    ->
                                                                    (fun v60
                                                                    ->
                                                                    muf_k
                                                                    (pair v59
                                                                    v60)) xs)
                                                                    outlier_prob))
                                                                    xs)) xs)
                                                                    (split
                                                                    v63)) res)
                                                                    (split
                                                                    v64)) res)
                                                                    (split
                                                                    v65)))
                                                                    (v67,
                                                                    v68, v69))
                                                                 (pair v70
                                                                    (
                                                                    pair v71
                                                                    (pair v72
                                                                    v73))))
                                                              (lst [v74]))
                                                           (const 0.))
                                                        (const 0.))
                                                     outlier_prob)
                                                  (const true)) data) step)
                                         (value v75))) (beta v77)) (v78, v79))
                           (const 1000.)) (const 100.))) (v81, v82))
             (read v83)) (const "data/processed_data.csv")) preprocess_data)
    (const 100)
let post_main _ = 
  let () = Format.printf "==== OUTPUT ====\n" in
  let _ = infer 1 main (Some output) in
  let () = Format.printf "\n==== RUNTIME APPROXIMATION STATUS ====\n" in
  let () = Format.printf "%s\n" (pp_approx_status false) in ()
let _ =
  post_main ()
