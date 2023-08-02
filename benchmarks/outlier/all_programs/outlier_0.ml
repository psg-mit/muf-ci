open Probzelus
open Distribution
open Muf
open Infer_muf

let preprocess_data entry muf_k =
  (fun v2 -> (List.tl v2) (fun v1 -> (List.hd v1) muf_k)) entry
let step (prev, yobs) muf_k =
  (fun v45 ->
     (fun v3 ->
        let (first, prev) = v3 in
        (fun v44 ->
           (fun v4 ->
              let (outlier_prob, prev) = v4 in
              (fun v43 ->
                 (fun v5 ->
                    let (prev_xt, xs) = v5 in
                    (fun v40 ->
                       (fun v41 ->
                          (fun v42 ->
                             (fun v6 ->
                                let xt_mu = v6 in
                                (fun v37 ->
                                   (fun v38 ->
                                      (fun v39 ->
                                         (fun v7 ->
                                            let xt_var = v7 in
                                            (fun v35 ->
                                               (fun v36 ->
                                                  (fun v34 ->
                                                     (fun v33 ->
                                                        (sample "step_xt" v33)
                                                          (fun v8 ->
                                                             let xt = v8 in
                                                             (fun v32 ->
                                                                (fun v31 ->
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
                                                                    let (_unit_v46
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
                                                               outlier_prob))
                                                       (gaussian v34))
                                                    (v35, v36)) xt_var) xt_mu)
                                           (ite v37 v38 v39)) (const 1.))
                                     (const 2500.)) first) (ite v40 v41 v42))
                            prev_xt) (const 0.)) first) (split v43)) prev)
             (split v44)) prev) (split v45)) prev
let output out =
  let (outlier_prob, xs) = split out in
  let (_unit_v49 : unit expr) = Print.print_float (mean_float outlier_prob) in
  let (_unit_v48 : unit expr) = Print.print_endline (const ()) in
  let (_unit_v47 : unit expr) = Print.print_float_list2 xs in const ()
let main _ muf_k =
  (fun v50 ->
     let n = v50 in
     (fun v79 ->
        (fun v81 ->
           (fun v80 ->
              (fun v78 ->
                 (List.map v78)
                   (fun v51 ->
                      let data = v51 in
                      (fun v76 ->
                         (fun v77 ->
                            (fun v75 ->
                               (fun v74 ->
                                  (sample "outlier_prob" v74)
                                    (fun v52 ->
                                       let outlier_prob = v52 in
                                       (fun v66 ->
                                          (fun v67 ->
                                             (fun v69 ->
                                                (fun v70 ->
                                                   (fun v71 ->
                                                      (fun v73 ->
                                                         (fun v72 ->
                                                            (fun v68 ->
                                                               (fun v65 ->
                                                                  (List.fold_resample
                                                                    v65)
                                                                    (
                                                                    fun v64
                                                                    ->
                                                                    (fun v53
                                                                    ->
                                                                    let 
                                                                    (_, res)
                                                                    = v53 in
                                                                    (fun v63
                                                                    ->
                                                                    (fun v54
                                                                    ->
                                                                    let 
                                                                    (outlier_prob,
                                                                    res) =
                                                                    v54 in
                                                                    (fun v62
                                                                    ->
                                                                    (fun v55
                                                                    ->
                                                                    let 
                                                                    (_, xs) =
                                                                    v55 in
                                                                    (fun v61
                                                                    ->
                                                                    (List.rev
                                                                    v61)
                                                                    (fun v56
                                                                    ->
                                                                    let xs =
                                                                    v56 in
                                                                    (fun v60
                                                                    ->
                                                                    (List.tl
                                                                    v60)
                                                                    (fun v57
                                                                    ->
                                                                    let xs =
                                                                    v57 in
                                                                    (fun v58
                                                                    ->
                                                                    (fun v59
                                                                    ->
                                                                    muf_k
                                                                    (pair v58
                                                                    v59)) xs)
                                                                    outlier_prob))
                                                                    xs)) xs)
                                                                    (split
                                                                    v62)) res)
                                                                    (split
                                                                    v63)) res)
                                                                    (split
                                                                    v64)))
                                                                 (v66, v67,
                                                                   v68))
                                                              (pair v69
                                                                 (pair v70
                                                                    (
                                                                    pair v71
                                                                    v72))))
                                                           (lst [v73]))
                                                        (const 0.))
                                                     (const 0.)) outlier_prob)
                                               (const true)) data) step))
                                 (beta v75)) (v76, v77)) (const 1000.))
                        (const 100.))) (v79, v80)) (read v81))
          (const "data/processed_data.csv")) preprocess_data) (const 100)
let post_main _ = 
  let () = Format.printf "==== OUTPUT ====\n" in
  let _ = infer 1 main (Some output) in
  let () = Format.printf "\n==== RUNTIME APPROXIMATION STATUS ====\n" in
  let () = Format.printf "%s\n" (pp_approx_status false) in ()
let _ =
  post_main ()
