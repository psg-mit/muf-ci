open Probzelus
open Distribution
open Muf
open Infer_muf

let preprocess_data entry muf_k =
  (fun v2 -> (List.hd v2) (fun v1 -> muf_k (int_of_float_det v1))) entry
let add_data (x, y) muf_k =
  (fun v4 ->
     (fun v6 ->
        (fun v5 -> (fun v3 -> muf_k (add v3)) (v4, v5)) (float_of_int_det v6))
       y) x
let mean data muf_k =
  (fun v15 ->
     (fun v16 ->
        (fun v17 ->
           (fun v14 ->
              (List.fold v14)
                (fun v7 ->
                   let sum = v7 in
                   (fun v13 ->
                      (List.length v13)
                        (fun v12 ->
                           (fun v8 ->
                              let n = v8 in
                              (fun v10 ->
                                 (fun v11 ->
                                    (fun v9 -> muf_k (div v9)) (v10, v11)) n)
                                sum) (float_of_int_det v12))) data))
             (v15, v16, v17)) (const 0.)) data) add_data
let step (params, count_obs) muf_k =
  (fun v41 ->
     (fun v18 ->
        let (tau, params) = v18 in
        (fun v40 ->
           (fun v19 ->
              let (lambda1, params) = v19 in
              (fun v39 ->
                 (fun v20 ->
                    let (lambda2, i) = v20 in
                    (fun v37 ->
                       (fun v38 ->
                          (fun v36 ->
                             (fun v33 ->
                                (fun v34 ->
                                   (fun v35 ->
                                      (fun v21 ->
                                         let lambda = v21 in
                                         (fun v32 ->
                                            (fun v30 ->
                                               (fun v31 ->
                                                  (observe v30 v31)
                                                    (fun v22 ->
                                                       let (_unit_v42 :
                                                         unit expr) = v22 in
                                                       (fun v23 ->
                                                          (fun v24 ->
                                                             (fun v25 ->
                                                                (fun v28 ->
                                                                   (fun v29
                                                                    ->
                                                                    (fun v27
                                                                    ->
                                                                    (fun v26
                                                                    ->
                                                                    muf_k
                                                                    (pair v23
                                                                    (pair v24
                                                                    (pair v25
                                                                    v26))))
                                                                    (int_add
                                                                    v27))
                                                                    (v28,
                                                                    v29))
                                                                    (const 1))
                                                                  i) lambda2)
                                                            lambda1) tau))
                                                 count_obs) (poisson v32))
                                           lambda) (ite v33 v34 v35)) lambda2)
                                  lambda1) (lt v36)) (v37, v38)) tau) i)
                   (split v39)) params) (split v40)) params) (split v41))
    params
let output out =
  let (tau, out) = split out in
  let (lambda1, lambda2) = split out in
  let (_unit_v47 : unit expr) = Print.print_float (mean_int tau) in
  let (_unit_v46 : unit expr) = Print.print_endline (const ()) in
  let (_unit_v45 : unit expr) = Print.print_float (mean_float lambda1) in
  let (_unit_v44 : unit expr) = Print.print_endline (const ()) in
  let (_unit_v43 : unit expr) = Print.print_float (mean_float lambda2) in
  const ()
let main _ muf_k =
  (fun v89 ->
     (fun v91 ->
        (fun v90 ->
           (fun v88 ->
              (List.map v88)
                (fun v48 ->
                   let data = v48 in
                   (fun v87 ->
                      (List.length v87)
                        (fun v49 ->
                           let n_data = v49 in
                           (fun v84 ->
                              (fun v86 ->
                                 (mean v86)
                                   (fun v85 ->
                                      (fun v83 ->
                                         (fun v50 ->
                                            let alpha = v50 in
                                            (fun v82 ->
                                               (fun v81 ->
                                                  (sample "lambda1" v81)
                                                    (fun v51 ->
                                                       let lambda1 = v51 in
                                                       (fun v80 ->
                                                          (fun v79 ->
                                                             (sample
                                                                "lambda2" v79)
                                                               (fun v52 ->
                                                                  let lambda2
                                                                    = v52 in
                                                                  (fun v74 ->
                                                                    (fun v77
                                                                    ->
                                                                    (fun v78
                                                                    ->
                                                                    (fun v76
                                                                    ->
                                                                    (fun v75
                                                                    ->
                                                                    (fun v73
                                                                    ->
                                                                    (fun v72
                                                                    ->
                                                                    (sample
                                                                    "tau" v72)
                                                                    (fun v53
                                                                    ->
                                                                    let tau =
                                                                    v53 in
                                                                    (fun v65
                                                                    ->
                                                                    (fun v66
                                                                    ->
                                                                    (fun v68
                                                                    ->
                                                                    (fun v69
                                                                    ->
                                                                    (fun v70
                                                                    ->
                                                                    (fun v71
                                                                    ->
                                                                    (fun v67
                                                                    ->
                                                                    (fun v64
                                                                    ->
                                                                    (List.fold_resample
                                                                    v64)
                                                                    (fun v54
                                                                    ->
                                                                    let res =
                                                                    v54 in
                                                                    (fun v63
                                                                    ->
                                                                    (fun v55
                                                                    ->
                                                                    let 
                                                                    (tau,
                                                                    res) =
                                                                    v55 in
                                                                    (fun v62
                                                                    ->
                                                                    (fun v56
                                                                    ->
                                                                    let 
                                                                    (lambda1,
                                                                    res) =
                                                                    v56 in
                                                                    (fun v61
                                                                    ->
                                                                    (fun v57
                                                                    ->
                                                                    let 
                                                                    (lambda2,
                                                                    _) = v57 in
                                                                    (fun v58
                                                                    ->
                                                                    (fun v59
                                                                    ->
                                                                    (fun v60
                                                                    ->
                                                                    muf_k
                                                                    (pair v58
                                                                    (pair v59
                                                                    v60)))
                                                                    lambda2)
                                                                    lambda1)
                                                                    tau)
                                                                    (split
                                                                    v61)) res)
                                                                    (split
                                                                    v62)) res)
                                                                    (split
                                                                    v63)) res))
                                                                    (v65,
                                                                    v66, v67))
                                                                    (pair v68
                                                                    (pair v69
                                                                    (pair v70
                                                                    v71))))
                                                                    (const 0))
                                                                    lambda2)
                                                                    lambda1)
                                                                    tau) data)
                                                                    step))
                                                                    (uniform_int
                                                                    v73))
                                                                    (v74,
                                                                    v75))
                                                                    (sub_int
                                                                    v76))
                                                                    (v77,
                                                                    v78))
                                                                    (const 1))
                                                                    n_data)
                                                                    (
                                                                    const 0)))
                                                            (exponential v80))
                                                         alpha))
                                                 (exponential v82)) alpha)
                                           (div v83)) (v84, v85))) data)
                             (const 1.0))) data)) (v89, v90)) (read v91))
       (const "data/processed_data.csv")) preprocess_data
let post_main _ = 
  let () = Format.printf "==== OUTPUT ====\n" in
  let _ = infer 1 main (Some output) in
  let () = Format.printf "\n==== RUNTIME APPROXIMATION STATUS ====\n" in
  let () = Format.printf "%s\n" (pp_approx_status false) in ()
let _ =
  post_main ()
