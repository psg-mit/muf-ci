open Probzelus
open Distribution
open Muf
open Infer_muf

let preprocess_data entry muf_k = (fun v1 -> (List.hd v1) muf_k) entry
let make_observations (acc, yobs) muf_k =
  (fun v22 ->
     (fun v2 ->
        let (theta, acc) = v2 in
        (fun v21 ->
           (fun v3 ->
              let (mu1, mu2) = v3 in
              (fun v20 ->
                 (fun v19 ->
                    (sample "make_observations_switch" v19)
                      (fun v18 ->
                         (fun v4 ->
                            let switch = v4 in
                            (fun v15 ->
                               (fun v16 ->
                                  (fun v17 ->
                                     (fun v5 ->
                                        let mu = v5 in
                                        (fun v13 ->
                                           (fun v14 ->
                                              (fun v12 ->
                                                 (fun v10 ->
                                                    (fun v11 ->
                                                       (observe v10 v11)
                                                         (fun v6 ->
                                                            let (_unit_v23 :
                                                              unit expr) = v6 in
                                                            (fun v7 ->
                                                               (fun v8 ->
                                                                  (fun v9 ->
                                                                    muf_k
                                                                    (pair v7
                                                                    (pair v8
                                                                    v9))) mu2)
                                                                 mu1) theta))
                                                      yobs) (gaussian v12))
                                                (v13, v14)) (const 1.)) mu)
                                       (ite v15 v16 v17)) mu2) mu1) switch)
                           (value v18))) (bernoulli v20)) theta) (split v21))
          acc) (split v22)) acc
let output out =
  let (theta, out) = split out in
  let (mu1, mu2) = split out in
  let (_unit_v28 : unit expr) = Print.print_float (mean_float theta) in
  let (_unit_v27 : unit expr) = Print.print_endline (const ()) in
  let (_unit_v26 : unit expr) = Print.print_float (mean_float mu1) in
  let (_unit_v25 : unit expr) = Print.print_endline (const ()) in
  let (_unit_v24 : unit expr) = Print.print_float (mean_float mu2) in
  const ()
let main _ muf_k =
  (fun v55 ->
     (fun v57 ->
        (fun v56 ->
           (fun v54 ->
              (List.map v54)
                (fun v29 ->
                   let data = v29 in
                   (fun v52 ->
                      (fun v53 ->
                         (fun v51 ->
                            (fun v50 ->
                               (sample "theta" v50)
                                 (fun v30 ->
                                    let theta = v30 in
                                    (fun v48 ->
                                       (fun v49 ->
                                          (fun v47 ->
                                             (fun v46 ->
                                                (sample "mu1" v46)
                                                  (fun v45 ->
                                                     (fun v31 ->
                                                        let mu1 = v31 in
                                                        (fun v43 ->
                                                           (fun v44 ->
                                                              (fun v42 ->
                                                                 (fun v41 ->
                                                                    (sample
                                                                    "mu2" v41)
                                                                    (fun v32
                                                                    ->
                                                                    let mu2 =
                                                                    v32 in
                                                                    (fun v35
                                                                    ->
                                                                    (fun v36
                                                                    ->
                                                                    (fun v38
                                                                    ->
                                                                    (fun v39
                                                                    ->
                                                                    (fun v40
                                                                    ->
                                                                    (fun v37
                                                                    ->
                                                                    (fun v34
                                                                    ->
                                                                    (List.fold_resample
                                                                    v34)
                                                                    (fun v33
                                                                    ->
                                                                    let out =
                                                                    v33 in
                                                                    muf_k out))
                                                                    (v35,
                                                                    v36, v37))
                                                                    (pair v38
                                                                    (pair v39
                                                                    v40)))
                                                                    mu2) mu1)
                                                                    theta)
                                                                    data)
                                                                    make_observations))
                                                                   (gaussian
                                                                    v42))
                                                                (v43, v44))
                                                             (const 1.))
                                                          (const 10.))
                                                       (value v45)))
                                               (gaussian v47)) (v48, v49))
                                         (const 1.)) (const (-10.))))
                              (beta v51)) (v52, v53)) (const 1.)) (const 1.)))
             (v55, v56)) (read v57)) (const "data/processed_data.csv"))
    preprocess_data
let post_main _ = 
  let () = Format.printf "==== OUTPUT ====\n" in
  let _ = infer 1 main (Some output) in
  let () = Format.printf "\n==== RUNTIME APPROXIMATION STATUS ====\n" in
  let () = Format.printf "%s\n" (pp_approx_status false) in ()
let _ =
  post_main ()
