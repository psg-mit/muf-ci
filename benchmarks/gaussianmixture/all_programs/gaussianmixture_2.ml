open Probzelus
open Distribution
open Muf
open Infer_muf

let preprocess_data entry muf_k = (fun v1 -> (List.hd v1) muf_k) entry
let make_observations (acc, yobs) muf_k =
  (fun v21 ->
     (fun v2 ->
        let (theta, acc) = v2 in
        (fun v20 ->
           (fun v3 ->
              let (mu1, mu2) = v3 in
              (fun v19 ->
                 (fun v18 ->
                    (sample "make_observations_switch" v18)
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
                                                         let (_unit_v22 :
                                                           unit expr) = v6 in
                                                         (fun v7 ->
                                                            (fun v8 ->
                                                               (fun v9 ->
                                                                  muf_k
                                                                    (
                                                                    pair v7
                                                                    (pair v8
                                                                    v9))) mu2)
                                                              mu1) theta))
                                                   yobs) (gaussian v12))
                                             (v13, v14)) (const 1.)) mu)
                                    (ite v15 v16 v17)) mu2) mu1) switch))
                   (bernoulli v19)) theta) (split v20)) acc) (split v21)) acc
let output out =
  let (theta, out) = split out in
  let (mu1, mu2) = split out in
  let (_unit_v27 : unit expr) = Print.print_float (mean_float theta) in
  let (_unit_v26 : unit expr) = Print.print_endline (const ()) in
  let (_unit_v25 : unit expr) = Print.print_float (mean_float mu1) in
  let (_unit_v24 : unit expr) = Print.print_endline (const ()) in
  let (_unit_v23 : unit expr) = Print.print_float (mean_float mu2) in
  const ()
let main _ muf_k =
  (fun v54 ->
     (fun v56 ->
        (fun v55 ->
           (fun v53 ->
              (List.map v53)
                (fun v28 ->
                   let data = v28 in
                   (fun v51 ->
                      (fun v52 ->
                         (fun v50 ->
                            (fun v49 ->
                               (sample "theta" v49)
                                 (fun v29 ->
                                    let theta = v29 in
                                    (fun v47 ->
                                       (fun v48 ->
                                          (fun v46 ->
                                             (fun v45 ->
                                                (sample "mu1" v45)
                                                  (fun v44 ->
                                                     (fun v30 ->
                                                        let mu1 = v30 in
                                                        (fun v42 ->
                                                           (fun v43 ->
                                                              (fun v41 ->
                                                                 (fun v40 ->
                                                                    (sample
                                                                    "mu2" v40)
                                                                    (fun v31
                                                                    ->
                                                                    let mu2 =
                                                                    v31 in
                                                                    (fun v34
                                                                    ->
                                                                    (fun v35
                                                                    ->
                                                                    (fun v37
                                                                    ->
                                                                    (fun v38
                                                                    ->
                                                                    (fun v39
                                                                    ->
                                                                    (fun v36
                                                                    ->
                                                                    (fun v33
                                                                    ->
                                                                    (List.fold_resample
                                                                    v33)
                                                                    (fun v32
                                                                    ->
                                                                    let out =
                                                                    v32 in
                                                                    muf_k out))
                                                                    (v34,
                                                                    v35, v36))
                                                                    (pair v37
                                                                    (pair v38
                                                                    v39)))
                                                                    mu2) mu1)
                                                                    theta)
                                                                    data)
                                                                    make_observations))
                                                                   (gaussian
                                                                    v41))
                                                                (v42, v43))
                                                             (const 1.))
                                                          (const 10.))
                                                       (value v44)))
                                               (gaussian v46)) (v47, v48))
                                         (const 1.)) (const (-10.))))
                              (beta v50)) (v51, v52)) (const 1.)) (const 1.)))
             (v54, v55)) (read v56)) (const "data/processed_data.csv"))
    preprocess_data
let post_main _ = 
  let () = Format.printf "==== OUTPUT ====\n" in
  let _ = infer 1 main (Some output) in
  let () = Format.printf "\n==== RUNTIME APPROXIMATION STATUS ====\n" in
  let () = Format.printf "%s\n" (pp_approx_status false) in ()
let _ =
  post_main ()
