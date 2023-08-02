open Probzelus
open Distribution
open Muf
open Infer_muf

let preprocess_data entry muf_k = (fun v1 -> (List.hd v1) muf_k) entry
let make_observations (acc, yobs) muf_k =
  (fun v32 ->
     (fun v2 ->
        let (mu1, acc) = v2 in
        (fun v31 ->
           (fun v3 ->
              let (mu2, acc) = v3 in
              (fun v30 ->
                 (fun v4 ->
                    let (var1, var2) = v4 in
                    (fun v29 ->
                       (fun v28 ->
                          (sample "make_observations_switch" v28)
                            (fun v27 ->
                               (fun v5 ->
                                  let switch = v5 in
                                  (fun v24 ->
                                     (fun v25 ->
                                        (fun v26 ->
                                           (fun v6 ->
                                              let mu = v6 in
                                              (fun v21 ->
                                                 (fun v22 ->
                                                    (fun v23 ->
                                                       (fun v7 ->
                                                          let var = v7 in
                                                          (fun v16 ->
                                                             (fun v19 ->
                                                                (fun v20 ->
                                                                   (fun v18
                                                                    ->
                                                                    (fun v17
                                                                    ->
                                                                    (fun v15
                                                                    ->
                                                                    (fun v13
                                                                    ->
                                                                    (fun v14
                                                                    ->
                                                                    (observe
                                                                    v13 v14)
                                                                    (fun v8
                                                                    ->
                                                                    let (_unit_v33
                                                                    :
                                                                    unit expr)
                                                                    = v8 in
                                                                    (fun v9
                                                                    ->
                                                                    (fun v10
                                                                    ->
                                                                    (fun v11
                                                                    ->
                                                                    (fun v12
                                                                    ->
                                                                    muf_k
                                                                    (pair v9
                                                                    (pair v10
                                                                    (pair v11
                                                                    v12))))
                                                                    var2)
                                                                    var1) mu2)
                                                                    mu1))
                                                                    yobs)
                                                                    (gaussian
                                                                    v15))
                                                                    (v16,
                                                                    v17))
                                                                    (div v18))
                                                                    (v19,
                                                                    v20)) var)
                                                               (const 1.)) mu)
                                                         (ite v21 v22 v23))
                                                      var2) var1) switch)
                                             (ite v24 v25 v26)) mu2) mu1)
                                    switch) (value v27))) (bernoulli v29))
                      (const 0.3)) (split v30)) acc) (split v31)) acc)
       (split v32)) acc
let output out =
  let (mu1, out) = split out in
  let (mu2, out) = split out in
  let (var1, var2) = split out in
  let (_unit_v40 : unit expr) = Print.print_float (mean_float mu1) in
  let (_unit_v39 : unit expr) = Print.print_endline (const ()) in
  let (_unit_v38 : unit expr) = Print.print_float (mean_float mu2) in
  let (_unit_v37 : unit expr) = Print.print_endline (const ()) in
  let (_unit_v36 : unit expr) = Print.print_float (mean_float var1) in
  let (_unit_v35 : unit expr) = Print.print_endline (const ()) in
  let (_unit_v34 : unit expr) = Print.print_float (mean_float var2) in
  const ()
let main _ muf_k =
  (fun v72 ->
     (fun v74 ->
        (fun v73 ->
           (fun v71 ->
              (List.map v71)
                (fun v41 ->
                   let data = v41 in
                   (fun v69 ->
                      (fun v70 ->
                         (fun v68 ->
                            (fun v67 ->
                               (sample "mu1" v67)
                                 (fun v42 ->
                                    let mu1 = v42 in
                                    (fun v65 ->
                                       (fun v66 ->
                                          (fun v64 ->
                                             (fun v63 ->
                                                (sample "mu2" v63)
                                                  (fun v43 ->
                                                     let mu2 = v43 in
                                                     (fun v61 ->
                                                        (fun v62 ->
                                                           (fun v60 ->
                                                              (fun v59 ->
                                                                 (sample
                                                                    "var1"
                                                                    v59)
                                                                   (fun v44
                                                                    ->
                                                                    let var1
                                                                    = v44 in
                                                                    (fun v57
                                                                    ->
                                                                    (fun v58
                                                                    ->
                                                                    (fun v56
                                                                    ->
                                                                    (fun v55
                                                                    ->
                                                                    (sample
                                                                    "var2"
                                                                    v55)
                                                                    (fun v45
                                                                    ->
                                                                    let var2
                                                                    = v45 in
                                                                    (fun v48
                                                                    ->
                                                                    (fun v49
                                                                    ->
                                                                    (fun v51
                                                                    ->
                                                                    (fun v52
                                                                    ->
                                                                    (fun v53
                                                                    ->
                                                                    (fun v54
                                                                    ->
                                                                    (fun v50
                                                                    ->
                                                                    (fun v47
                                                                    ->
                                                                    (List.fold_resample
                                                                    v47)
                                                                    (fun v46
                                                                    ->
                                                                    let out =
                                                                    v46 in
                                                                    muf_k out))
                                                                    (v48,
                                                                    v49, v50))
                                                                    (pair v51
                                                                    (pair v52
                                                                    (pair v53
                                                                    v54))))
                                                                    var2)
                                                                    var1) mu2)
                                                                    mu1) data)
                                                                    make_observations))
                                                                    (gamma
                                                                    v56))
                                                                    (v57,
                                                                    v58))
                                                                    (const 1.))
                                                                    (const 1.)))
                                                                (gamma v60))
                                                             (v61, v62))
                                                          (const 1.))
                                                       (const 1.)))
                                               (gaussian v64)) (v65, v66))
                                         (const 0.5)) (const (-2.75))))
                              (gaussian v68)) (v69, v70)) (const 0.5))
                     (const 2.75))) (v72, v73)) (read v74))
       (const "data/processed_data.csv")) preprocess_data
let post_main _ = 
  let () = Format.printf "==== OUTPUT ====\n" in
  let _ = infer 1 main (Some output) in
  let () = Format.printf "\n==== RUNTIME APPROXIMATION STATUS ====\n" in
  let () = Format.printf "%s\n" (pp_approx_status false) in ()
let _ =
  post_main ()
