open Probzelus
open Distribution
open Muf
open Infer_muf

let preprocess_data entry muf_k = (fun v1 -> (List.hd v1) muf_k) entry
let make_observations (acc, yobs) muf_k =
  (fun v31 ->
     (fun v2 ->
        let (mu1, acc) = v2 in
        (fun v30 ->
           (fun v3 ->
              let (mu2, acc) = v3 in
              (fun v29 ->
                 (fun v4 ->
                    let (var1, var2) = v4 in
                    (fun v28 ->
                       (fun v27 ->
                          (sample "make_observations_switch" v27)
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
                                                                (fun v18 ->
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
                                                                    let (_unit_v32
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
                                                                  (v19, v20))
                                                               var)
                                                            (const 1.)) mu)
                                                      (ite v21 v22 v23)) var2)
                                                var1) switch)
                                          (ite v24 v25 v26)) mu2) mu1) switch))
                         (bernoulli v28)) (const 0.3)) (split v29)) acc)
             (split v30)) acc) (split v31)) acc
let output out =
  let (mu1, out) = split out in
  let (mu2, out) = split out in
  let (var1, var2) = split out in
  let (_unit_v39 : unit expr) = Print.print_float (mean_float mu1) in
  let (_unit_v38 : unit expr) = Print.print_endline (const ()) in
  let (_unit_v37 : unit expr) = Print.print_float (mean_float mu2) in
  let (_unit_v36 : unit expr) = Print.print_endline (const ()) in
  let (_unit_v35 : unit expr) = Print.print_float (mean_float var1) in
  let (_unit_v34 : unit expr) = Print.print_endline (const ()) in
  let (_unit_v33 : unit expr) = Print.print_float (mean_float var2) in
  const ()
let main _ muf_k =
  (fun v72 ->
     (fun v74 ->
        (fun v73 ->
           (fun v71 ->
              (List.map v71)
                (fun v40 ->
                   let data = v40 in
                   (fun v69 ->
                      (fun v70 ->
                         (fun v68 ->
                            (fun v67 ->
                               (sample "mu1" v67)
                                 (fun v66 ->
                                    (fun v41 ->
                                       let mu1 = v41 in
                                       (fun v64 ->
                                          (fun v65 ->
                                             (fun v63 ->
                                                (fun v62 ->
                                                   (sample "mu2" v62)
                                                     (fun v42 ->
                                                        let mu2 = v42 in
                                                        (fun v60 ->
                                                           (fun v61 ->
                                                              (fun v59 ->
                                                                 (fun v58 ->
                                                                    (sample
                                                                    "var1"
                                                                    v58)
                                                                    (fun v43
                                                                    ->
                                                                    let var1
                                                                    = v43 in
                                                                    (fun v56
                                                                    ->
                                                                    (fun v57
                                                                    ->
                                                                    (fun v55
                                                                    ->
                                                                    (fun v54
                                                                    ->
                                                                    (sample
                                                                    "var2"
                                                                    v54)
                                                                    (fun v44
                                                                    ->
                                                                    let var2
                                                                    = v44 in
                                                                    (fun v47
                                                                    ->
                                                                    (fun v48
                                                                    ->
                                                                    (fun v50
                                                                    ->
                                                                    (fun v51
                                                                    ->
                                                                    (fun v52
                                                                    ->
                                                                    (fun v53
                                                                    ->
                                                                    (fun v49
                                                                    ->
                                                                    (fun v46
                                                                    ->
                                                                    (List.fold_resample
                                                                    v46)
                                                                    (fun v45
                                                                    ->
                                                                    let out =
                                                                    v45 in
                                                                    muf_k out))
                                                                    (v47,
                                                                    v48, v49))
                                                                    (pair v50
                                                                    (pair v51
                                                                    (pair v52
                                                                    v53))))
                                                                    var2)
                                                                    var1) mu2)
                                                                    mu1) data)
                                                                    make_observations))
                                                                    (gamma
                                                                    v55))
                                                                    (v56,
                                                                    v57))
                                                                    (const 1.))
                                                                    (const 1.)))
                                                                   (gamma v59))
                                                                (v60, v61))
                                                             (const 1.))
                                                          (const 1.)))
                                                  (gaussian v63)) (v64, v65))
                                            (const 0.5)) (const (-2.75)))
                                      (value v66))) (gaussian v68))
                           (v69, v70)) (const 0.5)) (const 2.75))) (v72, v73))
          (read v74)) (const "data/processed_data.csv")) preprocess_data
let post_main _ = 
  let () = Format.printf "==== OUTPUT ====\n" in
  let _ = infer 1 main (Some output) in
  let () = Format.printf "\n==== RUNTIME APPROXIMATION STATUS ====\n" in
  let () = Format.printf "%s\n" (pp_approx_status false) in ()
let _ =
  post_main ()
