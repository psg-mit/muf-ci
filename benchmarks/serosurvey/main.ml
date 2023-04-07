 open Muf 

(* simulation (discrete) function *)
let main =
  let mem = ref (Muflib.init Serosurvey_approx.main) in
  (fun x -> let _, s = Muflib.step !mem x in mem := s);;
(* (discrete) simulation loop *)
while true do main () done;
exit(1);;
