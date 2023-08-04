module Distr_operations = struct
  include Semi_symbolic.Distr_operations
end

module SSI = struct
  open Owl
  include Semi_symbolic.Semi_symbolic_impl

  type approx_status = 
  | Exact of int
  | Approx of int

  let rv_approx_status : (string, approx_status * approx_status) Hashtbl.t= Hashtbl.create 100

  let record_new_rv var =
    match Hashtbl.find_opt rv_approx_status var with
    | None -> Hashtbl.add rv_approx_status var (Exact 0, Approx 0)
    | Some _ -> ()

  let record_approx_status var status =
    match Hashtbl.find_opt rv_approx_status var with
    | None -> 
      begin match status with
      | Exact _ -> Hashtbl.add rv_approx_status var (Exact 1, Approx 0)
      | Approx _ -> Hashtbl.add rv_approx_status var (Exact 0, Approx 1)
      end
    | Some (Exact(e), Approx(a)) ->
      begin match status with
      | Exact _ -> 
        Hashtbl.replace rv_approx_status var (Exact (e + 1), Approx(a))
      | Approx _ -> 
        Hashtbl.replace rv_approx_status var (Exact(e), Approx (a + 1))
      end
    | _ -> failwith "Approx status error"

  let pp_approx_status : bool -> string =
    fun obs ->
      Hashtbl.fold (fun key value acc -> (key, value)::acc) rv_approx_status []
      |> List.sort compare
      |> List.fold_left (fun acc (var, status) ->
        match status with
        | Exact(e), Approx(a) ->
          if not obs && String.starts_with ~prefix: "obs" var then acc
          else if not (e = 0) && not (a = 0) then
            Format.sprintf "%s%s: DYNAMIC (e-%d,a-%d)\n" acc var e a
          else if not (a = 0) then
            Format.sprintf "%s%s: APPROX (%d)\n" acc var a
          else
            Format.sprintf "%s%s: EXACT (%d)\n" acc var e
        | _ -> failwith "Approx status error") ""

  let sample n d =
    record_new_rv n;
    
    let rv = {
      name = n;
      distr = d
    } in
    (*(Printf.printf "Sampling %s from %s\n" n (string_of_rvset (get_parents rv)));*)
    ExRand rv

  let rec draw : type a. a random_var -> unit -> a =
  fun rv ->
    record_approx_status rv.name (Approx 1);
    hoist_and_eval rv;
    fun _ ->
      match rv.distr with
      | Normal (ExConst(mu), ExConst(var)) -> Distr_operations.gaussian_draw mu var
      | Beta (ExConst(a), ExConst(b)) -> Distr_operations.beta_draw a b
      | Categorical (d, ExConst a) ->
        Distr_operations.categorical_draw
        (List.combine (List.init (d.upper - d.lower + 1) (fun i -> i + d.lower))
        (Array.to_list (fst a)))
      | Bernoulli(ExConst(p)) -> Distr_operations.bernoulli_draw p
      | Binomial(ExConst(n), ExConst(p)) -> Distr_operations.binomial_draw n p
      | BetaBinomial(ExConst(n), ExConst(a), ExConst(b)) -> Distr_operations.beta_binomial_draw n a b
      | NegativeBinomial(ExConst(n), ExConst(p)) -> Distr_operations.negative_binomial_draw n p
      | Gamma(ExConst(a), ExConst(b)) -> 
        if a = 1. then Distr_operations.exponential_draw b
        else Distr_operations.gamma_draw a b
      | Poisson(ExConst(l)) -> Distr_operations.poisson_draw l
      | StudentT(ExConst(mu), ExConst(tau2), ExConst(nu)) -> Distr_operations.student_t_draw mu tau2 nu
      | Delta (ExConst v) -> v
      | Sampler (f, _) -> f ()
      | MvNormal (ExConst mu, ExConst var) -> Distr_operations.mv_gaussian_draw mu var
      | Mixture l ->
        (* Check for cyclic dependency *)
        let cyclic = List.fold_left (fun cyclic (e, _) -> cyclic || (depends_on e rv true)) false l in
        if cyclic then raise (InternalError ("Cyclic dependency in mixture distribution"));

        let sample = Random.float 1.0 in
        let rec draw' sum l =
          match l with
          | [] -> raise (InternalError ("Invalid mixture distribution"))
          | (e, p) :: rest ->
            let sum = sum +. p in
            if sample <= sum then eval_sample e else draw' sum rest
        in
        draw' 0. l
      | _ -> raise (InternalError ("Draw did not properly hoist and evaluate random variable"))
  and value : type a. a random_var -> a =
  fun rv ->
    (* (Printf.printf "approxing %s\n" rv.name); *)
    let rec do_value _ =
      try draw rv ()
      with NonConjugate rv_nc ->
        (* (Printf.printf "NonConjugate %s\n" rv_nc.name); *)
        let _ = value rv_nc in
        do_value ()
    in
    let new_val = do_value () in
    intervene rv new_val;
    new_val

  and eval_sample : type a. a expr -> a =
  fun e ->
    begin match e with
    | ExConst c -> c
    | ExVar _ -> assert false
    | ExRand rv -> value rv
    | ExAdd (e1, e2) -> eval_sample e1 +. eval_sample e2
    | ExMul (e1, e2) -> eval_sample e1 *. eval_sample e2
    | ExDiv (e1, e2) -> eval_sample e1 /. eval_sample e2
    | ExIntAdd (e1, e2) -> eval_sample e1 + eval_sample e2
    | ExIntMul (e1, e2) -> eval_sample e1 * eval_sample e2
    | ExCmp (Eq, e1, e2) -> eval_sample e1 = eval_sample e2
    | ExCmp (Lt, e1, e2) -> eval_sample e1 < eval_sample e2
    | ExPair (e1, e2) -> (eval_sample e1, eval_sample e2)
    | ExArray a -> Array.map eval_sample a
    | ExMatrix m -> Array.map (Array.map eval_sample) m
    | ExList l -> List.map eval_sample l
    | ExIte (i, t, e) -> if (eval_sample i) then (eval_sample t) else (eval_sample e)
    | ExUnop (Squared, e_inner) -> (eval_sample e_inner) ** 2.
    | ExUnop (SquareRoot, e_inner) -> Float.sqrt (eval_sample e_inner)
    | ExUnop (Exp, e_inner) -> Float.exp (eval_sample e_inner)
    | ExIntToFloat e_inner -> float_of_int (eval_sample e_inner)
    | ExMatAdd (e1, e2) -> Mat.add (eval_sample e1) (eval_sample e2)
    | ExMatSub (e1, e2) -> Mat.sub (eval_sample e1) (eval_sample e2)
    | ExMatMul (e1, e2) -> Mat.mul (eval_sample e1) (eval_sample e2)
    | ExMatTrans e -> Mat.transpose (eval_sample e)
    | ExMatInv e -> Linalg.D.inv (eval_sample e)
    | ExMatScalarMul (s, e) -> Mat.scalar_mul (eval_sample s) (eval_sample e)
    | ExMatGet (e, i) -> Mat.get (eval_sample e) i 0
    | ExMatSingle(e) -> Mat.of_arrays [| [| (eval_sample e) |] |]
    | ExFactor (v, d, e) ->
      let range = Array.init (d.upper - d.lower + 1) (fun i -> i) in
      (Array.map (fun i -> eval_sample (subst e v (ExConst (i + d.lower)))) range, d)
    | ExSum (v, d, e) ->
      let range = List.init (d.upper - d.lower + 1) (fun i -> i) in
      List.fold_left (fun sum i -> sum +. eval_sample (subst e v (ExConst (i + d.lower)))) 0. range
    | ExGet (e1, e2) -> let e, d = eval_sample e1 in e.(eval_sample e2 - d.lower)
    | ExLet (v, e1, e2) -> let c = eval_sample e1 in eval_sample (subst e2 v (ExConst c))
    end

  let observe_inner : type a. float -> a random_var -> a -> float =
  fun w rv x ->
    (* (Printf.printf "observing %s\n" rv.name); *)
    let rec do_observe _ =
      try score rv x
      with NonConjugate rv_nc ->
        (* (Printf.printf "NonConjugate %s\n" rv_nc.name); *)
        let _ = value rv_nc in
        do_observe ()
    in
    let s = do_observe () in
    intervene rv x;
    w +. s
  
  let observe : type a. float -> a distribution -> a -> float =
  fun w distr x ->
    let new_rv =
      match sample ("obs" ^ (string_of_int (get_obsnum ()))) distr with
      | ExRand rv -> rv
      | _ -> raise (InternalError("Sample always returns a random variable"))
    in
    observe_inner w new_rv x

  let rec draw_no_record : type a. a random_var -> unit -> a =
  fun rv ->
    hoist_and_eval rv;
    fun _ ->
      match rv.distr with
      | Normal (ExConst(mu), ExConst(var)) -> Distr_operations.gaussian_draw mu var
      | Beta (ExConst(a), ExConst(b)) -> Distr_operations.beta_draw a b
      | Categorical (d, ExConst a) ->
        Distr_operations.categorical_draw
        (List.combine (List.init (d.upper - d.lower + 1) (fun i -> i + d.lower))
        (Array.to_list (fst a)))
      | Bernoulli(ExConst(p)) -> Distr_operations.bernoulli_draw p
      | Binomial(ExConst(n), ExConst(p)) -> Distr_operations.binomial_draw n p
      | BetaBinomial(ExConst(n), ExConst(a), ExConst(b)) -> Distr_operations.beta_binomial_draw n a b
      | NegativeBinomial(ExConst(n), ExConst(p)) -> Distr_operations.negative_binomial_draw n p
      | Gamma(ExConst(a), ExConst(b)) -> 
        if a = 1. then Distr_operations.exponential_draw b
        else Distr_operations.gamma_draw a b
      | Poisson(ExConst(l)) -> Distr_operations.poisson_draw l
      | StudentT(ExConst(mu), ExConst(tau2), ExConst(nu)) -> Distr_operations.student_t_draw mu tau2 nu
      | Delta (ExConst v) -> v
      | Sampler (f, _) -> f ()
      | MvNormal (ExConst mu, ExConst var) -> Distr_operations.mv_gaussian_draw mu var
      | Mixture l ->
        (* Check for cyclic dependency *)
        let cyclic = List.fold_left (fun cyclic (e, _) -> cyclic || (depends_on e rv true)) false l in
        if cyclic then raise (InternalError ("Cyclic dependency in mixture distribution"));

        let sample = Random.float 1.0 in
        let rec draw' sum l =
          match l with
          | [] -> raise (InternalError ("Invalid mixture distribution"))
          | (e, p) :: rest ->
            let sum = sum +. p in
            if sample <= sum then eval_sample e else draw' sum rest
        in
        draw' 0. l
      | _ -> raise (InternalError ("Draw did not properly hoist and evaluate random variable"))
      
  and value_no_record : type a. a random_var -> a =
  fun rv ->
    (* (Printf.printf "approxing %s\n" rv.name); *)
    let rec do_value _ =
      try draw_no_record rv ()
      with NonConjugate rv_nc ->
        (* (Printf.printf "NonConjugate %s\n" rv_nc.name); *)
        let _ = value_no_record rv_nc in
        do_value ()
    in
    let new_val = do_value () in
    intervene rv new_val;
    new_val

end