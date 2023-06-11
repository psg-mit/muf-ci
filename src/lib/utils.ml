(* open Semi_symbolic.Semi_symbolic_impl *)
(* module Distr_operations = Semi_symbolic.Distr_operations *)
open Semi_symbolic.Semi_symbolic_impl
module SSI = Semi_symbolic.Semi_symbolic_impl
module Distr_operations = Semi_symbolic.Distr_operations

type 'a distribution = 'a SSI.distribution
type 'a random_var = 'a SSI.random_var
type 'a expr = 'a SSI.expr

type 'a mdistr = 'a expr

type 'a prob = { id: int; particles: 'a particle Array.t }
and 'a particle = { value: 'a option; score: float; k: 'a next }
and 'a next = 'a prob -> 'a prob
and ('a, 'b) model = 'a -> ('b -> 'b next) -> 'b next

let to_owl_arr a = Owl.Arr.of_array a [| Array.length a |]

let run_next prob =
  if prob.id < Array.length prob.particles - 1 then
    let k = prob.particles.(prob.id + 1).k in
    k {prob with id = prob.id + 1}
  else
    prob

(* Turn an array of log-scores into an array of probabilities. *)
let normalize scores = 
  scores |> to_owl_arr
  |> (fun s -> Owl.Arr.(exp (s -$ log_sum_exp' s)))
  |> Owl.Arr.to_array

let finish v prob =
  let particle = prob.particles.(prob.id) in
  prob.particles.(prob.id) <- { particle with value = Some v };
  run_next prob

let copy : 'a. 'a -> 'a =
  fun x -> Marshal.from_bytes (Marshal.to_bytes x [Marshal.Closures]) 0

let to_distribution values scores =
  assert (Array.length values = Array.length scores);
  let probabilities = normalize scores in

  let distr, _ = Array.fold_left (fun (acc_d, i) value ->
    let prob = Array.get probabilities i in
    match acc_d with
    | [] -> ([value, prob], i + 1)
    | (v, p) :: acc' ->
      if value = v then
        ((v, p +. prob) :: acc', i + 1)
      else
        ((value, prob) :: acc', i + 1)
    ) ([], 0) values 
  in

  match distr with
  | [] -> failwith "Empty distribution"
  | [(v, _)] -> v
  | _ -> 
    let rv = { name = "res"; distr = mixture distr } in 
    ExRand rv

let rec get_marginal_expr : type a. a expr -> a mdistr = 
fun e ->
  let e = eval e in
  match e with
  | ExConst c -> ExConst c
  | ExRand rv -> 
    let rec marginalize _ =
      try hoist_and_eval rv
      with NonConjugate rv_nc ->
        (* (Printf.printf "NonConjugate %s\n" rv_nc.name); *)
        let _ = SSI.value rv_nc in
        marginalize ()
    in
    marginalize ();
    (* rv should now be marginal *)
    ExRand rv
  | ExAdd (e1, e2) -> ExAdd (get_marginal_expr e1, get_marginal_expr e2)
  | ExMul (e1, e2) -> ExMul (get_marginal_expr e1, get_marginal_expr e2)
  | ExDiv (e1, e2) -> ExDiv (get_marginal_expr e1, get_marginal_expr e2)
  | ExList l -> ExList (List.map get_marginal_expr l)
  | ExPair (e1, e2) -> ExPair (get_marginal_expr e1, get_marginal_expr e2)
  | ExArray a -> ExArray (Array.map get_marginal_expr a)
  | _ -> ExConst (eval_sample e)

let get_const e =
  match e with
  | ExConst v -> v
  | _ -> raise (InternalError "not a constant")

let split p = 
  match p with
  | ExPair(a, b) -> (a, b)
  | ExConst((a, b)) -> (ExConst a, ExConst b)
  | _ -> raise (InternalError "not a pair")

let get_array a =
  match a with
  | ExArray a -> a
  | ExConst l -> Array.map ((fun x -> ExConst x)) l
  | _ -> raise (InternalError "not an array")

let get_lst l =
  match l with
  | ExList l -> l
  | ExConst l -> List.map ((fun x -> ExConst x)) l
  | _ -> raise (InternalError "not a list")

let mean_float_d : float distribution -> float =
  fun d ->
    begin match d with
    | Normal (ExConst(mu), ExConst(_)) -> mu
    | Beta (ExConst(a), ExConst(b)) -> Distr_operations.beta_mean a b
    | Gamma (ExConst(a), ExConst(b)) -> Distr_operations.gamma_mean a b
    | Delta (ExConst v) -> v
    | Sampler _ -> raise (InternalError "not implemented")
    | _ -> raise (InternalError "not marginal")
    end
    
  let mean_int_d : int distribution -> float =
  fun d ->
    match d with
    | Binomial (ExConst(n), ExConst(p)) -> Distr_operations.binomial_mean n p
    | BetaBinomial (ExConst(n), ExConst(a), ExConst(b)) ->
      Distr_operations.beta_binomial_mean n a b
    | NegativeBinomial (ExConst(n), ExConst(p)) ->
      Distr_operations.negative_binomial_mean n p
    | Poisson (ExConst(l)) -> Distr_operations.poisson_mean l
    | Delta (ExConst v) -> float_of_int v
    | Sampler _ | Categorical _ -> raise (InternalError "not implemented")
    | _ -> raise (InternalError "not marginal")
  
  let mean_bool_d : bool distribution -> float =
  fun d ->
    match d with
    | Bernoulli (ExConst p) -> Distr_operations.bernoulli_mean p
    | Delta (ExConst v) -> if v then 1. else 0.
    | Sampler _ -> raise (InternalError "not implemented")
    | _ -> raise (InternalError "not marginal")
