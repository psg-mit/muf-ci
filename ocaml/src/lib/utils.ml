open Sirenlib.SSI

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
  |> (fun s -> Owl.Arr.(exp (s -$ max'(s))))
  |> (fun s -> Owl.Arr.(s /$ sum' s))
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
        ((value, prob) :: acc_d, i + 1)
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
        let _ = value_no_record rv_nc in
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

let is_const e =
  let e = eval e in
  match e with
  | ExConst _ -> true
  | _ -> false

let get_const e =
  let e = eval e in
  match e with
  | ExConst v -> v
  | _ -> raise (InternalError "not a constant")

let rec split p = 
  let p = eval p in
  match p with
  | ExPair(a, b) -> (a, b)
  | ExConst((a, b)) -> (ExConst a, ExConst b)
  | ExRand {distr = Mixture l; name} ->
    let s1, s2 =
      List.fold_left (fun (acc1, acc2) (d, p) ->
        let d1, d2 = split d in
        ((d1, p)::acc1), ((d2, p)::acc2)
      ) ([], []) l
    in
    (ExRand {distr = Mixture s1; name = name ^ "split1"}, ExRand {distr = Mixture s2; name = name ^ "split2"})
  | _ -> raise (InternalError "not a pair")

let get_array a =
  let a = eval a in
  match a with
  | ExArray a -> a
  | ExConst l -> Array.map ((fun x -> ExConst x)) l
  | _ -> raise (InternalError "not an array")

let rec lst_map2 f1 f2 f12 l1 l2 =
  match l1, l2 with
  | l1, [] -> List.map f1 l1
  | [], l2 -> List.map f2 l2
  | x1::l1, x2::l2 -> f12 x1 x2 :: (lst_map2 f1 f2 f12 l1 l2)

let rec get_lst l =
  let l = eval l in
  match l with
  | ExList l -> l
  | ExConst l -> List.map ((fun x -> ExConst x)) l
  | ExRand {distr = Mixture l; name} ->
    let l =
      List.fold_left (fun accs (d, w) ->
        let l = get_lst d in
        lst_map2 (fun acc -> acc) (fun d -> [(d, w)]) (fun acc d -> (d, w)::acc) accs l
      ) [] l
    in
    List.mapi (fun i l -> ExRand {distr = Mixture l; name = name ^ (string_of_int i)}) l
  | _ -> raise (InternalError "not a list")
