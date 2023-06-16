open Mufextern
(* Abstract *)
module RandomVar = struct
  type t = String.t

  let compare = compare
end

module RVSet = Set.Make (RandomVar)
module RVMap = Map.Make (RandomVar)

type abs_identifier = Mufextern.identifier
type abs_constant = 
| Cbool of bool
| Cint of int
| Cfloat of float
| Cstring of string
| Cunit

type abs_pattern = Mufextern.pattern
type abs_unop =
| Squared
| SquareRoot
| Exp

type abs_expr =
| Econst of abs_constant
| Evar of abs_identifier
| Etuple of abs_expr list
| Eapp of abs_expr * abs_expr
(* ops *)
| Eadd of abs_expr * abs_expr
| Emul of abs_expr * abs_expr
| Ediv of abs_expr * abs_expr
| Eunop of abs_unop * abs_expr
| Eif of abs_expr * abs_expr * abs_expr
| Eifeval of abs_expr * abs_expr * abs_expr
| Elist of abs_expr list
(* new *)
| Edistr of abs_distribution
| Erandomvar of RandomVar.t
| Eunk

and abs_distribution =
| Dgaussian of abs_expr * abs_expr
(* | DmvNormal of abs_expr * abs_expr *)
| Dcategorical of abs_expr * abs_expr * abs_expr
| Dbeta of abs_expr * abs_expr
| Dbernoulli of abs_expr
| Dbinomial of abs_expr * abs_expr
| Dbetabinomial of abs_expr * abs_expr * abs_expr
| Dnegativebinomial of abs_expr * abs_expr
| Dexponential of abs_expr
| Dgamma of abs_expr * abs_expr
| Dpoisson of abs_expr
| Ddelta of abs_expr
| Ddelta_sampled 
| Ddelta_observed
| Dunk

module VarMap = Map.Make (struct
  type t = identifier

  let compare = compare
  
end)

type ctx = abs_expr VarMap.t

let rv_n = ref 0
let get_rv x =
  rv_n := !rv_n + 1;
  x ^ (string_of_int (!rv_n))

let used_rv_names : RandomVar.t list ref = ref []

let v_n = ref 0
let get_v () =
  v_n := !v_n + 1;
  "v" ^ (string_of_int (!v_n))

let get_obs () =
  v_n := !v_n + 1;
  "obs" ^ (string_of_int (!v_n))

let temp_var () : identifier = 
  {modul=None; name=get_v ()}

exception Approximation_Status_Error of string

exception NonConjugate of RandomVar.t

module ApproximationStatus = struct
  type t = 
  | Approx
  | Exact
  | Dynamic

  let join : t -> t -> t =
  fun s1 s2 ->
    match s1, s2 with
    | Approx, Approx -> Approx
    | Exact, Exact -> Exact
    | _ -> Dynamic
end

module InferenceStrategy = struct
  type t = ApproximationStatus.t RVMap.t

  let join : t -> t -> t =
  fun s1 s2 ->
    RVMap.merge (fun _ s1 s2 ->
      match s1, s2 with
      | Some s1, Some s2 -> Some (ApproximationStatus.join s1 s2)
      | Some s1, None -> Some s1
      | None, Some s2 -> Some s2
      | None, None -> None
    ) s1 s2

  let empty = RVMap.empty

  let add : RandomVar.t -> ApproximationStatus.t -> t -> t =
  fun rv a inf ->
    RVMap.add rv a inf

  let rec add_patt : abs_pattern -> ApproximationStatus.t -> t -> t =
  fun patt a inf ->
    match patt with
    | Pid id -> 
      begin match id with
      | {modul=None; name=x} -> add x a inf
      | _ -> failwith "add_patt: not implemented"
      end
    | Ptuple ps -> List.fold_left (fun inf p -> add_patt p a inf) inf ps
    | Punit -> inf
    | Pany -> inf
end

let rec has_randomvar ctx e =
  match e with 
  | Erandomvar _ -> true
  | Evar x -> has_randomvar ctx (VarMap.find x ctx)
  | Eapp (e1, e2) | Eadd (e1, e2) | Emul (e1, e2)
  | Ediv (e1, e2) -> 
    (has_randomvar ctx e1) || (has_randomvar ctx e2)
  | Eunop (_, e1) -> has_randomvar ctx e1
  | Eif (e1, e2, e3) | Eifeval (e1, e2, e3) -> 
    (has_randomvar ctx e1) || (has_randomvar ctx e2) || (has_randomvar ctx e3)
  | Elist es | Etuple es ->
    List.exists (has_randomvar ctx) es
  | Edistr d -> has_randomvar_distr ctx d
  | Econst _ | Eunk -> false
and has_randomvar_distr ctx d =
  match d with
  | Dgaussian (e1, e2) | Dbeta (e1, e2) | Dbinomial (e1, e2)
  | Dnegativebinomial (e1, e2) | Dgamma (e1, e2) -> 
    has_randomvar ctx e1 || has_randomvar ctx e2
  (* | DmvNormal (e1, e2) -> has_randomvar ctx e1 || has_randomvar ctx e2 *)
  | Dcategorical (e1, e2, e3) | Dbetabinomial (e1, e2, e3) -> 
    has_randomvar ctx e1 || has_randomvar ctx e2 || has_randomvar ctx e3
  | Dbernoulli e1 | Dexponential e1 
  | Dpoisson e1 | Ddelta e1 -> has_randomvar ctx e1
  | Ddelta_sampled | Ddelta_observed | Dunk -> false

let rec eval_add e1 e2 =
  match e1, e2 with
  | Econst (Cfloat c1), Econst (Cfloat c2) -> Econst (Cfloat (c1 +. c2))
  | Econst (Cfloat c1), Eadd(Econst (Cfloat c2), e3) -> 
    eval_add (Econst (Cfloat (c1 +. c2))) e3
  | Econst (Cfloat c1), Eadd(e2, Econst (Cfloat c3)) ->
    eval_add (Econst (Cfloat (c1 +. c3))) e2
  | Eadd(Econst (Cfloat c1), e2), e3 -> 
    eval_add (Econst (Cfloat c1)) (eval_add e2 e3)
  | _ -> Eadd (e1, e2)

let rec eval_mul e1 e2 =
  match e1, e2 with
  | Econst (Cfloat c1), Econst (Cfloat c2) -> Econst (Cfloat (c1 *. c2))
  | Econst (Cfloat c1), Emul(Econst (Cfloat c2), e3) -> 
    eval_mul (Econst (Cfloat (c1 *. c2))) e3
  | Econst (Cfloat c1), Emul(e2, Econst (Cfloat c3)) ->
    eval_mul (Econst (Cfloat (c1 *. c3))) e2
  | Econst (Cfloat c1), Eadd(Econst (Cfloat c2), e3) -> 
    eval_add (Econst (Cfloat (c1 *. c2))) (eval_mul (Econst (Cfloat c1)) e3)
  | _ -> Emul (e1, e2)

let eval_div e1 e2 =
  match e1, e2 with
  | Econst (Cfloat c1), Econst (Cfloat c2) -> Econst (Cfloat (c1 /. c2))
  | _ -> Ediv (e1, e2)

let eval_unop op e =
  match op, e with
  | Squared, Econst (Cfloat c) -> Econst (Cfloat (c ** 2.))
  | SquareRoot, Econst (Cfloat c) -> Econst (Cfloat (Float.sqrt c))
  | Exp, Econst (Cfloat c) -> Econst (Cfloat (Float.exp c))
  | _ -> Eunop (op, e)

let rec eval_expr : abs_expr -> abs_expr =
fun e ->
  match e with
  | Etuple es ->
    Etuple (List.map eval_expr es)
  | Eapp (e1, e2) -> 
    let e1 = eval_expr e1 in
    let e2 = eval_expr e2 in
    Eapp (e1, e2)
  | Eadd (e1, e2) ->
    let e1 = eval_expr e1 in
    let e2 = eval_expr e2 in
    eval_add e1 e2
  | Emul (e1, e2) ->
    let e1 = eval_expr e1 in
    let e2 = eval_expr e2 in
    eval_mul e1 e2
  | Ediv (e1, e2) ->
    let e1 = eval_expr e1 in
    let e2 = eval_expr e2 in
    eval_div e1 e2
  | Eunop (op, e1) ->
    let e1 = eval_expr e1 in
    eval_unop op e1
  | Eif (e1, e2, e3) ->
    let e1 = eval_expr e1 in
    let e2 = eval_expr e2 in
    let e3 = eval_expr e3 in
    begin match e1 with
    | Econst (Cbool true) -> e2
    | Econst (Cbool false) -> e3
    | _ -> Eif (e1, e2, e3)
    end
  | Eifeval (e1, e2, e3) ->
    let e1 = eval_expr e1 in
    let e2 = eval_expr e2 in
    let e3 = eval_expr e3 in
    begin match e1 with
    | Econst (Cbool true) -> e2
    | Econst (Cbool false) -> e3
    | _ -> Eifeval (e1, e2, e3)
    end
  | Elist es ->
    let es = List.map eval_expr es in
    Elist es
  | Edistr d -> Edistr (eval_distribution d)
  | Eunk
  | Econst _ | Evar _
  | Erandomvar _ -> e
and eval_distribution : abs_distribution -> abs_distribution =
fun d ->
  match d with
  | Dgaussian (e1, e2) ->
    let e1 = eval_expr e1 in
    let e2 = eval_expr e2 in
    Dgaussian (e1, e2)
  (* | DmvNormal of expr * expr *)
  | Dcategorical (e1, e2, e3) -> 
    let e1 = eval_expr e1 in
    let e2 = eval_expr e2 in
    let e3 = eval_expr e3 in
    Dcategorical (e1, e2, e3)
  | Dbeta (e1, e2) ->
    let e1 = eval_expr e1 in
    let e2 = eval_expr e2 in
    Dbeta (e1, e2)
  | Dbernoulli e ->
    let e = eval_expr e in
    Dbernoulli e
  | Dbinomial (e1, e2) -> 
    let e1 = eval_expr e1 in
    let e2 = eval_expr e2 in
    Dbinomial (e1, e2)
  | Dbetabinomial (e1, e2, e3) -> 
    let e1 = eval_expr e1 in
    let e2 = eval_expr e2 in
    let e3 = eval_expr e3 in
    Dbetabinomial (e1, e2, e3)
  | Dnegativebinomial (e1, e2) -> 
    let e1 = eval_expr e1 in
    let e2 = eval_expr e2 in
    Dnegativebinomial (e1, e2)
  | Dexponential e -> 
    let e = eval_expr e in
    Dexponential e
  | Dgamma (e1, e2) -> 
    let e1 = eval_expr e1 in
    let e2 = eval_expr e2 in
    Dgamma (e1, e2)
  | Dpoisson e -> 
    let e = eval_expr e in
    Dpoisson e
  | Ddelta e ->
    let e = eval_expr e in
    Ddelta e
  | Ddelta_sampled | Ddelta_observed | Dunk -> d

and join_expr e1 e2 =
  let e1 = eval_expr e1 in
  let e2 = eval_expr e2 in
  match e1, e2 with
  | Econst c1, Econst c2 ->
    let e = if c1 = c2 then Econst c1 else Eunk in
    e
  | Evar v1, Evar v2 ->
    if v1 = v2 then Evar v1 else Eunk
  | Etuple es1, Etuple es2 ->
    let es = List.map2 join_expr es1 es2 in
    Etuple es
  | Eapp (e11, e12), Eapp (e21, es22) ->
    if e11 = e21 then
      Eapp (e11, join_expr e12 es22)
    else
      Eunk
  | Eadd (e11, e12), Eadd (e21, e22) ->
    Eadd (join_expr e11 e21, join_expr e12 e22)
  | Emul (e11, e12), Emul (e21, e22) ->
    Emul (join_expr e11 e21, join_expr e12 e22)
  | Ediv (e11, e12), Ediv (e21, e22) ->
    Ediv (join_expr e11 e21, join_expr e12 e22)
  | Eunop (u1, e1), Eunop (u2, e2) ->
    if u1 = u2 then Eunop (u1, join_expr e1 e2) else Eunk
  | Eif (e11, e12, e13), Eif (e21, e22, e23) ->
    Eif (join_expr e11 e21, join_expr e12 e22, join_expr e13 e23)
  | Eifeval (e11, e12, e13), Eifeval (e21, e22, e23) ->
    Eifeval (join_expr e11 e21, join_expr e12 e22, join_expr e13 e23)
  | Elist es1, Elist es2 ->
    let es = List.map2 join_expr es1 es2 in
    Elist es
  | Edistr d1, Edistr d2 ->
    Edistr (join_distribution d1 d2)
  | Erandomvar rv1, Erandomvar rv2 ->
    if rv1 = rv2 then Erandomvar rv1 else Eunk
    (* TODO: Widen: can merge them if distribution is the same *)
  | _ -> Eunk
and join_distribution : abs_distribution -> abs_distribution -> abs_distribution =
fun d1 d2 ->
  match d1, d2 with
  | Dgaussian (e1, e2), Dgaussian (e1', e2') -> 
    Dgaussian (join_expr e1 e1', join_expr e2 e2')
  | Dcategorical (e1, e2, e3), Dcategorical (e1', e2', e3') ->
    Dcategorical (join_expr e1 e1', join_expr e2 e2', join_expr e3 e3')
  | Dbeta (e1, e2), Dbeta (e1', e2') ->
    Dbeta (join_expr e1 e1', join_expr e2 e2')
  | Dbernoulli e1, Dbernoulli e2 ->
    Dbernoulli (join_expr e1 e2)
  | Dbinomial (e1, e2), Dbinomial (e1', e2') ->
    Dbinomial (join_expr e1 e1', join_expr e2 e2')
  | Dbetabinomial (e1, e2, e3), Dbetabinomial (e1', e2', e3') ->
    Dbetabinomial (join_expr e1 e1', join_expr e2 e2', join_expr e3 e3')
  | Dnegativebinomial (e1, e2), Dnegativebinomial (e1', e2') ->
    Dnegativebinomial (join_expr e1 e1', join_expr e2 e2')
  | Dexponential e1, Dexponential e2 ->
    Dexponential (join_expr e1 e2)
  | Dgamma (e1, e2), Dgamma (e1', e2') ->
    Dgamma (join_expr e1 e1', join_expr e2 e2')
  | Dpoisson e1, Dpoisson e2 ->
    Dpoisson (join_expr e1 e2)
  | Ddelta e1, Ddelta e2 ->
    Ddelta (join_expr e1 e2)
  | Ddelta_sampled, Ddelta_sampled -> Ddelta_sampled
  (* TODO: Can delta_s and delta be merged? *)
  (* | Ddelta_sampled e1, Ddelta e2 *)
  (* | Ddelta e1, Ddelta_sampled e2 *)
  | _ -> Dunk

let annotated_inference_strategy : Mufextern.program -> InferenceStrategy.t =
fun (decls, e) ->

  (* assumes each random var has unique name *)
  let rec annotated_inference_strategy' : InferenceStrategy.t -> expr -> InferenceStrategy.t =
  fun inf e ->
    match e with
    | Econst _ | Eresample | Evar _ | Edistr _ -> inf
    | Etuple es | Elist es | Epair es -> List.fold_left annotated_inference_strategy' inf es
    | Eapp (e1, e2) ->
      let inf = annotated_inference_strategy' inf e1 in
      let inf = annotated_inference_strategy' inf e2 in
      inf
    | Eif (e1, e2, e3) | Eifeval (e1, e2, e3) ->
      let inf = annotated_inference_strategy' inf e1 in
      let inf = annotated_inference_strategy' inf e2 in
      let inf = annotated_inference_strategy' inf e3 in
      inf
    | Elet (_, e1, e2) ->
      let inf = annotated_inference_strategy' inf e1 in
      let inf = annotated_inference_strategy' inf e2 in
      inf
    | Esample (p, a, e1) ->
      let inf = annotated_inference_strategy' inf e1 in
      let a = 
        match a with
        | Aapprox -> ApproximationStatus.Approx
        | Aexact -> ApproximationStatus.Exact
        | Adynamic -> ApproximationStatus.Dynamic
      in
      InferenceStrategy.add_patt p a inf
    | Eobserve (e1, e2) ->
      let inf = annotated_inference_strategy' inf e1 in
      let inf = annotated_inference_strategy' inf e2 in
      inf
    | Evalue e1 ->
      let inf = annotated_inference_strategy' inf e1 in
      inf
    | Efun _ -> failwith "annotated_inference_strategy: fun is internal"
  in

  let inf = 
    List.fold_left (fun inf d ->
      match d with
      | Ddecl _ | Dopen _ -> inf
      | Dfun (_, _, e) -> annotated_inference_strategy' inf e
    ) InferenceStrategy.empty decls
  in

  annotated_inference_strategy' inf e

let infer : string -> Mufextern.program -> string =
fun output p ->
  (* Remove output function from analysis *)
  let decls, e = p in
  let decls = List.filter (fun d ->
    match d with
    | Ddecl _ | Dopen _ -> false
    | Dfun (s, _, _) -> not (s = output)
  ) decls in

  let ann_inf = annotated_inference_strategy (decls, e) in
  RVMap.fold (fun rv status acc ->
    let status = 
      match status with
      | ApproximationStatus.Approx -> "Approx"
      | ApproximationStatus.Exact -> "Exact"
      | ApproximationStatus.Dynamic -> "Dynamic"
    in
    (Format.sprintf "%s%s: %s\n" acc rv status)
  ) ann_inf "" 
