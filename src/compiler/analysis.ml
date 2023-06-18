open Mufextern
(* Abstract *)
module RandomVar = struct
  type t = identifier

  let compare = compare
end

module RVSet = Set.Make (RandomVar)
module RVMap = Map.Make (RandomVar)

type abs_constant = 
| Cbool of bool
| Cint of int
| Cfloat of float
| Cstring of string
| Cunit
| Cunk (* TODO: Remove if not used *)

type abs_unop =
| Squared
| SquareRoot
| Exp

type abs_expr =
| Econst of abs_constant
| Etuple of abs_expr list
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
| Dunk

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

let string_of_ident : identifier -> string =
fun id ->
  match id.modul with
  | None -> id.name
  | Some m -> m ^ "." ^ id.name

let string_of_constant : abs_constant -> string =
fun c ->
  match c with
  | Cbool b -> string_of_bool b
  | Cint i -> string_of_int i
  | Cfloat f -> string_of_float f
  | Cstring s -> s
  | Cunit -> "()"
  | Cunk -> "Unknown"

let string_of_unop : abs_unop -> string =
fun op ->
  match op with
  | Squared -> "squared"
  | SquareRoot -> "sqrt"
  | Exp -> "exp"

let rec string_of_pattern : pattern -> string =
fun p ->
  match p with
  | Pid id -> string_of_ident id
  | Ptuple ps -> Printf.sprintf "(%s)" (String.concat ", " (List.map string_of_pattern ps))
  | Punit -> "()"
  | Pany -> "_"

let rec string_of_expr : abs_expr -> string =
fun e ->
  match e with
  | Econst c -> string_of_constant c
  | Etuple es -> Printf.sprintf "(%s)" (String.concat ", " (List.map string_of_expr es))
  | Eadd (e1, e2) -> Printf.sprintf "%s + %s" (string_of_expr e1) (string_of_expr e2)
  | Emul (e1, e2) -> Printf.sprintf "%s * %s" (string_of_expr e1) (string_of_expr e2)
  | Ediv (e1, e2) -> Printf.sprintf "%s / %s" (string_of_expr e1) (string_of_expr e2)
  | Eunop (op, e) -> Printf.sprintf "%s(%s)" (string_of_unop op) (string_of_expr e)
  | Eif (e1, e2, e3) -> Printf.sprintf "if %s then %s else %s" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
  | Eifeval (e1, e2, e3) -> Printf.sprintf "if %s then %s else %s" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
  | Elist es -> Printf.sprintf "[%s]" (String.concat ", " (List.map string_of_expr es))
  | Edistr d -> string_of_distribution d
  | Erandomvar rv -> string_of_ident rv
  | Eunk -> "Unknown"

and string_of_distribution : abs_distribution -> string =
fun d ->
  match d with
  | Dgaussian (e1, e2) ->
    Printf.sprintf "Gaussian(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  (* | DmvNormal of abs_expr * abs_expr *)
  | Dcategorical (e1, e2, e3) ->
    Printf.sprintf "Categorical(%s, %s, %s)" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
  | Dbeta (e1, e2) ->
    Printf.sprintf "Beta(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Dbernoulli e1 ->
    Printf.sprintf "Bernoulli(%s)" (string_of_expr e1)
  | Dbinomial (e1, e2) ->
    Printf.sprintf "Binomial(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Dbetabinomial (e1, e2, e3) ->
    Printf.sprintf "BetaBinomial(%s, %s, %s)" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
  | Dnegativebinomial (e1, e2) ->
    Printf.sprintf "NegativeBinomial(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Dexponential e1 ->
    Printf.sprintf "Exponential(%s)" (string_of_expr e1)
  | Dgamma (e1, e2) ->
    Printf.sprintf "Gamma(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Dpoisson e1 ->
    Printf.sprintf "Poisson(%s)" (string_of_expr e1)
  | Ddelta e1 ->
    Printf.sprintf "Delta(%s)" (string_of_expr e1)
  | Ddelta_sampled -> "Delta_sampled()"
  | Dunk -> "Unknown"

module VarMap = Map.Make (struct
  type t = identifier

  let compare = compare

end)

type ctx = abs_expr VarMap.t

let rec ctx_add : pattern -> abs_expr -> ctx -> ctx =
fun p e ctx ->
  match p, e with
  | Pid name, _ -> VarMap.add name e ctx
  | Ptuple [], Etuple [] | Ptuple [], Eunk -> ctx
  | Ptuple (p :: ps), Etuple (e :: es) ->
    ctx_add (Ptuple ps) (Etuple es) (ctx_add p e ctx)
  | Ptuple (p :: ps), Eunk ->
    ctx_add (Ptuple ps) Eunk (ctx_add p Eunk ctx)
  | Pany, _ -> ctx
  | Punit, _ -> ctx
  | _, _ -> 
    failwith 
      (Format.sprintf "ctx_add: %s and %s mismatch" 
        (string_of_pattern p) (string_of_expr e))

let ctx_find : identifier -> ctx -> abs_expr =
fun p ctx ->
  match VarMap.find_opt p ctx with
  | Some e -> e
  | None -> Eunk 
    (* failwith (Format.sprintf "ctx_find: %s not found" (string_of_ident p)) *)

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

  let verify : t -> t -> bool =
  fun ann inf ->
    match ann, inf with
    | Approx, Approx -> true
    | Exact, Exact -> true
    | Dynamic, _ -> true
    | _, _ -> false

  let to_string : t -> string =
  fun status ->
    match status with
    | Approx -> "Approx"
    | Exact -> "Exact"
    | Dynamic -> "Dynamic"
end

exception Approximation_Status_Error of RandomVar.t * ApproximationStatus.t * ApproximationStatus.t
exception Inference_Strategy_Error of string

let rec has_randomvar ctx e =
  match e with 
  | Erandomvar _ -> true
  | Eadd (e1, e2) | Emul (e1, e2)
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
  | Ddelta_sampled | Dunk -> false

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
  | Eunk | Econst _ | Erandomvar _ -> e
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
  | Ddelta_sampled | Dunk -> d

and join_expr e1 e2 =
  let e1 = eval_expr e1 in
  let e2 = eval_expr e2 in
  match e1, e2 with
  | Econst c1, Econst c2 ->
    let e = if c1 = c2 then Econst c1 else Eunk in
    e
  | Etuple es1, Etuple es2 ->
    let es = List.map2 join_expr es1 es2 in
    Etuple es
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

exception NonConjugate of RandomVar.t

module SymState = struct
  type t = abs_distribution RVMap.t
  (* Abstract Semi-symbolic inference operators *)

  (* Helper functions *)
  let join : t -> t -> t =
    fun g1 g2 ->
      RVMap.merge (fun _ d1 d2 ->
        match d1, d2 with
        | Some d1, Some d2 -> Some (join_distribution d1 d2)
        | Some d1, None -> Some d1
        | None, Some d2 -> Some d2
        | None, None -> None
      ) g1 g2

  let empty : t =
    RVMap.empty

  let assume : RandomVar.t -> abs_expr -> t -> t =
    fun rv e g ->
      match e with
      | Edistr d -> RVMap.add rv d g
      | _ -> failwith "SymState.add: Not a distribution"

  let assume_patt : pattern -> abs_expr -> t -> t * abs_expr =
    fun patt es g ->
    let rec add_patt : pattern -> abs_expr -> t * RandomVar.t list -> t * RandomVar.t list =
      fun patt es (g, xs) ->
        match patt, es with
        | Pid id, es -> 
          begin match es with
          | Etuple _ -> failwith "SymState.add_patt: Cannot sample multiple distributions"
          | _ -> assume id es g, id :: xs
          end
        | Ptuple (p :: ps), Etuple (e :: es) ->
          add_patt (Ptuple ps) (Etuple es) (add_patt p e (g, xs))
        | Pany, _ -> g, xs
        | _, _ -> failwith "SymState.add_patt: Invalid sample expression"
    in
    let g, xs = add_patt patt es (g, []) in
    let xs = List.rev xs in
    let e = 
      match xs with
      | [] -> failwith "SymState.add_patt: Invalid sample expression"
      | [x] -> Erandomvar x
      | _ -> Etuple (List.map (fun x -> Erandomvar x) xs)
    in
    g, e

  let to_string : t -> string =
    fun g ->
      RVMap.fold (fun rv d acc ->
        Format.sprintf "%s%s: %s\n" acc (string_of_ident rv) (string_of_distribution d)) g ""
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

  let rec add_patt : pattern -> ApproximationStatus.t -> t -> t =
  fun patt a inf ->
    match patt with
    | Pid id -> add id a inf
    | Ptuple ps -> List.fold_left (fun inf p -> add_patt p a inf) inf ps
    | Punit -> inf
    | Pany -> inf

  let from_symstate : SymState.t -> t =
  fun g ->
    RVMap.map (fun d ->
      match d with
      | Ddelta_sampled -> ApproximationStatus.Approx
      | Dunk -> ApproximationStatus.Dynamic
      | _ -> ApproximationStatus.Exact
    ) g

  let to_string : t -> string =
  fun inf ->
    RVMap.fold (fun rv status acc ->
      Format.sprintf "%s%s: %s\n" acc (string_of_ident rv) (ApproximationStatus.to_string status)
    ) inf ""

  let verify : t -> t -> unit =
  fun ann inferred ->
    RVMap.iter (fun rv ann_status ->
      let inferred_status = RVMap.find rv inferred in
      (if not (ApproximationStatus.verify ann_status inferred_status) then
        raise (Approximation_Status_Error (rv, ann_status, inferred_status)));
    ) ann
end

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

(* Infers whether the program satisfies the annotated inference strategy
   output - string of the output function, which will be skipped in the analysis *)
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

  (* TODO: do abstract SSI *)
  let rec infer' : ctx -> SymState.t -> expr -> 
    ctx * SymState.t * abs_expr =
  fun ctx g e ->
    match e with
    | Econst c ->
      let c = 
        match c with
        | Cbool b -> Cbool b
        | Cint i -> Cint i
        | Cfloat f -> Cfloat (float_of_string f)
        | Cstring s -> Cstring s
        | Cunit -> Cunit
      in 
      ctx, g, Econst c
    | Eresample -> ctx, g, Econst Cunit
    | Evar x -> 
      ctx, g, ctx_find x ctx
    | Etuple es  | Epair es ->
      let ctx, g, es = 
        List.fold_left (fun (ctx, g, es) e ->
          let ctx, g, es' = infer' ctx g e in
          ctx, g, es @ [es']
        ) (ctx, g,[]) es in
      ctx, g, Etuple es
    | Elist es ->
      let ctx, g, es = 
        List.fold_left (fun (ctx, g, es) e ->
          let ctx, g, es' = infer' ctx g e in
          ctx, g, es @ [es']
        ) (ctx, g,[]) es in
      ctx, g, Elist es
    | Eapp (e1, e2) ->
      (* Assuming e1 is an identifier *)
      begin match e1 with
      | Evar _f ->
        let ctx, g, _es2 = infer' ctx g e2 in
        (* TODO: match on primitive names *)
        (* TODO: all custom functions are inlined, so there shouldn't be unidentified
          function calls *)
        (* TODO: return distribution and abstract expr from primitive function calls 
          (List/Array only) *)
        ctx, g, Eunk
      | _ -> failwith "infer: invalid function call"
      end
    | Eif (e1, e2, e3) ->
      let ctx, g, e1 = infer' ctx g e1 in
      let ctx, g, e2 = infer' ctx g e2 in
      let ctx, g, e3 = infer' ctx g e3 in
      (* Widen distribution *)
      if has_randomvar ctx e1 then
        ctx, g, Eif (e1, e2, e3)
      else
        let es23 = join_expr e2 e3 in
        let es = 
          match es23 with
          | Eunk -> Eif (e1, e2, e3)
          | _ -> es23
        in
        ctx, g, es
    | Eifeval (e1, e2, e3) ->
      let ctx, g, e1 = infer' ctx g e1 in
      let ctx, g, e2 = infer' ctx g e2 in
      let ctx, g, e3 = infer' ctx g e3 in
      (* Widen distribution *)
      if has_randomvar ctx e1 then
        ctx, g, Eifeval (e1, e2, e3)
      else
        let es23 = join_expr e2 e3 in
        let es = 
          match es23 with
          | Eunk -> Eifeval (e1, e2, e3)
          | _ -> es23
        in
        ctx, g, es
    | Elet (p, e1, e2) ->
      let ctx1, g, e1 = infer' ctx g e1 in
      let _, g, e2 = infer' (ctx_add p e1 ctx1) g e2 in
      ctx, g, e2
    | Esample (p, _, e1) ->
      let ctx, g, e1 = infer' ctx g e1 in
      let g, xs = SymState.assume_patt p e1 g in
      ctx, g, xs
    | Eobserve (e1, e2) ->
      (* TODO *)
      let ctx, g, _e1 = infer' ctx g e1 in
      let ctx, g, _ = infer' ctx g e2 in
      ctx, g, Econst Cunit
    | Evalue e1 ->
      (* TODO *)
      let ctx, g, _e1 = infer' ctx g e1 in
      ctx, g, Econst Cunk
    | Edistr d ->
      begin match d with
      | Dgaussian (e1, e2) ->
        let ctx, g, e1 = infer' ctx g e1 in
        let ctx, g, e2 = infer' ctx g e2 in
        ctx, g, Edistr (Dgaussian (e1, e2))
      | Dcategorical (e1, e2, e3) ->
        let ctx, g, e1 = infer' ctx g e1 in
        let ctx, g, e2 = infer' ctx g e2 in
        let ctx, g, e3 = infer' ctx g e3 in
        ctx, g, Edistr (Dcategorical (e1, e2, e3))
      | Duniformint (e1, e2) ->
        (* Uniform int is a wrapper for categorical *)
        let ctx, g, e1 = infer' ctx g e1 in
        let ctx, g, e2 = infer' ctx g e2 in
        (* TODO: if e1 and e2 are constant, we know what each probability is *)
        ctx, g, Edistr (Dcategorical (e1, e2, Eunk))
      | Dbeta (e1, e2) ->
        let ctx, g, e1 = infer' ctx g e1 in
        let ctx, g, e2 = infer' ctx g e2 in
        ctx, g, Edistr (Dbeta (e1, e2))
      | Dbernoulli e1 ->
        let ctx, g, e1 = infer' ctx g e1 in
        ctx, g, Edistr (Dbernoulli e1)
      | Dbinomial (e1, e2) ->
        let ctx, g, e1 = infer' ctx g e1 in
        let ctx, g, e2 = infer' ctx g e2 in
        ctx, g, Edistr (Dbinomial (e1, e2))
      | Dbetabinomial (e1, e2, e3) ->
        let ctx, g, e1 = infer' ctx g e1 in
        let ctx, g, e2 = infer' ctx g e2 in
        let ctx, g, e3 = infer' ctx g e3 in
        ctx, g, Edistr (Dbetabinomial (e1, e2, e3))
      | Dnegativebinomial (e1, e2) ->
        let ctx, g, e1 = infer' ctx g e1 in
        let ctx, g, e2 = infer' ctx g e2 in
        ctx, g, Edistr (Dnegativebinomial (e1, e2))
      | Dexponential e1 ->
        let ctx, g, e1 = infer' ctx g e1 in
        ctx, g, Edistr (Dexponential e1)
      | Dgamma (e1, e2) ->
        let ctx, g, e1 = infer' ctx g e1 in
        let ctx, g, e2 = infer' ctx g e2 in
        ctx, g, Edistr (Dgamma (e1, e2))
      | Dpoisson e1 ->
        let ctx, g, e1 = infer' ctx g e1 in
        ctx, g, Edistr (Dpoisson e1)
      | Ddelta e1 ->
        let ctx, g, e1 = infer' ctx g e1 in
        ctx, g, Edistr (Ddelta e1)
      end
    | Efun _ -> failwith "infer: fun is internal"
  in

  let ctx = VarMap.empty in
  let g = SymState.empty in

  (* TODO: inline functions *)
  (* TODO: list fold fixpoint *)

  let _, g', _ = infer' ctx g e in

  (* Reduce g' into inference strategy *)
  let inferred_inf = InferenceStrategy.from_symstate g' in

  let inferred_inf_s = InferenceStrategy.to_string inferred_inf in

  (* TODO: debug. delete later *)
  let sym_state_s = SymState.to_string g' in
  let inferred_inf_s = sym_state_s ^ "\n" ^ inferred_inf_s in

  try 
    InferenceStrategy.verify ann_inf inferred_inf;
    inferred_inf_s
  with Approximation_Status_Error (rv, ann, inf) ->
    let err = 
      Format.sprintf "`%s` is annotated with %s but expected to be %s\n" 
        (string_of_ident rv) (ApproximationStatus.to_string ann) (ApproximationStatus.to_string inf)
    in
    raise (Inference_Strategy_Error err)
  
