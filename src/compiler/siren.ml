open Muf

let buckets = 50

module TypMap = Map.Make (struct
  type t = identifier

  let compare = compare
  
end)

module VarMap = Map.Make (struct
  type t = identifier

  let compare = compare
  
end)

module type Approximation_Status = sig
  type t

  (* Update an abstract state by assuming an RV *)
  (* val assume : Rep.scalar * t -> int -> t

  (* Update an abstract state by observing an RV with an expression typs *)
  val observe : Rep.scalar * t -> int -> t

  (* Update an abstract state by evaluating an expression typs *)
  val value : Rep.scalar * t -> t

  (* Compute an abstract state that is a conservative choice between two states *)
  val join : t -> t -> t *)
  
end

type dist_types = 
| Det
| Any
| Var of dist
| Dist of dist
| Pair of dist_types * dist_types
and dist =
| Beta
| Gaussian
| Bernoulli
| Delta
| Prob
| Binomial
[@@deriving show]

let pp_map ppf m =
  TypMap.iter (fun {name=k;_} v -> 
    Format.fprintf ppf "%s -> %s@\n" k (show_dist_types v)) m

let rec dist_type_join t1 t2 =
  match t1, t2 with
  | Det, Det -> Det
  | Det, _ -> t2
  | _, Det -> t1
  | Var(d1), Var(d2) -> 
    begin match d1, d2 with
    | Beta, Beta -> Var(Beta)
    | Gaussian, Gaussian -> Var(Gaussian)
    | Bernoulli, Bernoulli -> Var(Bernoulli)
    | Delta, Delta -> Var(Delta)
    | Binomial, Binomial -> Var(Binomial)
    | _ -> Var(Prob)
    end
  | Dist(d1), Dist(d2) -> 
    begin match d1, d2 with
    | Beta, Beta -> Dist(Beta)
    | Gaussian, Gaussian -> Dist(Gaussian)
    | Bernoulli, Bernoulli -> Dist(Bernoulli)
    | Delta, Delta -> Dist(Delta)
    | Binomial, Binomial -> Dist(Binomial)
    | _ -> Dist(Prob)
    end
  | Pair(t1, t2), Pair(t3, t4) -> 
    let t1 = dist_type_join t1 t3 in
    let t2 = dist_type_join t2 t4 in
    Pair(t1, t2)
  | _ -> Any

let approximation_status muf_list = 
  let _ = Hashtbl.create buckets in

  let get_dist t = 
    match t with
    | Tconstr("beta", []) -> Beta
    | Tconstr("gaussian", []) -> Gaussian
    | Tconstr("bernoulli", []) -> Bernoulli
    | Tconstr("delta", []) -> Delta
    | Tconstr("binomial", []) -> Binomial
    | _ -> failwith "Invalid distribution type"
  in

  let rec get_ty ty =
    match ty with
    | Tany -> Any
    | Tconstr("det", []) -> Det
    | Tconstr("dist", [t]) ->
      let d = get_dist t in
      Dist(d)
    | Tconstr("var", [t]) ->
      let d = get_dist t in
      Var(d)
    | Ttuple([]) -> Det
    | Ttuple(tys) ->
      let rec build acc tys =
        match tys with
        | [] -> acc
        | ty::tys -> build (Pair(acc, get_ty ty)) tys
      in
      build (get_ty (List.hd tys)) (List.tl tys)
    | Tconstr("array", _) -> failwith "Not implemented parameter type annotation"
    | Tconstr("list", _) -> failwith "Not implemented parameter type annotation"
    | Tconstr(_) | Tvar(_) -> failwith "Invalid parameter type annotation"
  in

  let rec get_param_typ typs p = 
    match p with
    | Pid(id) -> TypMap.add id Any typs
    | Ptype({patt=Pid(id); _}, ty) -> 
      TypMap.add id (get_ty ty) typs
    | Ptuple(patts) -> 
      List.fold_left (fun typ {patt=patt; _} -> get_param_typ typ patt) typs patts
    | Pany | Pconst(_) -> typs
    | _ -> failwith "Invalid parameter type annotation"
  in

  (* Assigns t1 to p *)
  let rec pattern typs {patt=p; _} t1 =
    match p with
    | Pid(id) -> TypMap.add id t1 typs
    | Ptype({patt=Pid(id); _}, ty) -> 
      let written_ty = get_ty ty in
      if written_ty = t1 then
        TypMap.add id t1 typs
      else
        failwith "Invalid pattern1"
    | Ptuple([]) -> typs
    | Ptuple(patts) -> 
      (* convert t1 to a list *)
      let rec convert t1 =
        match t1 with
        | Pair(t1, t2) -> List.append (convert t1) (convert t2)
        | _ -> [t1]
      in
      let t1 = convert t1 in
      let rec assign typs patts t1 =
        match patts, t1 with
        | [], [] -> typs
        | patt::patts, t1::t1s -> 
          let typs = pattern typs patt t1 in
          assign typs patts t1s
        | patt::patts, [] ->
          let typs = pattern typs patt Any in
          assign typs patts []
        | _ -> typs
      in
      assign typs patts t1
    | Pany | Pconst(_) -> typs
    | _ -> failwith "Invalid pattern3"
  in

  let rec app typs {expr=e1;_} e2 =
    match e1 with
    | Evar({name=name;_} as id) -> 
      begin match name with
      | "beta" -> Dist(Beta)
      | "gaussian" -> Dist(Gaussian)
      | "bernoulli" -> Dist(Bernoulli)
      | "delta" -> Dist(Delta)
      | "binomial" -> Dist(Binomial)
      | "exact" -> 
        let t = expression typs e2 in
        t
      | "approx" -> 
        let t = expression typs e2 in
        t
      | _ ->
        begin match TypMap.find_opt id typs with
        | Some(t) -> t
        | None -> Any
        end
      end
    | _ -> failwith "Unexpected expression"

  (* Returns type of e *)
  and expression typs {expr=e; _} =
    match e with
    | Econst _ -> Det
    | Evar (id) -> 
      begin match TypMap.find_opt id typs with
      | Some(t) -> t
      | None -> failwith "Variable not found1"
      end
    | Etuple ([]) -> Det
    | Etuple (exprs) -> 
      let rec build acc exprs =
        match exprs with
        | [] -> acc
        | e::exprs -> build (Pair(acc, expression typs e)) exprs
      in
      build (expression typs (List.hd exprs)) (List.tl exprs)
    | Eapp (e1, e2) -> 
      app typs e1 e2
    | Eif (_, e2, e3) ->
      let t2 = expression typs e2 in
      let t3 = expression typs e3 in
      dist_type_join t2 t3
    | Elet (p, e1, e2) ->
      let t1 = expression typs e1 in
      let typs = pattern typs p t1 in
      let t2 = expression typs e2 in
      t2
    | Ecall_init ({expr=e;_}) -> 
      begin match e with
      | Evar({name=name;_}) -> 
        begin match TypMap.find_opt { modul = None; name=name^".init" } typs with
        | Some(t) -> t
        | None -> failwith "Variable not found2"
        end
      | _ -> failwith "Unexpected expression"
      end
    | Ecall_step ({expr=e;_}, _) -> 
      begin match e with
      | Evar({name=name;_}) -> 
        begin match TypMap.find_opt { modul = None; name=name^".step" } typs with
        | Some(t) -> t
        | None -> failwith "Variable not found3"
        end
      | _ -> failwith "Unexpected expression"
      end
    | Esample (_, e) -> 
      let t = expression typs e in
      begin match t with
      | Dist(d) -> Var(d)
      | _ -> failwith "Invalid sample"
      end
    | Eobserve _ -> Det
    | Einfer (_, _) ->
      (* TODO: Assign stream name in step *)
      Any
      (* begin match TypMap.find_opt id typs with
      | Some(t) -> 
        begin match t with
        | Var(d) -> Dist(d)
        | _ -> failwith "Invalid inference"
        end
      | None -> failwith "Variable not found4"
      end *)
    | Ecall_reset _ | Efactor _ -> failwith "Not implemented"
    | Efun _ | Ematch _ | Efield _ | Esequence _
    | Erecord _ | Econstr _ -> failwith "Unexpected expression typs"
  in

  let funct typs { patt = p; pmeta = (); } e =
    let typs = get_param_typ typs p in
    let t = expression typs e in
    t
  in

  let typs = List.fold_left (fun typs d -> 
    match d.decl with
    | Dfun (name, p, e) ->
      let t = funct typs p e in
      TypMap.add { modul = None; name } t typs
    | Dnode (name, _, {n_init=e_init; n_step=(p, e_step); _}) ->
      let t_init = expression typs e_init in
      let t_step = funct typs p e_step in
      TypMap.add { modul = None; name=name^".init" } t_init typs
      |> TypMap.add { modul = None; name=name^".step" } t_step
    | Ddecl (p, e) ->
      let t = expression typs e in
      pattern typs p t
    | _ -> typs
  ) (TypMap.empty) muf_list 
  in
  typs

let ops =
  [
    "bernoulli";
    "gaussian";
    "random_order";
    "beta";
    "delta";
    "binomial";
    "infer_init";
    "const";
    "pair";
    "array";
    "lst";
    "ite";
    "add";
    "add_int";
    "sub_int";
    "mult";
    "eval";
    "get";
    "poisson";
    "shuffle";
    "not";
    "lt";
    "print_any_t";
    "print_newline";
    "of_distribution";
    "float_of_int";
    "expp";
    "subtract";
    "approx";
    "exact";
    "sub_float";
    "pow";
    "exit";
  ]

let ops = List.map (fun x -> { modul = None; name = x }) ops

exception Infer

(* Approximation status by simulation SSI *)
(* let process_node e_init p_state p_state p_in e ctx = 
  let module E = Evaluator (Empty) in
  let (t_init, _), _ =
    E.eval ()
      (fun _ _ _ _ _ _ -> true)
      ops ctx e_init.expr
  in
  let approx_stat =
    try
      ignore (infer_status e_init.expr ctx);
      true
    with Infer -> false
  in
  ({ t_state = t_init; p_state; p_in; e; ctx }, approx_stat) *)
  

(* let inference_strategy_sat muf_list approx_status = *)
