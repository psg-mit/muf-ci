open Mufextern

let string_of_ident : identifier -> string =
fun id ->
  match id.modul with
  | None -> id.name
  | Some m -> m ^ "." ^ id.name

(* Abstract *)
module RandomVar = struct
  type t = identifier

  let compare = compare

  let to_string = string_of_ident
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
| Ddelta_observed
| Dunk

let v_n = ref 0

let get_obs () =
  v_n := !v_n + 1;
  {modul=None; name="obs" ^ (string_of_int (!v_n))}

let string_of_constant : abs_constant -> string =
fun c ->
  match c with
  | Cbool b -> string_of_bool b
  | Cint i -> string_of_int i
  | Cfloat f -> string_of_float f
  | Cstring s -> s
  | Cunit -> "()"
  | Cunk -> "Const Unknown"

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
  | Erandomvar rv -> Printf.sprintf "%s" (string_of_ident rv)
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
  | Ddelta_observed -> "Delta-Observed"
  | Ddelta_sampled -> "Delta-Sampled"
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
exception Inference_Strategy_Error

let rec eval_add e1 e2 =
  match e1, e2 with
  | Econst (Cfloat c1), Econst (Cfloat c2) -> Econst (Cfloat (c1 +. c2))
  | Econst (Cfloat c1), Eadd(Econst (Cfloat c2), e3) -> 
    eval_add (Econst (Cfloat (c1 +. c2))) e3
  | Econst (Cfloat c1), Eadd(e2, Econst (Cfloat c3)) ->
    eval_add (Econst (Cfloat (c1 +. c3))) e2
  | Eadd(Econst (Cfloat c1), e2), e3 -> 
    eval_add (Econst (Cfloat c1)) (eval_add e2 e3)
  | Econst (Cfloat 0.), e2 -> e2
  | e1, Econst (Cfloat 0.) -> e1
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
  | Econst (Cfloat 0.), _ | _, Econst (Cfloat 0.) -> Econst (Cfloat 0.)
  | _ -> Emul (e1, e2)

let eval_div e1 e2 =
  match e1, e2 with
  | Econst (Cfloat c1), Econst (Cfloat c2) -> Econst (Cfloat (c1 /. c2))
  | e1, Econst (Cfloat 1.) -> e1
  | _ -> Ediv (e1, e2)

let eval_unop op e =
  match op, e with
  | Squared, Econst (Cfloat c) -> Econst (Cfloat (c ** 2.))
  | SquareRoot, Econst (Cfloat c) -> Econst (Cfloat (Float.sqrt c))
  | Exp, Econst (Cfloat c) -> Econst (Cfloat (Float.exp c))
  | _ -> Eunop (op, e)

let eval_if ~eval e1 e2 e3 =
  match e1 with
  | Econst (Cbool true) -> e2
  | Econst (Cbool false) -> e3
  | _ -> if eval then Eifeval (e1, e2, e3) else Eif (e1, e2, e3)

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
    eval_if ~eval:false e1 e2 e3
  | Eifeval (e1, e2, e3) ->
    let e1 = eval_expr e1 in
    let e2 = eval_expr e2 in
    let e3 = eval_expr e3 in
    eval_if ~eval:true e1 e2 e3
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
  | Ddelta_sampled | Ddelta_observed | Dunk -> d

exception NonConjugate of RandomVar.t

module SymState = struct
  type t = (abs_distribution ref) RVMap.t
  (* abs_distribution RVMap.t *)
  let empty : t = RVMap.empty

  let find : RandomVar.t -> t -> abs_distribution ref = RVMap.find
  
  let remove : RandomVar.t -> t -> t =
  fun rv s ->
    RVMap.remove rv s

  let add : RandomVar.t -> abs_distribution ref -> t -> t =
  fun rv d g ->
    RVMap.add rv d g

  (* Helper functions *)
  (* let join : t -> t -> t =
    fun g1 g2 ->
      RVMap.merge (fun _ d1 d2 ->
        match d1, d2 with
        | Some d1, Some d2 -> Some (join_distribution d1 d2)
        | Some d1, None -> Some d1
        | None, Some d2 -> Some d2
        | None, None -> None
      ) g1 g2 *)

  (* let eval : RandomVar.t -> t -> t =
  fun rv g ->
    let d = find rv g in
    let d = eval_distribution d in
    add rv d g *)

  let clean : t -> t =
  fun g ->
    RVMap.filter (fun _ d ->
      match !d with
      | Ddelta_observed -> false
      | _ -> true
    ) g

  let to_string : t -> string =
    fun g ->
      RVMap.fold (fun rv d acc ->
        Format.sprintf "%s%s: %s\n" acc (string_of_ident rv) (string_of_distribution !d)) g ""   
end

(* Returns true if expression evaluates to a constant *)
let rec is_const : abs_expr -> SymState.t -> bool =
fun e g ->
  match e with
  | Econst _ -> true
  | Eunk -> false
  | Erandomvar rv' ->
    begin match !(SymState.find rv' g) with
    | Ddelta _ | Ddelta_observed | Ddelta_sampled -> true
    | _ -> false
    end
  | Etuple es | Elist es ->
    List.for_all (fun e -> is_const e g) es
  | Eadd (e1, e2) | Emul (e1, e2) | Ediv (e1, e2) ->
    is_const e1 g && is_const e2 g
  | Eunop (_, e1) ->
    is_const e1 g
  | Eif (e1, e2, e3) | Eifeval (e1, e2, e3) ->
    is_const e1 g && is_const e2 g && is_const e3 g
  | Edistr _ -> false

module AbstractSSI = struct

  let intervene : RandomVar.t -> abs_distribution -> SymState.t -> SymState.t =
  fun rv d g ->
    let d = eval_distribution d in
    let dref = SymState.find rv g in
    dref := d;
    g

  let is_member_list : RandomVar.t -> RandomVar.t list -> SymState.t -> bool =
  fun rv rvs g ->
    List.exists (fun rv' -> SymState.find rv g == SymState.find rv' g) rvs

  let is_member_set : RandomVar.t -> RVSet.t -> SymState.t -> bool =
  fun rv rvs g ->
    RVSet.exists (fun rv' -> SymState.find rv g == SymState.find rv' g) rvs

  (* Returns true if expression e depends on random variable rv *)
  let rec depends_on : abs_expr -> RandomVar.t -> SymState.t -> bool -> bool =
  fun e rv g transitive ->
    match e with
    | Econst _ -> false
    | Eunk -> 
      (* Conservatively assume it depends on rv *)
      true
    | Erandomvar rv' ->
      if is_const e g then false
      else
        if SymState.find rv g == SymState.find rv' g then 
          true
        else
          if transitive then
            let d = SymState.find rv' g in
            depends_on_distribution !d rv g transitive
          else false
    | Etuple es | Elist es ->
      List.exists (fun e -> depends_on e rv g transitive) es
    | Eadd (e1, e2) | Emul (e1, e2) | Ediv (e1, e2) ->
      depends_on e1 rv g transitive || depends_on e2 rv g transitive
    | Eunop (_, e1) ->
      depends_on e1 rv g transitive
    | Eif (e1, e2, e3) | Eifeval (e1, e2, e3) ->
      depends_on e1 rv g transitive || 
      depends_on e2 rv g transitive || 
      depends_on e3 rv g transitive
    | Edistr d ->
      depends_on_distribution d rv g transitive
  and depends_on_distribution : abs_distribution -> RandomVar.t -> SymState.t -> bool -> bool =
  fun d rv g transitive ->
    match d with
    | Dgaussian (e1, e2) | Dbeta (e1, e2) | Dbinomial (e1, e2) 
    | Dnegativebinomial (e1, e2) | Dgamma (e1, e2) ->
      depends_on e1 rv g transitive || depends_on e2 rv g transitive
    (* | DmvNormal of expr * expr *)
    | Dcategorical (e1, e2, e3) | Dbetabinomial (e1, e2, e3) -> 
      depends_on e1 rv g transitive || 
      depends_on e2 rv g transitive || 
      depends_on e3 rv g transitive
    | Dbernoulli e | Dexponential e | Dpoisson e | Ddelta e ->
      depends_on e rv g transitive
    | Ddelta_sampled -> false
    | Ddelta_observed -> false
    | Dunk -> 
      (* Conservatively assume it depends on rv *)
      true

  let rec indirectly_depends_on : abs_expr -> RandomVar.t -> SymState.t -> bool =
  fun e rv g ->
    match e with
    | Econst _ -> false
    | Eunk -> 
      (* Conservatively assume it depends on rv *)
      true
    | Erandomvar rv' ->
      if is_const e g then false
      else
        if not (SymState.find rv g == SymState.find rv' g) then 
          let d = SymState.find rv' g in
          indirectly_depends_on_distribution !d rv g
        else
          false
    | Etuple es | Elist es ->
      List.exists (fun e -> indirectly_depends_on e rv g) es
    | Eadd (e1, e2) | Emul (e1, e2) | Ediv (e1, e2) ->
      indirectly_depends_on e1 rv g || indirectly_depends_on e2 rv g
    | Eunop (_, e1) ->
      indirectly_depends_on e1 rv g
    | Eif (e1, e2, e3) | Eifeval (e1, e2, e3) ->
      indirectly_depends_on e1 rv g || 
      indirectly_depends_on e2 rv g || 
      indirectly_depends_on e3 rv g
    | Edistr d ->
      indirectly_depends_on_distribution d rv g
  and indirectly_depends_on_distribution : abs_distribution -> RandomVar.t -> SymState.t -> bool =
  fun d rv g ->
    match d with 
    | Dgaussian (e1, e2) | Dbeta (e1, e2) | Dbinomial (e1, e2) 
    | Dnegativebinomial (e1, e2) | Dgamma (e1, e2) ->
      depends_on e1 rv g true || depends_on e2 rv g true
    (* | DmvNormal of expr * expr *)
    | Dcategorical (e1, e2, e3) | Dbetabinomial (e1, e2, e3) -> 
      depends_on e1 rv g true || 
      depends_on e2 rv g true || 
      depends_on e3 rv g true
    | Dbernoulli e | Dexponential e | Dpoisson e | Ddelta e ->
      depends_on e rv g true
    | Ddelta_sampled -> false
    | Ddelta_observed -> false
    | Dunk -> 
      (* Conservatively assume it depends on rv *)
      true
  
  (* Returns Some(a,b) if e can be written as an affine function of
   * rv (e = a * rv + b) *)
  let rec is_affine : abs_expr -> RandomVar.t -> SymState.t -> (abs_expr * abs_expr) option =
  fun e rv g ->
    match e with
    | Econst c -> Some (Econst (Cfloat 0.), Econst c)
    | Erandomvar rv' ->
      if is_const e g then Some (Econst (Cfloat 0.), e)
      else
        if SymState.find rv g == SymState.find rv' g then 
          Some(Econst (Cfloat 1.), Econst (Cfloat 0.))
        else
          Some (Econst (Cfloat 0.), Erandomvar rv')
    | Eadd (e1, e2) ->
      begin match is_affine e1 rv g, is_affine e2 rv g with
      | Some (a1, b1), Some(a2, b2) ->
        Some (Eadd (a1, a2), Eadd (b1, b2))
      | _ -> None
      end
    | Emul (e1, e2) ->
      begin match is_affine e1 rv g, is_affine e2 rv g with
      | Some (a1, b1), Some(a2, b2) ->
        begin match eval_expr a1, eval_expr a2 with
        | Econst (Cfloat 0.), Econst (Cfloat 0.) ->
          Some (Econst (Cfloat 0.), Emul (b1, b2))
        | a1, Econst (Cfloat 0.) ->
          Some (Emul(a1, b2), Emul(b1, b2))
        | Econst (Cfloat 0.), a2 ->
          Some (Emul(b1, a2), Emul(b1, b2))
        | _ -> None
        end
      | _ -> None
      end
    | Ediv (e1, e2) ->
      begin match is_affine e1 rv g, is_affine e2 rv g with
      | Some (a1, b1), Some (a2, b2) ->
        begin match eval_expr a2 with
        | Econst (Cfloat 0.) -> Some(Ediv(a1, b2), Ediv(b1, b2))
        | _ -> None
        end
      | _ -> None
      end
    | Eunop (op, e1) ->
      begin match is_affine e1 rv g with
      | Some(a, b) ->
        begin match eval_expr a with
        | Econst (Cfloat 0.) -> Some (Econst (Cfloat 0.), Eunop (op, b))
        | _ -> None
        end
      | _ -> None
      end
    | Eif _ | Eifeval _ -> None
    | Etuple _ | Elist _ | Edistr _ -> None
    | Eunk -> 
      (* Conservatively assume it is not affine wrt rv *)
      None

  let get_parents : RandomVar.t -> SymState.t -> RandomVar.t list =
  fun rv g ->
    let rec get_parents_expr : abs_expr -> RandomVar.t list =
    fun e ->
      match e with
      | Erandomvar rv -> if is_const e g then [] else [rv]
      | Eadd (e1, e2) | Emul (e1, e2) | Ediv (e1, e2) -> 
        List.append (get_parents_expr e1) (get_parents_expr e2)
      | Eunop (_, e1) -> get_parents_expr e1
      | Eif (e1, e2, e3) | Eifeval (e1, e2, e3) -> 
        List.append (List.append (get_parents_expr e1) (get_parents_expr e2)) (get_parents_expr e3)
      | Elist es | Etuple es ->
        List.fold_left (fun acc e -> List.append acc (get_parents_expr e)) [] es
      | Econst _ | Eunk -> []
      | Edistr _ -> failwith "get_parents_expr: unexpected expression"
    in

    match !(SymState.find rv g) with
    | Dgaussian (e1, e2) | Dbeta (e1, e2) | Dbinomial (e1, e2) | Dnegativebinomial (e1, e2) 
    | Dgamma (e1, e2) ->
      List.append (get_parents_expr e1) (get_parents_expr e2)
    (* | DmvNormal of expr * expr *)
    | Dcategorical (e1, e2, e3) | Dbetabinomial (e1, e2, e3) -> 
      List.append (List.append (get_parents_expr e1) (get_parents_expr e2)) (get_parents_expr e3)
    | Dbernoulli e | Dexponential e | Dpoisson e | Ddelta e ->
      get_parents_expr e
    | Ddelta_sampled | Ddelta_observed | Dunk -> [] 

  let topo_sort : RandomVar.t list -> SymState.t -> RandomVar.t list =
  fun rvs g ->
    let sorted_nodes = ref [] in

    let rec visit : RandomVar.t -> unit =
    fun rv ->
      let parents = get_parents rv g in
      let rec visit_parents : RandomVar.t list -> unit =
      fun parents ->
        match parents with
        | [] -> ()
        | rv_parent :: rvs ->
          visit rv_parent;
          visit_parents rvs
      in
      visit_parents parents;
      if (not (is_member_list rv !sorted_nodes g)) then
        sorted_nodes := rv :: !sorted_nodes
    in

    let rec do_visits : RandomVar.t list -> unit =
    fun rvs ->
      match rvs with
      | [] -> ()
      | rv :: rvs ->
        visit rv;
        do_visits rvs
    in
    do_visits rvs;

    (* Dedup *)
    List.filter (fun rv -> is_member_list rv rvs g) !sorted_nodes

  (* Returns whether rv_parent and rv_child can be swapped without creating a cycle *)
  let can_swap : RandomVar.t -> RandomVar.t -> SymState.t -> bool =
  fun rv_parent rv_child g ->
    match !(SymState.find rv_child g) with
    | Dgaussian (e1, e2) | Dbeta (e1, e2) | Dbinomial (e1, e2) 
    | Dnegativebinomial (e1, e2) | Dgamma (e1, e2) ->
      ((depends_on e1 rv_parent g false) ||
      (depends_on e2 rv_parent g false)) &&
      (not (indirectly_depends_on e1 rv_parent g)) &&
      (not (indirectly_depends_on e2 rv_parent g))
    (* | DmvNormal of expr * expr *)
    | Dcategorical (e1, e2, e3) | Dbetabinomial (e1, e2, e3) -> 
      ((depends_on e1 rv_parent g false) ||
      (depends_on e2 rv_parent g false) ||
      (depends_on e3 rv_parent g false)) &&
      (not (indirectly_depends_on e1 rv_parent g)) &&
      (not (indirectly_depends_on e2 rv_parent g)) &&
      (not (indirectly_depends_on e3 rv_parent g))
    | Dbernoulli e | Dexponential e | Dpoisson e | Ddelta e ->
      (depends_on e rv_parent g false) &&
      (not (indirectly_depends_on e rv_parent g))
    | Ddelta_sampled | Ddelta_observed -> 
      (* Never needs to do this, but technically swapping with a constant
         never creates a cycle *)
      true
    | Dunk -> 
      (* Overapproximating by assuming it doesn't create a cycle *)
      true

  let gaussian_marginal : RandomVar.t -> RandomVar.t -> SymState.t -> abs_distribution option =
  fun rv1 rv2 g ->
    let prior, likelihood = SymState.find rv1 g, SymState.find rv2 g in
    match !prior, !likelihood with
    | (Dgaussian(mu_0, var_0), Dgaussian(mu, var)) ->
      begin match is_affine mu rv1 g with
      | Some(a, b) ->
        if (not (depends_on mu_0 rv2 g true)) &&
            (not (depends_on var_0 rv2 g true)) &&
            (not (depends_on var rv1 g true)) then
          let mu' = Eadd ((Emul (a, mu_0)), b) in
          let var' = Eadd ((Emul(Eunop (Squared, a), var_0)), var) in
          Some(Dgaussian(mu', var'))
        else
          None
      | None -> None
      end
    | _ -> None

  let gaussian_posterior : RandomVar.t -> RandomVar.t -> SymState.t -> abs_distribution option =
  fun rv1 rv2 g ->
    let prior, likelihood = SymState.find rv1 g, SymState.find rv2 g in
    match !prior, !likelihood with
    | (Dgaussian(mu_0, var_0), Dgaussian(mu, var)) ->
      begin match is_affine mu rv1 g with
      | Some(a, b) ->
        if (not (depends_on mu_0 rv2 g true)) &&
          (not (depends_on var_0 rv2 g true)) &&
          (not (depends_on var rv1 g true)) then

          (* Apply the linear transformation *)
          let mu_0' = Eadd (Emul(a, mu_0), b) in
          let var_0' = Emul(Eunop (Squared, a), var_0) in

          (* Perform the conjugate update *)
          let inv_var'' = Eadd (Ediv (Econst (Cfloat 1.), var_0'), Ediv(Econst (Cfloat 1.), var)) in
          let mu'' = Emul(Ediv(Econst (Cfloat 1.), inv_var''), Eadd(Ediv(mu_0', var_0'), Ediv (Erandomvar rv2, var))) in
          let var'' = Ediv(Econst (Cfloat 1.), inv_var'') in

          (* Apply the inverse linear transformation *)
          let mu''' = Ediv (Eadd (mu'', (Emul (Econst (Cfloat (-1.)), b))), a) in
          let var''' = Ediv (var'', (Eunop (Squared, a))) in
          Some(Dgaussian(mu''', var'''))
        else
          None
      | None -> None
      end
    | _ -> None

  let beta_bernoulli_marginal : RandomVar.t -> RandomVar.t -> SymState.t -> abs_distribution option =
  fun rv1 rv2 g ->
    let prior, likelihood = SymState.find rv1 g, SymState.find rv2 g in
    match !prior, !likelihood with
    | Dbeta(a, b), Dbernoulli(Erandomvar rv) ->
      if rv = rv1 &&
        (not (depends_on a rv2 g true)) &&
        (not (depends_on b rv2 g true)) 
      then
        Some(Dbernoulli(Ediv(a, Eadd(a, b))))
      else
        None
  | _ -> None

  let beta_bernoulli_posterior : RandomVar.t -> RandomVar.t -> SymState.t -> abs_distribution option =
  fun rv1 rv2 g ->
    let prior, likelihood = SymState.find rv1 g, SymState.find rv2 g in
    match !prior, !likelihood with
    | Dbeta(a, b), Dbernoulli(Erandomvar rv) ->
      if rv = rv1 &&
        (not (depends_on a rv2 g true)) &&
        (not (depends_on b rv2 g true)) 
      then
        Some(Dbeta(Eadd(a, Eif(Erandomvar rv2, Econst(Cfloat 1.), Econst(Cfloat 0.))),
          Eadd(b, Eif(Erandomvar(rv2), Econst(Cfloat 0.), Econst(Cfloat 1.)))))
      else
        None
    | _ -> None

  let swap : RandomVar.t -> RandomVar.t -> SymState.t -> bool * SymState.t =
  fun rv1 rv2 g ->
    match !(SymState.find rv1 g), !(SymState.find rv2 g) with
    | Dgaussian (_, _), Dgaussian (_, _) ->
      begin match gaussian_marginal rv1 rv2 g, gaussian_posterior rv1 rv2 g with
      | Some(dist_marg), Some(dist_post) ->
        let g = intervene rv2 dist_marg g in
        let g = intervene rv1 dist_post g in
        true, g
      | _ -> false, g
      end
    | Dbeta(_, _), Dbernoulli(_) ->
      begin match beta_bernoulli_marginal rv1 rv2 g, beta_bernoulli_posterior rv1 rv2 g with
      | Some(dist_marg), Some(dist_post) ->
        let g = intervene rv2 dist_marg g in
        let g = intervene rv1 dist_post g in
        true, g
      | _ -> false, g
      end
    (* | Dbeta(_, _), Dbinomial(_, _) ->
      begin match beta_binomial_marginal rv1 rv2 g, beta_binomial_posterior rv1 rv2 g with
      | Some(dist_marg), Some(dist_post) ->
        let g = add rv2 dist_marg g in
        let g = add rv1 dist_post g in
        true, g
      | _ -> false, g
      end
    | Dgamma(_, _), Dpoisson(_, _) ->
      begin match gamma_poisson_marginal rv1 rv2 g, gamma_poisson_posterior rv1 rv2 g with
      | Some(dist_marg), Some(dist_post) ->
        let g = add rv2 dist_marg g in
        let g = add rv1 dist_post g in
        true, g
      | _ -> false, g
      end
    | Dgamma(_, _), Dgaussian(_, _) ->
      begin match gamma_normal_marginal rv1 rv2 g, gamma_normal_posterior rv1 rv2 g with
      | Some(dist_marg), Some(dist_post) ->
        let g = add rv2 dist_marg g in
        let g = add rv1 dist_post g in
        true, g
      | _ -> false, g
      end
    (* | Dcategorical(_, _, _), Dcategorical(_, _, _) -> *)
      (* begin match categorical_marginal rv1 rv2 g, categorical_posterior rv1 rv2 g with
      | Some(dist_marg), Some(dist_post) ->
        let g = add rv2 dist_marg g in
        let g = add rv1 dist_post g in
        true, g
      | _ -> false, g
      end *)
    | Dbernoulli _, Dbernoulli _ ->
      begin match bernoulli_marginal rv1 rv2 g, bernoulli_posterior rv1 rv2 g with
      | Some(dist_marg), Some(dist_post) ->
        let g = add rv2 dist_marg g in
        let g = add rv1 dist_post g in
        true, g
      | _ -> false, g
      end *)
    | _ -> false, g

  let hoist : RandomVar.t -> SymState.t -> SymState.t =
  fun rv g ->
    let rec hoist_inner : RandomVar.t -> RVSet.t -> SymState.t -> SymState.t =
    fun rv_child ghost_roots g ->
      let parents = List.rev (topo_sort (get_parents rv_child g) g) in

      (* Format.printf "Hoisting %s with ghost roots [%s] and parents [%s]\n" (RandomVar.to_string rv_child)
        (String.concat ", " (List.map RandomVar.to_string (RVSet.elements ghost_roots)))
        (String.concat ", " (List.map RandomVar.to_string parents)); *)

      let rec hoist_parents : RandomVar.t list -> RVSet.t -> SymState.t -> SymState.t =
      fun parents ghost_roots g ->
        match parents with
        | [] -> g
        | rv_parent :: rvs ->
          (* Format.printf "Got parent: %s\n" (RandomVar.to_string rv_parent); *)
          let g = 
            if not (is_member_set rv_parent ghost_roots g) then
              (* let _ = Format.printf "Recursing into %s\n" (RandomVar.to_string rv_parent) in *)
              hoist_inner rv_parent ghost_roots g
            else g
          in
          hoist_parents rvs (RVSet.add rv_parent ghost_roots) g
      in

      let rec swap_with_parents : RandomVar.t list -> SymState.t -> SymState.t =
      fun parents g ->
        match parents with
        | [] -> g
        | rv_parent :: rvs ->
          if not (is_member_set rv_parent ghost_roots g) then
            begin 
            (* let () = Format.printf "Swapping %s with %s\n" (RandomVar.to_string rv_parent) (RandomVar.to_string rv_child) in *)
            (if not (can_swap rv_parent rv_child g) then
              (* TODO: bug  *)
              (* let () = 
                Format.printf "parent %s; parent's parents: [%s]\n" (RandomVar.to_string rv_parent)
                  (String.concat ", " (List.map RandomVar.to_string (get_parents rv_parent g)))
              in *)
              (* let () = 
                Format.printf "child %s; child's parents: [%s]\n" (RandomVar.to_string rv_child)
                  (String.concat ", " (List.map RandomVar.to_string (get_parents rv_child g)))
              in *)
              failwith (Format.sprintf "Cannot swap parent %s and child %s" 
                (RandomVar.to_string rv_parent) (RandomVar.to_string rv_child) ));
            
            let did_swap, g = swap rv_parent rv_child g in
            if did_swap then
              (* let _ = 
                Format.printf "Swapped %s and %s\n" (RandomVar.to_string rv_parent) (RandomVar.to_string rv_child)
              in *)
              swap_with_parents rvs g
            else 
              raise (NonConjugate rv_parent)
            end
          else
            (* let _ = Format.printf "Parent %s is a ghost root\n" (RandomVar.to_string rv_parent) in *)
            swap_with_parents rvs g
      in

      let g = hoist_parents parents ghost_roots g in
      (* Format.printf "Dont hoisting parents for %s\n" (RandomVar.to_string rv_child); *)
      let parents = List.rev parents in
      (* Format.printf "Begin swapping child %s with parents [%s]\n" (RandomVar.to_string rv_child)
        (String.concat ", " (List.map RandomVar.to_string parents)); *)
      let g = swap_with_parents parents g in
      (* Format.printf "Done hoisting %s\n" (RandomVar.to_string rv_child); *)
      g
    in

    hoist_inner rv RVSet.empty g
  
  (* Abstract Semi-symbolic inference interface *)

  let assume : RandomVar.t -> abs_expr -> SymState.t -> SymState.t =
  fun x e g ->
    match e with
    | Edistr d -> 
      let d = eval_distribution d in
      let d = ref d in
      SymState.add x d g
    | _ -> failwith "SymState.add: Not a distribution"

  let rec value : identifier -> SymState.t -> SymState.t =
  fun rv g ->
    try 
      let g = hoist rv g in
      intervene rv Ddelta_sampled g
    with NonConjugate rv_parent ->
      Format.printf "Value Non-conjugate: %s\n" (RandomVar.to_string rv_parent);
      let g' = value rv_parent g in
      value rv g'

  let rec value_expr : abs_expr -> SymState.t -> SymState.t =
  fun e g ->
    match e with 
    | Econst _ | Eunk -> g
    | Erandomvar rv -> if is_const e g then g else value rv g
    | Etuple es | Elist es ->
      List.fold_left (fun g e -> value_expr e g) g es
    | Eadd (e1, e2) | Emul (e1, e2) | Ediv (e1, e2) ->
      value_expr e1 (value_expr e2 g)
    | Eunop (_, e1) -> value_expr e1 g
    | Eif (e1, e2, e3) | Eifeval (e1, e2, e3) ->
      value_expr e1 (value_expr e2 (value_expr e3 g))
    | Edistr _ -> 
      failwith "SymState.value_expr: not a random variable"

  let assume_patt : pattern -> annotation -> abs_expr -> SymState.t -> SymState.t * abs_expr =
    fun patt a es g ->
    let rec add_patt : pattern -> abs_expr -> SymState.t * RandomVar.t list -> SymState.t * RandomVar.t list =
      fun patt es (g, xs) ->
        match patt, es with
        | Pid id, es -> 
          begin match es with
          | Etuple _ -> failwith "SymState.add_patt: Cannot sample multiple distributions"
          | _ -> 
            let g = assume id es g in
            let g =
              match a with
              | Mufextern.Aapprox -> value id g
              | _ -> g
            in
            g, id :: xs
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

  let rec observe : RandomVar.t -> SymState.t -> SymState.t =
  fun rv g ->
    try 
      let g = hoist rv g in
      intervene rv Ddelta_observed g
    with NonConjugate rv_parent ->
      Format.printf "Observe Non-conjugate: %s\n" (RandomVar.to_string rv_parent);
      let g' = value rv_parent g in
      observe rv g'

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

  let find : RandomVar.t -> t -> ApproximationStatus.t =
  fun rv inf ->
    RVMap.find rv inf

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
      match !d with
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
      let inferred_status = find rv inferred in
      (if not (ApproximationStatus.verify ann_status inferred_status) then
        raise (Approximation_Status_Error (rv, ann_status, inferred_status)));
    ) ann
end

let rec has_randomvar g e =
  match e with 
  | Erandomvar _ -> not (is_const e g)
  | Eadd (e1, e2) | Emul (e1, e2)
  | Ediv (e1, e2) -> 
    has_randomvar g e1 || has_randomvar g e2
  | Eunop (_, e1) -> has_randomvar g e1
  | Eif (e1, e2, e3) | Eifeval (e1, e2, e3) -> 
    has_randomvar g e1 || has_randomvar g e2 || has_randomvar g e3
  | Elist es | Etuple es ->
    List.exists (has_randomvar g) es
  | Edistr d -> has_randomvar_distr g d
  | Econst _ -> false
  | Eunk -> 
    (* Overapproximation by conservatively assuming it has a random variable *)
    true
and has_randomvar_distr g d =
  match d with
  | Dgaussian (e1, e2) | Dbeta (e1, e2) | Dbinomial (e1, e2)
  | Dnegativebinomial (e1, e2) | Dgamma (e1, e2) -> 
    has_randomvar g e1 || has_randomvar g e2
  (* | DmvNormal (e1, e2) -> has_randomvar ctx e1 || has_randomvar ctx e2 *)
  | Dcategorical (e1, e2, e3) | Dbetabinomial (e1, e2, e3) -> 
    has_randomvar g e1 || has_randomvar g e2 || has_randomvar g e3
  | Dbernoulli e1 | Dexponential e1 
  | Dpoisson e1 | Ddelta e1 -> has_randomvar g e1
  | Ddelta_sampled -> false
  | Ddelta_observed -> false
  | Dunk -> 
    (* Overapproximation by conservatively assuming it has a random variable *)
    true

(* TODO: This widening can be even smarter... curently has alias *)
let rec join_expr : abs_expr -> abs_expr -> SymState.t -> abs_expr * SymState.t =
fun e1 e2 g ->
  let e1 = eval_expr e1 in
  let e2 = eval_expr e2 in
  match e1, e2 with
  | Econst c1, Econst c2 ->
    let e = if c1 = c2 then Econst c1 else Econst Cunk in
    e, g
  | Etuple es1, Etuple es2 ->
    let es, g = List.fold_left2 (fun (es, g) e1 e2 ->
      let e, g = join_expr e1 e2 g in
      e :: es, g
    ) ([], g) es1 es2 in
    Etuple (List.rev es), g
  | Eadd (e11, e12), Eadd (e21, e22) ->
    let e1, g = join_expr e11 e21 g in
    let e2, g = join_expr e12 e22 g in
    eval_add e1 e2, g
  | Emul (e11, e12), Emul (e21, e22) ->
    let e1, g = join_expr e11 e21 g in
    let e2, g = join_expr e12 e22 g in
    eval_mul e1 e2, g
  | Ediv (e11, e12), Ediv (e21, e22) ->
    let e1, g = join_expr e11 e21 g in
    let e2, g = join_expr e12 e22 g in
    eval_div e1 e2, g
  | Eunop (u1, e1), Eunop (u2, e2) ->
    if u1 = u2 then 
      let e, g = join_expr e1 e2 g in
      eval_unop u1 e, g
    else Eunk, g
  | Eif (e11, e12, e13), Eif (e21, e22, e23) ->
    let e1, g = join_expr e11 e21 g in
    let e2, g = join_expr e12 e22 g in
    let e3, g = join_expr e13 e23 g in
    eval_if ~eval:false e1 e2 e3, g
  | Eifeval (e11, e12, e13), Eifeval (e21, e22, e23) ->
    let e1, g = join_expr e11 e21 g in
    let e2, g = join_expr e12 e22 g in
    let e3, g = join_expr e13 e23 g in
    eval_if ~eval:true e1 e2 e3, g
  | Elist es1, Elist es2 ->
    let es, g = List.fold_left2 (fun (es, g) e1 e2 ->
      let e, g = join_expr e1 e2 g in
      e :: es, g
    ) ([], g) es1 es2 in
    Elist (List.rev es), g
  | Edistr d1, Edistr d2 ->
    let d, g = join_distribution d1 d2 g in
    Edistr d, g
  | Erandomvar rv1, Erandomvar rv2 ->
    if rv1 = rv2 then
      Erandomvar rv1, g
    else 
      let d1 = SymState.find rv1 g in
      let d2 = SymState.find rv2 g in
      let d, g = join_distribution !d1 !d2 g in
      let g = AbstractSSI.intervene rv1 d g in
      (* Map rv2 to rv1 *)
      (* let g = SymState.remove rv2 g in *)
      let g = SymState.add rv2 d1 g in
      Erandomvar rv1, g
  | _ -> Eunk, g

and join_distribution : abs_distribution -> abs_distribution -> SymState.t 
  -> abs_distribution * SymState.t =
fun d1 d2 g ->
  match d1, d2 with
  | Dgaussian (e1, e2), Dgaussian (e1', e2') -> 
    let e1, g = join_expr e1 e1' g in
    let e2, g = join_expr e2 e2' g in
    Dgaussian (e1, e2), g
  | Dcategorical (e1, e2, e3), Dcategorical (e1', e2', e3') ->
    let e1, g = join_expr e1 e1' g in
    let e2, g = join_expr e2 e2' g in
    let e3, g = join_expr e3 e3' g in
    Dcategorical (e1, e2, e3), g
  | Dbeta (e1, e2), Dbeta (e1', e2') ->
    let e1, g = join_expr e1 e1' g in
    let e2, g = join_expr e2 e2' g in
    Dbeta (e1, e2), g
  | Dbernoulli e1, Dbernoulli e2 ->
    let e, g = join_expr e1 e2 g in
    Dbernoulli e, g
  | Dbinomial (e1, e2), Dbinomial (e1', e2') ->
    let e1, g = join_expr e1 e1' g in
    let e2, g = join_expr e2 e2' g in
    Dbinomial (e1, e2), g
  | Dbetabinomial (e1, e2, e3), Dbetabinomial (e1', e2', e3') ->
    let e1, g = join_expr e1 e1' g in
    let e2, g = join_expr e2 e2' g in
    let e3, g = join_expr e3 e3' g in
    Dbetabinomial (e1, e2, e3), g
  | Dnegativebinomial (e1, e2), Dnegativebinomial (e1', e2') ->
    let e1, g = join_expr e1 e1' g in
    let e2, g = join_expr e2 e2' g in
    Dnegativebinomial (e1, e2), g
  | Dexponential e1, Dexponential e2 ->
    let e, g = join_expr e1 e2 g in
    Dexponential e, g
  | Dgamma (e1, e2), Dgamma (e1', e2') ->
    let e1, g = join_expr e1 e1' g in
    let e2, g = join_expr e2 e2' g in
    Dgamma (e1, e2), g
  | Dpoisson e1, Dpoisson e2 ->
    let e, g = join_expr e1 e2 g in
    Dpoisson e, g
  | Ddelta e1, Ddelta e2 ->
    let e, g = join_expr e1 e2 g in
    Ddelta e, g
  | Ddelta_sampled, Ddelta_sampled -> Ddelta_sampled, g
  | _ -> Dunk, g

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

let infer : Mufextern.program -> InferenceStrategy.t =
fun p ->
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
      if has_randomvar g e1 then
        ctx, g, Eif (e1, e2, e3)
      else
        let es23, g = join_expr e2 e3 g in
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
      if has_randomvar g e1 then
        ctx, g, Eifeval (e1, e2, e3)
      else
        let es23, g = join_expr e2 e3 g in
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
    | Esample (p, a, e1) ->
      let ctx, g, e1 = infer' ctx g e1 in
      let g, xs = AbstractSSI.assume_patt p a e1 g in
      ctx, g, xs
    | Eobserve (e1, e2) ->
      let ctx, g, e1 = infer' ctx g e1 in
      let ctx, g, _ = infer' ctx g e2 in
      let rv = get_obs () in
      (* TODO: widen e1 (if it has ifs) here instead? *)
      let g = AbstractSSI.assume rv e1 g in
      let g = AbstractSSI.observe rv g in
      ctx, g, Econst Cunit
    | Evalue e1 ->
      let ctx, g, e1 = infer' ctx g e1 in
      let g = AbstractSSI.value_expr e1 g in
      ctx, g, e1
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
  let _decls, e = p in

  (* TODO: inline functions *)
  (* TODO: list fold fixpoint *)

  let _, g', _ = infer' ctx g e in
  
  (* TODO: debug. delete later *)
  let sym_state_s = SymState.to_string g' in
  Format.printf "%s\n" sym_state_s;

  (* Remove observed variables *)
  let g' = SymState.clean g' in

  (* Reduce g' into inference strategy *)
  let inferred_inf = InferenceStrategy.from_symstate g' in
  inferred_inf