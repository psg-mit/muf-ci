open Mufextern

let i = ref 0 

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

module ProgVar = struct
  type t = identifier

  let compare = compare

  let to_string = string_of_ident
end

module PVSet = Set.Make (ProgVar)
module PVMap = Map.Make (ProgVar)

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

type abs_cmpop =
| Eq
| Lt

type abs_expr =
| Econst of abs_constant
| Etuple of abs_expr list
(* ops *)
| Eadd of abs_expr * abs_expr
| Emul of abs_expr * abs_expr
| Ediv of abs_expr * abs_expr
| Eintadd of abs_expr * abs_expr
| Eintmul of abs_expr * abs_expr
| Einttofloat of abs_expr
| Eunop of abs_unop * abs_expr
| Ecmp of abs_cmpop * abs_expr * abs_expr
| Eif of abs_expr * abs_expr * abs_expr
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
| Dgamma of abs_expr * abs_expr
| Dpoisson of abs_expr
| Dstudentt of abs_expr * abs_expr * abs_expr
| Ddelta of abs_expr
| Ddelta_sampled 
| Dunk

let v_n = ref 0

let get_obs () =
  v_n := !v_n + 1;
  {modul=Some "Temp"; name="obs" ^ (string_of_int (!v_n))}

let get_temp () =
  v_n := !v_n + 1;
  {modul=Some "Temp"; name="temp" ^ (string_of_int (!v_n))}

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

let string_of_cmpop : abs_cmpop -> string =
fun op ->
  match op with
  | Eq -> "="
  | Lt -> "<"

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
  | Eintadd (e1, e2) -> Printf.sprintf "%s + %s" (string_of_expr e1) (string_of_expr e2)
  | Eintmul (e1, e2) -> Printf.sprintf "%s * %s" (string_of_expr e1) (string_of_expr e2)
  | Einttofloat e -> Printf.sprintf "float(%s)" (string_of_expr e)
  | Eunop (op, e) -> Printf.sprintf "%s(%s)" (string_of_unop op) (string_of_expr e)
  | Ecmp (op, e1, e2) -> Printf.sprintf "%s %s %s" (string_of_expr e1) (string_of_cmpop op) (string_of_expr e2)
  | Eif (e1, e2, e3) -> Printf.sprintf "if %s then %s else %s" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
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
  | Dgamma (e1, e2) ->
    Printf.sprintf "Gamma(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Dpoisson e1 ->
    Printf.sprintf "Poisson(%s)" (string_of_expr e1)
  | Dstudentt (e1, e2, e3) ->
    Printf.sprintf "StudentT(%s, %s, %s)" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
  | Ddelta e1 ->
    Printf.sprintf "Delta(%s)" (string_of_expr e1)
  | Ddelta_sampled -> "Delta-Sampled"
  | Dunk -> "Unknown"

let rec rename_expr : RandomVar.t -> RandomVar.t -> abs_expr -> abs_expr =
fun rv1 rv2 e ->
  match e with
  | Econst _ | Eunk -> e
  | Erandomvar rv -> if rv = rv1 then Erandomvar rv2 else e
  | Etuple es ->
    Etuple (List.map (rename_expr rv1 rv2) es)
  | Eadd (e1, e2) -> 
    Eadd (rename_expr rv1 rv2 e1, rename_expr rv1 rv2 e2)
  | Emul (e1, e2) -> 
    Emul (rename_expr rv1 rv2 e1, rename_expr rv1 rv2 e2)
  | Ediv (e1, e2) -> 
    Ediv (rename_expr rv1 rv2 e1, rename_expr rv1 rv2 e2)
  | Eintadd (e1, e2) -> 
    Eintadd (rename_expr rv1 rv2 e1, rename_expr rv1 rv2 e2)
  | Eintmul (e1, e2) -> 
    Eintmul (rename_expr rv1 rv2 e1, rename_expr rv1 rv2 e2)
  | Einttofloat e -> 
    Einttofloat (rename_expr rv1 rv2 e)
  | Eunop (op, e) -> 
    Eunop (op, rename_expr rv1 rv2 e)
  | Ecmp (op, e1, e2) -> 
    Ecmp (op, rename_expr rv1 rv2 e1, rename_expr rv1 rv2 e2)
  | Eif (e1, e2, e3) -> 
    Eif (rename_expr rv1 rv2 e1, rename_expr rv1 rv2 e2, rename_expr rv1 rv2 e3)
  | Elist es -> 
    Elist (List.map (rename_expr rv1 rv2) es)
  | Edistr d -> 
    Edistr (rename_distr rv1 rv2 d)
and rename_distr : RandomVar.t -> RandomVar.t -> abs_distribution -> abs_distribution =
fun rv1 rv2 d ->
  match d with
  | Dgaussian (e1, e2) -> 
    Dgaussian (rename_expr rv1 rv2 e1, rename_expr rv1 rv2 e2)
  | Dcategorical (e1, e2, e3) -> 
    Dcategorical (rename_expr rv1 rv2 e1, rename_expr rv1 rv2 e2, rename_expr rv1 rv2 e3)
  | Dbeta (e1, e2) -> 
    Dbeta (rename_expr rv1 rv2 e1, rename_expr rv1 rv2 e2)
  | Dbernoulli e1 -> 
    Dbernoulli (rename_expr rv1 rv2 e1)
  | Dbinomial (e1, e2) -> 
    Dbinomial (rename_expr rv1 rv2 e1, rename_expr rv1 rv2 e2)
  | Dbetabinomial (e1, e2, e3) -> 
    Dbetabinomial (rename_expr rv1 rv2 e1, rename_expr rv1 rv2 e2, rename_expr rv1 rv2 e3)
  | Dnegativebinomial (e1, e2) -> 
    Dnegativebinomial (rename_expr rv1 rv2 e1, rename_expr rv1 rv2 e2)
  | Dgamma (e1, e2) -> 
    Dgamma (rename_expr rv1 rv2 e1, rename_expr rv1 rv2 e2)
  | Dpoisson e1 -> 
    Dpoisson (rename_expr rv1 rv2 e1)
  | Dstudentt (e1, e2, e3) -> 
    Dstudentt (rename_expr rv1 rv2 e1, rename_expr rv1 rv2 e2, rename_expr rv1 rv2 e3)
  | Ddelta e1 -> 
    Ddelta (rename_expr rv1 rv2 e1)
  | Ddelta_sampled -> d
  | Dunk -> d


module VarMap = Map.Make (struct
  type t = identifier

  let compare = compare

end)

type ctx = (identifier, abs_expr) Hashtbl.t

let ctx_add_helper : identifier -> abs_expr -> ctx -> unit =
fun x e ctx ->
  match Hashtbl.find_opt ctx x with
  | Some e' -> 
    if e = e' then ()
    else Hashtbl.add ctx x e
  | None ->
    Hashtbl.add ctx x e

let rec ctx_add : pattern -> abs_expr -> ctx -> ctx =
fun p e ctx ->
  match p, e with
  | Pid name, Etuple [e] -> ctx_add_helper name e ctx; ctx
  | Pid name, _ -> ctx_add_helper name e ctx; ctx
  | Ptuple [], Etuple [] | Ptuple [], Eunk -> ctx
  | Ptuple [p], _ -> ctx_add p e ctx
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
  match Hashtbl.find_opt ctx p with
  | Some e -> e
  | None -> Eunk 
    (* failwith (Format.sprintf "ctx_find: %s not found" (string_of_ident p)) *)

let rec ctx_remove : pattern -> ctx -> ctx =
fun p ctx ->
  match p with
  | Pid name -> Hashtbl.remove ctx name; ctx
  | Ptuple ps -> List.fold_left (fun ctx p -> ctx_remove p ctx) ctx ps
  | Pany | Punit -> ctx

let ctx_fold : (identifier -> abs_expr -> 'a -> 'a) -> ctx -> 'a -> 'a =
fun f ctx acc ->
  let acc, _ = Hashtbl.fold (fun x e (acc, seen) ->
    let acc = if PVSet.mem x seen then acc else f x e acc in
    (acc, PVSet.add x seen)
  ) ctx (acc, PVSet.empty) in
  acc

let string_of_ctx : ctx -> string =
fun ctx ->
  Hashtbl.fold (fun name e acc -> 
    acc ^ (Format.sprintf "%s -> %s\n" (string_of_ident name) (string_of_expr e))) ctx ""

module ApproximationStatus = struct
  type t = 
  | Approx
  | Exact
  | Dynamic

  (* Returns -1 if s1 < s2, 1 if s1 > s2, 0 otherwise *)
  let compare : t -> t -> int =
  fun s1 s2 ->
    match s1, s2 with
    | Approx, Approx -> 0
    | Exact, Exact -> 0
    | Dynamic, Dynamic -> 0
    | Approx, Exact -> 0
    | Exact, Approx -> 0
    | _, Dynamic -> -1
    | Dynamic, _ -> 1

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
    | Approx -> "APPROX"
    | Exact -> "EXACT"
    | Dynamic -> "DYNAMIC"
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
  | Econst _, Econst _ -> Econst Cunk
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
  | Econst _, Econst _ -> Econst Cunk
  | _ -> Emul (e1, e2)

let rec eval_sub e1 e2 =
  match e1, e2 with
  | Econst (Cfloat c1), Econst (Cfloat c2) -> Econst (Cfloat (c1 -. c2))
  (* c1 - (c2 + e3) *)
  | Econst (Cfloat c1), Eadd(Econst(Cfloat c2), e3) ->
    eval_sub (Econst (Cfloat (c1 -. c2))) e3
  (* c1 - (e2 + c3) *)
  | Econst (Cfloat c1), Eadd(e2, Econst (Cfloat c3)) ->
    eval_sub (Econst (Cfloat (c1 -. c3))) e2
  | Econst (Cfloat 0.), e2 -> eval_mul (Econst (Cfloat (-1.))) e2
  | e1, Econst (Cfloat 0.) -> e1
  | Econst _, Econst _ -> Econst Cunk
  | _ -> eval_add e1 (eval_mul (Econst (Cfloat (-1.))) e2)

let eval_div e1 e2 =
  match e1, e2 with
  | Econst (Cfloat c1), Econst (Cfloat c2) -> Econst (Cfloat (c1 /. c2))
  | e1, Econst (Cfloat 1.) -> e1
  | Econst _, Econst _ -> Econst Cunk
  | _ -> Ediv (e1, e2)

let rec eval_int_add e1 e2 =
  match e1, e2 with
  | Econst (Cint c1), Econst (Cint c2) -> Econst (Cint (c1 + c2))
  | Econst (Cint c1), Eadd(Econst (Cint c2), e3) -> 
    eval_int_add (Econst (Cint (c1 + c2))) e3
  | Econst (Cint c1), Eadd(e2, Econst (Cint c3)) ->
    eval_int_add (Econst (Cint (c1 + c3))) e2
  | Eadd(Econst (Cint c1), e2), e3 -> 
    eval_int_add (Econst (Cint c1)) (eval_int_add e2 e3)
  | Econst (Cint 0), e2 -> e2
  | e1, Econst (Cint 0) -> e1
  | Econst _, Econst _ -> Econst Cunk
  | _ -> Eintadd (e1, e2)

let rec eval_int_mul e1 e2 =
  match e1, e2 with
  | Econst (Cint c1), Econst (Cint c2) -> Econst (Cint (c1 * c2))
  | Econst (Cint c1), Emul(Econst (Cint c2), e3) -> 
    eval_int_mul (Econst (Cint (c1 * c2))) e3
  | Econst (Cint c1), Emul(e2, Econst (Cint c3)) ->
    eval_int_mul (Econst (Cint (c1 * c3))) e2
  | Econst (Cint c1), Eadd(Econst (Cint c2), e3) -> 
    eval_int_add (Econst (Cint (c1 * c2))) (eval_int_mul (Econst (Cint c1)) e3)
  | Econst (Cint 0), _ | _, Econst (Cint 0) -> Econst (Cint 0)
  | Econst _, Econst _ -> Econst Cunk
  | _ -> Eintmul (e1, e2)

let rec eval_sub_int e1 e2 =
  match e1, e2 with
  | Econst (Cint c1), Econst (Cint c2) -> Econst (Cint (c1 - c2))
  (* c1 - (c2 + e3) *)
  | Econst (Cint c1), Eadd(Econst(Cint c2), e3) ->
    eval_sub_int (Econst (Cint (c1 - c2))) e3
  (* c1 - (e2 + c3) *)
  | Econst (Cint c1), Eadd(e2, Econst (Cint c3)) ->
    eval_sub_int (Econst (Cint (c1 - c3))) e2
  | Econst (Cint 0), e2 -> eval_int_mul (Econst (Cint (-1))) e2
  | e1, Econst (Cint 0) -> e1
  | Econst _, Econst _ -> Econst Cunk
  | _ -> eval_int_add e1 (eval_int_mul (Econst (Cint (-1))) e2)

let eval_unop op e =
  match op, e with
  | Squared, Econst (Cfloat c) -> Econst (Cfloat (c ** 2.))
  | SquareRoot, Econst (Cfloat c) -> Econst (Cfloat (Float.sqrt c))
  | Exp, Econst (Cfloat c) -> Econst (Cfloat (Float.exp c))
  | _, Econst Cunk -> Econst Cunk
  | _ -> Eunop (op, e)

let eval_cmp op e1 e2 =
  match op, e1, e2 with
  | _, Econst Cunk, Econst Cunk -> Econst Cunk
  | Eq, Econst c1, Econst c2 -> Econst (Cbool (c1 = c2))
  | Lt, Econst c1, Econst c2 -> Econst (Cbool (c1 < c2))
  | _ -> Ecmp (op, e1, e2)

let rec subst_rv : abs_expr -> RandomVar.t -> abs_expr -> abs_expr =
fun e rv e' ->
  match e with
  | Eunk | Econst _ -> e
  | Erandomvar rv' -> if rv = rv' then e' else e
  | Etuple es ->
    Etuple (List.map (fun e -> subst_rv e rv e') es)
  | Eadd (e1, e2) ->
    let e1 = subst_rv e1 rv e' in
    let e2 = subst_rv e2 rv e' in
    Eadd (e1, e2)
  | Emul (e1, e2) ->
    let e1 = subst_rv e1 rv e' in
    let e2 = subst_rv e2 rv e' in
    Emul (e1, e2)
  | Ediv (e1, e2) ->
    let e1 = subst_rv e1 rv e' in
    let e2 = subst_rv e2 rv e' in
    Ediv (e1, e2)
  | Eintadd (e1, e2) ->
    let e1 = subst_rv e1 rv e' in
    let e2 = subst_rv e2 rv e' in
    Eintadd (e1, e2)
  | Eintmul (e1, e2) ->
    let e1 = subst_rv e1 rv e' in
    let e2 = subst_rv e2 rv e' in
    Eintmul (e1, e2)
  | Einttofloat e1 ->
    let e1 = subst_rv e1 rv e' in
    Einttofloat e1
  | Eunop (op, e1) ->
    let e1 = subst_rv e1 rv e' in
    Eunop (op, e1)
  | Ecmp (op, e1, e2) ->
    let e1 = subst_rv e1 rv e' in
    let e2 = subst_rv e2 rv e' in
    Ecmp (op, e1, e2)
  | Eif (e1, e2, e3) ->
    let e1 = subst_rv e1 rv e' in
    let e2 = subst_rv e2 rv e' in
    let e3 = subst_rv e3 rv e' in
    Eif (e1, e2, e3)
  | Elist es ->
    let es = List.map (fun e -> subst_rv e rv e') es in
    Elist es
  | Edistr d -> Edistr (subst_rv_distr d rv e')
and subst_rv_distr : abs_distribution -> RandomVar.t -> abs_expr -> abs_distribution =
fun d rv e' ->
  match d with
  | Dgaussian (e1, e2) ->
    let e1 = subst_rv e1 rv e' in
    let e2 = subst_rv e2 rv e' in
    Dgaussian (e1, e2)
  (* | DmvNormal of expr * expr *)
  | Dcategorical (e1, e2, e3) -> 
    let e1 = subst_rv e1 rv e' in
    let e2 = subst_rv e2 rv e' in
    let e3 = subst_rv e3 rv e' in
    Dcategorical (e1, e2, e3)
  | Dbeta (e1, e2) ->
    let e1 = subst_rv e1 rv e' in
    let e2 = subst_rv e2 rv e' in
    Dbeta (e1, e2)
  | Dbernoulli e ->
    let e = subst_rv e rv e' in
    Dbernoulli e
  | Dbinomial (e1, e2) -> 
    let e1 = subst_rv e1 rv e' in
    let e2 = subst_rv e2 rv e' in
    Dbinomial (e1, e2)
  | Dbetabinomial (e1, e2, e3) -> 
    let e1 = subst_rv e1 rv e' in
    let e2 = subst_rv e2 rv e' in
    let e3 = subst_rv e3 rv e' in
    Dbetabinomial (e1, e2, e3)
  | Dnegativebinomial (e1, e2) -> 
    let e1 = subst_rv e1 rv e' in
    let e2 = subst_rv e2 rv e' in
    Dnegativebinomial (e1, e2)
  | Dgamma (e1, e2) -> 
    let e1 = subst_rv e1 rv e' in
    let e2 = subst_rv e2 rv e' in
    Dgamma (e1, e2)
  | Dpoisson e -> 
    let e = subst_rv e rv e' in
    Dpoisson e
  | Dstudentt (e1, e2, e3) -> 
    let e1 = subst_rv e1 rv e' in
    let e2 = subst_rv e2 rv e' in
    let e3 = subst_rv e3 rv e' in
    Dstudentt (e1, e2, e3)
  | Ddelta e ->
    let e = subst_rv e rv e' in
    Ddelta e
  | Ddelta_sampled | Dunk -> d
  

exception NonConjugate of RandomVar.t

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
  fun rv status inf ->
    let status =
      match RVMap.find_opt rv inf with
      | Some old_status -> 
        (* Only update if greater *)
        if ApproximationStatus.compare status old_status = -1 then
          old_status
        else 
          status
      | None -> status
    in
    RVMap.add rv status inf

  let find : RandomVar.t -> t -> ApproximationStatus.t = RVMap.find

  let find_opt : RandomVar.t -> t -> ApproximationStatus.t option = RVMap.find_opt

  let rec add_patt : pattern -> ApproximationStatus.t -> t -> t =
  fun patt a inf ->
    match patt with
    | Pid id -> add id a inf
    | Ptuple ps -> List.fold_left (fun inf p -> add_patt p a inf) inf ps
    | Punit -> inf
    | Pany -> inf

  let to_string : t -> string =
  fun inf ->
    RVMap.fold (fun rv status acc ->
      Format.sprintf "%s%s: %s\n" acc (string_of_ident rv) (ApproximationStatus.to_string status)
    ) inf ""

  let verify : t -> t -> unit =
  fun ann inferred ->
    RVMap.iter (fun rv ann_status ->
      match find_opt rv inferred with
      | Some inferred_status ->
        if not (ApproximationStatus.verify ann_status inferred_status) then
          raise (Approximation_Status_Error (rv, ann_status, inferred_status))
      | None -> 
        (* Not found means never used so don't bother verify *)
        Format.printf "> Warning: %s is never used\n\n" (RandomVar.to_string rv);
        (* failwith (Format.sprintf "verify: can't find %s in inferred" (RandomVar.to_string rv)) *)
    ) ann
end

module SymState = struct
  type state = {
    name: PVSet.t;
    distr: abs_distribution;
  }

  type t = state RVMap.t
  let empty : t = RVMap.empty

  let mem : RandomVar.t -> t -> bool = RVMap.mem

  let find : RandomVar.t -> t -> state = RVMap.find

  let find_opt : RandomVar.t -> t -> state option = RVMap.find_opt
  
  let remove : RandomVar.t -> t -> t =
  fun rv s ->
    RVMap.remove rv s

  let add : RandomVar.t -> state -> t -> t = RVMap.add

  let equal : t -> t -> bool =
  fun g1 g2 ->
    RVMap.for_all (fun rv s ->
      begin match find_opt rv g2 with
      | Some s' -> PVSet.equal s.name s'.name && s.distr = s'.distr
      | None -> false
      end
    ) g1
    &&
    RVMap.for_all (fun rv s ->
      begin match find_opt rv g1 with
      | Some s' -> PVSet.equal s.name s'.name && s.distr = s'.distr
      | None -> false
      end
    ) g2

  let fold : (RandomVar.t -> state -> 'a -> 'a) -> t -> 'a -> 'a = RVMap.fold

  let map : (state -> state) -> t -> t = RVMap.map

  let filter : (RandomVar.t -> state -> bool) -> t -> t = RVMap.filter

  (* Helper functions *)

  let rec get_randomvars : t -> abs_expr -> bool -> RVSet.t =
  fun g e get_deltas ->
    let rec get_randomvars' : t -> abs_expr -> RVSet.t -> RVSet.t =
    fun g e rvs ->
      match e with
      | Econst _ | Eunk -> RVSet.empty
      | Erandomvar rv -> 
        begin match find_opt rv g with
        | Some s -> 
          let rvs = get_randomvars_distr g s.distr get_deltas in
          begin match s.distr with
          | Ddelta_sampled | Ddelta _ ->
            if get_deltas then RVSet.add rv rvs else rvs
          | _ -> RVSet.add rv rvs
          end
        | None -> rvs
          (* failwith (Format.sprintf "get_randomvars: can't find %s in g" (RandomVar.to_string rv)) *)
        end
      | Etuple es | Elist es -> 
        List.fold_left (fun acc e -> RVSet.union acc (get_randomvars' g e rvs)) RVSet.empty es
      | Eadd (e1, e2) | Emul (e1, e2) | Ediv (e1, e2) 
      | Eintadd (e1, e2) | Eintmul (e1, e2) | Ecmp (_, e1, e2) -> 
        RVSet.union (get_randomvars' g e1 rvs) (get_randomvars' g e2 rvs)
      | Eunop (_, e) | Einttofloat e -> get_randomvars' g e rvs
      | Eif (e1, e2, e3) -> 
        RVSet.union (get_randomvars' g e1 rvs) 
          (RVSet.union (get_randomvars' g e2 rvs) (get_randomvars' g e3 rvs))
      | Edistr d -> get_randomvars_distr g d get_deltas
    in
    get_randomvars' g e RVSet.empty
  and get_randomvars_distr : t -> abs_distribution -> bool -> RVSet.t =
  fun g d get_deltas ->
    match d with
    | Dgaussian (e1, e2) -> 
      RVSet.union (get_randomvars g e1 get_deltas) (get_randomvars g e2 get_deltas) 
    | Dcategorical (e1, e2, e3) -> 
      RVSet.union (get_randomvars g e1 get_deltas) 
        (RVSet.union (get_randomvars g e2 get_deltas) (get_randomvars g e3 get_deltas))
    | Dbeta (e1, e2) -> 
      RVSet.union (get_randomvars g e1 get_deltas) (get_randomvars g e2 get_deltas)
    | Dbernoulli e1 -> get_randomvars g e1 get_deltas
    | Dbinomial (e1, e2) -> 
      RVSet.union (get_randomvars g e1 get_deltas) (get_randomvars g e2 get_deltas)
    | Dbetabinomial (e1, e2, e3) | Dstudentt (e1, e2, e3) -> 
      RVSet.union (get_randomvars g e1 get_deltas) 
        (RVSet.union (get_randomvars g e2 get_deltas) (get_randomvars g e3 get_deltas))
    | Dnegativebinomial (e1, e2) -> 
      RVSet.union (get_randomvars g e1 get_deltas) (get_randomvars g e2 get_deltas)
    | Dgamma (e1, e2) -> 
      RVSet.union (get_randomvars g e1 get_deltas) (get_randomvars g e2 get_deltas)
    | Dpoisson e1 -> get_randomvars g e1 get_deltas
    | Ddelta e1 -> get_randomvars g e1 get_deltas
    | Ddelta_sampled -> RVSet.empty
    | Dunk -> RVSet.empty

  (* Garbage collect random variables if not referenced *)
  let rec clean : ctx -> abs_expr -> t -> t =
  fun ctx e g ->
    let used_rvs_e = get_randomvars g e true in
    let used_rvs_ctx = ctx_fold (fun _ e acc ->
      RVSet.union acc (get_randomvars g e true)
    ) ctx RVSet.empty in
    let used_rvs_g = RVMap.fold (fun _ s acc -> 
      let rvs = get_randomvars_distr g s.distr true in
      RVSet.union acc rvs
    ) g RVSet.empty in

    let used_rvs = RVSet.union used_rvs_e (RVSet.union used_rvs_ctx used_rvs_g) in
    let g' = RVMap.filter (fun rv _ -> RVSet.mem rv used_rvs) g in
    if g = g' then g else clean ctx e g'

  let rec rename : ctx -> abs_expr -> t -> RandomVar.t -> RandomVar.t -> ctx * abs_expr * t =
  fun ctx e g old_name new_name ->

    (* If old name is already in g, need to rename existing entry into 
       fresh name *)
    let ctx, e, g = 
      if mem new_name g then
        rename ctx e g new_name (get_temp ())
      else
        ctx, e, g
    in

    (* Rename RV refs in ctx *)
    Hashtbl.filter_map_inplace (fun _ e ->
      let e = rename_expr old_name new_name e in
      Some e
    ) ctx;

    (* Rename RV refs in e *)
    let e = rename_expr old_name new_name e in

    (* Rename entries and distributions in g *)
    let g =
      map (fun s ->
        let d = rename_distr old_name new_name s.distr in
        { s with distr = d }
      ) g
    in
    let g = 
      match find_opt old_name g with
      | Some s -> 
        remove old_name g |> add new_name s
      | None -> g
    in

    ctx, e, g

  let to_string : t -> string =
    fun g ->
      RVMap.fold (fun rv d acc ->
        Format.sprintf "%s%s: { %s; %s }\n" acc 
          (RandomVar.to_string rv) 
          (PVSet.fold (fun pv acc ->
            Format.sprintf "%s%s, " acc (ProgVar.to_string pv)) d.name "")
          (string_of_distribution d.distr)) g ""
end

(* Returns true if expression evaluates to a constant *)
let rec is_const : abs_expr -> SymState.t -> bool =
fun e g ->
  match e with
  | Econst _ -> true
  | Eunk -> false
  | Erandomvar rv' ->
    begin match (SymState.find rv' g).distr with
    | Ddelta _ | Ddelta_sampled -> true
    | _ -> false
    end
  | Etuple es | Elist es ->
    List.for_all (fun e -> is_const e g) es
  | Eadd (e1, e2) | Emul (e1, e2) | Ediv (e1, e2) 
  | Eintadd (e1, e2) | Eintmul (e1, e2) | Ecmp (_, e1, e2) ->
    is_const e1 g && is_const e2 g
  | Eunop (_, e1) | Einttofloat e1 ->
    is_const e1 g
  | Eif (e1, e2, e3) ->
    is_const e1 g && is_const e2 g && is_const e3 g
  | Edistr _ -> false

let rec replace_free_vars : abs_expr -> SymState.t -> abs_expr =
fun e g ->
  match e with
  | Econst _ | Eunk -> e
  | Erandomvar rv -> if SymState.mem rv g then e else Eunk
  | Etuple es -> Etuple (List.map (fun e -> replace_free_vars e g) es)
  | Elist es -> Elist (List.map (fun e -> replace_free_vars e g) es)
  | Eadd (e1, e2) ->
    let e1' = replace_free_vars e1 g in
    let e2' = replace_free_vars e2 g in
    Eadd (e1', e2')
  | Emul (e1, e2) ->
    let e1' = replace_free_vars e1 g in
    let e2' = replace_free_vars e2 g in
    Emul (e1', e2')
  | Ediv (e1, e2) ->
    let e1' = replace_free_vars e1 g in
    let e2' = replace_free_vars e2 g in
    Ediv (e1', e2')
  | Eintadd (e1, e2) ->
    let e1' = replace_free_vars e1 g in
    let e2' = replace_free_vars e2 g in
    Eintadd (e1', e2')
  | Eintmul (e1, e2) ->
    let e1' = replace_free_vars e1 g in
    let e2' = replace_free_vars e2 g in
    Eintmul (e1', e2')
  | Ecmp (cmp, e1, e2) ->
    let e1' = replace_free_vars e1 g in
    let e2' = replace_free_vars e2 g in
    Ecmp (cmp, e1', e2')
  | Eunop (op, e1) ->
    let e1' = replace_free_vars e1 g in
    Eunop (op, e1')
  | Einttofloat e1 ->
    let e1' = replace_free_vars e1 g in
    Einttofloat e1'
  | Eif (e1, e2, e3) ->
    let e1' = replace_free_vars e1 g in
    let e2' = replace_free_vars e2 g in
    let e3' = replace_free_vars e3 g in
    Eif (e1', e2', e3')
  | Edistr d -> Edistr (replace_free_vars_distr d g)
and replace_free_vars_distr : abs_distribution -> SymState.t -> abs_distribution =
fun d g ->
  match d with
  | Dgaussian (e1, e2) -> 
    let e1' = replace_free_vars e1 g in
    let e2' = replace_free_vars e2 g in
    Dgaussian (e1', e2')
  | Dcategorical (e1, e2, e3) -> 
    let e1' = replace_free_vars e1 g in
    let e2' = replace_free_vars e2 g in
    let e3' = replace_free_vars e3 g in
    Dcategorical (e1', e2', e3')
  | Dbeta (e1, e2) ->
    let e1' = replace_free_vars e1 g in
    let e2' = replace_free_vars e2 g in
    Dbeta (e1', e2')
  | Dbernoulli e ->
    let e' = replace_free_vars e g in
    Dbernoulli e'
  | Dbinomial (e1, e2) -> 
    let e1' = replace_free_vars e1 g in
    let e2' = replace_free_vars e2 g in
    Dbinomial (e1', e2')
  | Dbetabinomial (e1, e2, e3) -> 
    let e1' = replace_free_vars e1 g in
    let e2' = replace_free_vars e2 g in
    let e3' = replace_free_vars e3 g in
    Dbetabinomial (e1', e2', e3')    
  | Dnegativebinomial (e1, e2) -> 
    let e1' = replace_free_vars e1 g in
    let e2' = replace_free_vars e2 g in
    Dnegativebinomial (e1', e2')
  | Dgamma (e1, e2) -> 
    let e1' = replace_free_vars e1 g in
    let e2' = replace_free_vars e2 g in
    Dgamma (e1', e2')
  | Dpoisson e -> 
    let e' = replace_free_vars e g in
    Dpoisson e'
  | Dstudentt (e1, e2, e3) -> 
    let e1' = replace_free_vars e1 g in
    let e2' = replace_free_vars e2 g in
    let e3' = replace_free_vars e3 g in
    Dstudentt (e1', e2', e3')
  | Ddelta e ->
    let e' = replace_free_vars e g in
    Ddelta e'
  | Ddelta_sampled | Dunk -> d

(* 
  TODO: Widening can be even smarter... 
  curently has imprecision due to alias 
*)
let rec eval_if : ctx -> abs_expr -> abs_expr -> abs_expr -> SymState.t -> InferenceStrategy.t 
  -> ctx * abs_expr * SymState.t * InferenceStrategy.t =
fun ctx e1 e2 e3 g inf_strat ->
  match e1 with
  | Econst (Cbool true) -> ctx, e2, g, inf_strat
  | Econst (Cbool false) -> ctx, e3, g, inf_strat
  | _ ->
    (* Widen *)
    if not (is_const e1 g) then
      (* Make no changes *)
      ctx, Eif (e1, e2, e3), g, inf_strat
    else
      let ctx' = Hashtbl.copy ctx in
      let ctx', e23, g', inf_strat' = join_by_value ctx' e2 e3 g g inf_strat true in
      ctx', e23, g', inf_strat'

and eval_expr : ctx -> SymState.t -> abs_expr -> InferenceStrategy.t
  -> ctx * abs_expr * SymState.t * InferenceStrategy.t =
fun ctx g e inf_strat ->
  match e with
  | Erandomvar rv ->
    let s = SymState.find rv g in
    begin match s.distr with
    | Ddelta e -> eval_expr ctx g e inf_strat 
    | Ddelta_sampled -> ctx, Econst Cunk, g, inf_strat
    | _ -> ctx, e, g, inf_strat
    end
  | Etuple es ->
    let ctx, es, g, inf_strat = 
      List.fold_left (fun (ctx, es, g, inf_strat) e ->
        let ctx', e', g', inf_strat' = eval_expr ctx g e inf_strat  in
        ctx', e' :: es, g', inf_strat'
      ) (ctx, [], g, inf_strat) es
    in
    ctx, Etuple (List.rev es), g, inf_strat
  | Eadd (e1, e2) ->
    let ctx, e1, g1, inf_strat = eval_expr ctx g e1 inf_strat in
    let ctx, e2, g2, inf_strat = eval_expr ctx g1 e2 inf_strat in
    ctx, eval_add e1 e2, g2, inf_strat
  | Emul (e1, e2) ->
    let ctx, e1, g1, inf_strat = eval_expr ctx g e1 inf_strat in
    let ctx, e2, g2, inf_strat = eval_expr ctx g1 e2 inf_strat in
    ctx, eval_mul e1 e2, g2, inf_strat
  | Ediv (e1, e2) ->
    let ctx, e1, g1, inf_strat = eval_expr ctx g e1 inf_strat in
    let ctx, e2, g2, inf_strat = eval_expr ctx g1 e2 inf_strat in
    ctx, eval_div e1 e2, g2, inf_strat
  | Eintadd (e1, e2) ->
    let ctx, e1, g1, inf_strat = eval_expr ctx g e1 inf_strat in
    let ctx, e2, g2, inf_strat = eval_expr ctx g1 e2 inf_strat in
    ctx, eval_int_add e1 e2, g2, inf_strat
  | Eintmul (e1, e2) ->
    let ctx, e1, g1, inf_strat = eval_expr ctx g e1 inf_strat in
    let ctx, e2, g2, inf_strat = eval_expr ctx g1 e2 inf_strat in
    ctx, eval_int_mul e1 e2, g2, inf_strat
  | Einttofloat e1 ->
    let ctx, e1, g1, inf_strat = eval_expr ctx g e1 inf_strat in
    begin match e1 with
    | Econst (Cint c) -> ctx, Econst (Cfloat (float_of_int c)), g1, inf_strat
    | Econst Cunk -> ctx, Econst Cunk, g1, inf_strat
    | _ -> ctx, Einttofloat e1, g1, inf_strat
    end
  | Eunop (op, e1) ->
    let ctx, e1, g1, inf_strat = eval_expr ctx g e1 inf_strat in
    ctx, eval_unop op e1, g1, inf_strat
  | Ecmp (op, e1, e2) ->
    let ctx, e1, g1, inf_strat = eval_expr ctx g e1 inf_strat in
    let ctx, e2, g2, inf_strat = eval_expr ctx g1 e2 inf_strat in
    ctx, eval_cmp op e1 e2, g2, inf_strat
  | Eif (e1, e2, e3) ->
    let ctx, e1, g1, inf_strat = eval_expr ctx g e1 inf_strat in
    let ctx, e2, g2, inf_strat = eval_expr ctx g1 e2 inf_strat in
    let ctx, e3, g3, inf_strat = eval_expr ctx g2 e3 inf_strat in
     eval_if ctx e1 e2 e3 g3 inf_strat
  | Elist es ->
    let ctx, es, g, inf_strat = 
      List.fold_left (fun (ctx, es, g, inf_strat) e ->
        let ctx, e', g', inf_strat' = eval_expr ctx g e inf_strat in
        ctx, e' :: es, g', inf_strat'
      ) (ctx, [], g, inf_strat) es
    in
    ctx, Elist (List.rev es), g, inf_strat
  | Edistr d -> 
    let ctx, d, g, inf_strat = eval_distribution ctx g d inf_strat in
    ctx, Edistr d, g, inf_strat
  | Eunk | Econst _ -> ctx, e, g, inf_strat
and eval_distribution : ctx -> SymState.t -> abs_distribution -> InferenceStrategy.t 
-> ctx * abs_distribution * SymState.t * InferenceStrategy.t =
fun ctx g d inf_strat ->
  match d with
  | Dgaussian (e1, e2) ->
    let ctx, e1, g1, inf_strat = eval_expr ctx g e1 inf_strat  in
    let ctx, e2, g2, inf_strat = eval_expr ctx g1 e2 inf_strat  in
    ctx, Dgaussian (e1, e2), g2, inf_strat
  (* | DmvNormal of expr * expr *)
  | Dcategorical (e1, e2, e3) -> 
    let ctx, e1, g1, inf_strat = eval_expr ctx g e1 inf_strat  in
    let ctx, e2, g2, inf_strat = eval_expr ctx g1 e2 inf_strat  in
    let ctx, e3, g3, inf_strat = eval_expr ctx g2 e3 inf_strat  in
    ctx, Dcategorical (e1, e2, e3), g3, inf_strat
  | Dbeta (e1, e2) ->
    let ctx, e1, g1, inf_strat = eval_expr ctx g e1 inf_strat  in
    let ctx, e2, g2, inf_strat = eval_expr ctx g1 e2 inf_strat  in
    ctx, Dbeta (e1, e2), g2, inf_strat
  | Dbernoulli e ->
    let ctx, e, g, inf_strat = eval_expr ctx g e inf_strat  in
    ctx, Dbernoulli e, g, inf_strat
  | Dbinomial (e1, e2) -> 
    let ctx, e1, g1, inf_strat = eval_expr ctx g e1 inf_strat  in
    let ctx, e2, g2, inf_strat = eval_expr ctx g1 e2 inf_strat  in
    ctx, Dbinomial (e1, e2), g2, inf_strat
  | Dbetabinomial (e1, e2, e3) -> 
    let ctx, e1, g1, inf_strat = eval_expr ctx g e1 inf_strat  in
    let ctx, e2, g2, inf_strat = eval_expr ctx g1 e2 inf_strat  in
    let ctx, e3, g3, inf_strat = eval_expr ctx g2 e3 inf_strat  in
    ctx, Dbetabinomial (e1, e2, e3), g3, inf_strat
  | Dnegativebinomial (e1, e2) -> 
    let ctx, e1, g1, inf_strat = eval_expr ctx g e1 inf_strat  in
    let ctx, e2, g2, inf_strat = eval_expr ctx g1 e2 inf_strat  in
    ctx, Dnegativebinomial (e1, e2), g2, inf_strat
  | Dgamma (e1, e2) -> 
    let ctx, e1, g1, inf_strat = eval_expr ctx g e1 inf_strat  in
    let ctx, e2, g2, inf_strat = eval_expr ctx g1 e2 inf_strat  in
    ctx, Dgamma (e1, e2), g2, inf_strat
  | Dpoisson e -> 
    let ctx, e, g, inf_strat = eval_expr ctx g e inf_strat  in
    ctx, Dpoisson e, g, inf_strat
  | Dstudentt (e1, e2, e3) -> 
    let ctx, e1, g1, inf_strat = eval_expr ctx g e1 inf_strat  in
    let ctx, e2, g2, inf_strat = eval_expr ctx g1 e2 inf_strat  in
    let ctx, e3, g3, inf_strat = eval_expr ctx g2 e3 inf_strat  in
    ctx, Dstudentt (e1, e2, e3), g3, inf_strat
  | Ddelta e ->
    let ctx, e, g, inf_strat = eval_expr ctx g e inf_strat  in
    ctx, Ddelta e, g, inf_strat
  | Ddelta_sampled | Dunk -> ctx, d, g, inf_strat
and join_by_value : ctx -> abs_expr -> abs_expr -> SymState.t -> SymState.t -> InferenceStrategy.t -> bool
  -> ctx * abs_expr * SymState.t * InferenceStrategy.t =
fun ctx e1 e2 g1 g2 inf_strat use_old_name ->

  let mark_as_lost e g inf_strat =
    let rvs = SymState.get_randomvars g e false in
    RVSet.fold (fun rv inf_strat ->
      let s = SymState.find rv g in
      PVSet.fold (fun pv inf_strat ->
        InferenceStrategy.add pv Dynamic inf_strat
      ) s.name inf_strat
    ) rvs inf_strat
  in

  let g1_renamings = Hashtbl.create 10 in
  let g2_renamings = Hashtbl.create 10 in
  let old_renamings = Hashtbl.create 10 in
  
  let rec join_expr ctx e1 e2 g1 g2 g_acc inf_strat merge_distr =
    let ctx, e1, g1, inf_strat = eval_expr ctx g1 e1 inf_strat in
    let ctx, e2, g2, inf_strat = eval_expr ctx g2 e2 inf_strat in
    match e1, e2 with
    | Econst c1, Econst c2 ->
      let e = if c1 = c2 then Econst c1 else Econst Cunk in
      ctx, e, g_acc, inf_strat
    | Etuple es1, Etuple es2 ->
      let ctx, es, g_acc, inf_strat = List.fold_left2 (fun (ctx, es, g_acc, inf_strat) e1 e2 ->
        let ctx, e, g_acc, inf_strat = join_expr ctx e1 e2 g1 g2 g_acc inf_strat merge_distr in
        ctx, e :: es, g_acc, inf_strat
      ) (ctx, [], g_acc, inf_strat) es1 es2 in
      ctx, Etuple (List.rev es), g_acc, inf_strat
    | Eadd (e11, e12), Eadd (e21, e22) ->
      let ctx, e1, g_acc, inf_strat = join_expr ctx e11 e21 g1 g2 g_acc inf_strat merge_distr in
      let ctx, e2, g_acc, inf_strat = join_expr ctx e12 e22 g1 g2 g_acc inf_strat merge_distr in
      ctx, eval_add e1 e2, g_acc, inf_strat
    | Emul (e11, e12), Emul (e21, e22) ->
      let ctx, e1, g_acc, inf_strat = join_expr ctx e11 e21 g1 g2 g_acc inf_strat merge_distr in
      let ctx, e2, g_acc, inf_strat = join_expr ctx e12 e22 g1 g2 g_acc inf_strat merge_distr in
      ctx, eval_mul e1 e2, g_acc, inf_strat
    | Ediv (e11, e12), Ediv (e21, e22) ->
      let ctx, e1, g_acc, inf_strat = join_expr ctx e11 e21 g1 g2 g_acc inf_strat merge_distr in
      let ctx, e2, g_acc, inf_strat = join_expr ctx e12 e22 g1 g2 g_acc inf_strat merge_distr in
      ctx, eval_div e1 e2, g_acc, inf_strat
    | Eunop (u1, e1), Eunop (u2, e2) ->
      if u1 = u2 then 
        let ctx, e, g_acc, inf_strat = join_expr ctx e1 e2 g1 g2 g_acc inf_strat merge_distr in
        ctx, Eunop (u1, e), g_acc, inf_strat
      else 
        let inf_strat = mark_as_lost e1 g1 inf_strat in
        let inf_strat = mark_as_lost e2 g2 inf_strat in
        ctx, Eunk, g_acc, inf_strat
    | Einttofloat e1, Einttofloat e2 ->
      let ctx, e, g_acc, inf_strat = join_expr ctx e1 e2 g1 g2 g_acc inf_strat merge_distr in
      ctx, Einttofloat e, g_acc, inf_strat
    | Eif (e11, e12, e13), Eif (e21, e22, e23) ->
      let ctx, e1, g_acc, inf_strat = join_expr ctx e11 e21 g1 g2 g_acc inf_strat merge_distr in
      let ctx, e2, g_acc, inf_strat = join_expr ctx e12 e22 g1 g2 g_acc inf_strat merge_distr in
      let ctx, e3, g_acc, inf_strat = join_expr ctx e13 e23 g1 g2 g_acc inf_strat merge_distr in
      ctx, Eif (e1, e2, e3), g_acc, inf_strat
    | Elist es1, Elist es2 ->
      (* if List.length es1 <> List.length es2 then
        let inf_strat = mark_as_lost (Elist es1) g1 inf_strat in
        let inf_strat = mark_as_lost (Elist es2) g2 inf_strat in
        ctx, Eunk, g_acc, inf_strat
      else *)
      let rec traversal ctx l1 l2 es g_acc inf_strat =
        match l1, l2 with
        | [], [] -> ctx, es, g_acc, inf_strat
        | [], e2 -> 
          let inf_strat = mark_as_lost (Elist e2) g2 inf_strat in
          let es = match es with
          | Eunk :: es -> Eunk :: es
          | _ -> Eunk :: es
          in
          ctx, es, g_acc, inf_strat
        | e1, [] ->
          let inf_strat = mark_as_lost (Elist e1) g1 inf_strat in
          let es = match es with
          | Eunk :: es -> Eunk :: es
          | _ -> Eunk :: es
          in
          ctx, es, g_acc, inf_strat
        | e1 :: l1, e2 :: l2 ->
          let ctx, e, g_acc, inf_strat = join_expr ctx e1 e2 g1 g2 g_acc inf_strat merge_distr in
          traversal ctx l1 l2 (e :: es) g_acc inf_strat
      in
      let ctx, es, g_acc, inf_strat = traversal ctx es1 es2 [] g_acc inf_strat in
      ctx, Elist (List.rev es), g_acc, inf_strat
    | Edistr d1, Edistr d2 ->
      let ctx, d, g_acc, inf_strat = join_distribution ctx d1 d2 g1 g2 g_acc inf_strat in
      ctx, Edistr d, g_acc, inf_strat
    | Econst _, Erandomvar rv2 ->
      ctx, Erandomvar rv2, g_acc, inf_strat
    | Erandomvar rv1, Econst _ ->
      ctx, Erandomvar rv1, g_acc, inf_strat
    | Erandomvar rv1, Erandomvar rv2 ->
      if merge_distr then
        let new_var, s1, s2 = 
          if Hashtbl.mem g1_renamings rv1 && Hashtbl.mem g2_renamings rv2 then
            (* Both rv1 and rv2 have been renamed *)
            let new_var1 = Hashtbl.find g1_renamings rv1 in
            let new_var2 = Hashtbl.find g2_renamings rv2 in
            if new_var1 = new_var2 then
              (* Both rv1 and rv2 have been renamed to the same variable *)
              let s = SymState.find new_var1 g_acc in
              new_var1, s, s
            else
              (* Both rv1 and rv2 have been renamed to different variables *)
              let new_var = get_temp () in
              Hashtbl.replace g1_renamings rv1 new_var;
              Hashtbl.replace g2_renamings rv2 new_var;
              let s1 = SymState.find new_var1 g_acc in
              let s2 = SymState.find new_var2 g_acc in
              new_var, s1, s2
          else if Hashtbl.mem g1_renamings rv1 then
            (* only rv1 has been renamed *)
            let new_var = Hashtbl.find g1_renamings rv1 in
            Hashtbl.replace g2_renamings rv2 new_var;
            let s1 = SymState.find new_var g_acc in
            let s2 = SymState.find rv2 g2 in
            new_var, s1, s2
          else if Hashtbl.mem g2_renamings rv2 then
            (* only rv2 has been renamed *)
            let new_var = Hashtbl.find g2_renamings rv2 in
            Hashtbl.replace g1_renamings rv1 new_var;
            let s1 = SymState.find rv1 g1 in
            let s2 = SymState.find new_var g_acc in
            new_var, s1, s2
          else
            (* Neither rv1 nor rv2 have been renamed *)
            let new_var = get_temp () in
            Hashtbl.replace g1_renamings rv1 new_var;
            Hashtbl.replace g2_renamings rv2 new_var;
            let s1 = SymState.find rv1 g1 in
            let s2 = SymState.find rv2 g2 in
            new_var, s1, s2
        in
        
        let name = PVSet.union s1.name s2.name in
        let ctx, d, g_acc, inf_strat = join_distribution ctx s1.distr s2.distr g1 g2 g_acc inf_strat in

        let g_acc = SymState.add new_var { name = name; distr = d } g_acc in

        Hashtbl.replace old_renamings new_var rv1;

        ctx, Erandomvar new_var, g_acc, inf_strat
      else
        if rv1 = rv2 then ctx, Erandomvar rv1, g_acc, inf_strat
        else
          let inf_strat = mark_as_lost (Erandomvar rv1) g1 inf_strat in
          let inf_strat = mark_as_lost (Erandomvar rv2) g2 inf_strat in
          ctx, Eunk, g_acc, inf_strat
    | e1, e2 -> 
      let inf_strat = mark_as_lost e1 g1 inf_strat in
      let inf_strat = mark_as_lost e2 g2 inf_strat in
      ctx, Eunk, g_acc, inf_strat
  and join_distribution ctx d1 d2 g1 g2 g_acc inf_strat =
    match d1, d2 with
    | Dgaussian (e1, e2), Dgaussian (e1', e2') -> 
      let ctx, e1, g_acc, inf_strat = join_expr ctx e1 e1' g1 g2 g_acc inf_strat false in
      let ctx, e2, g_acc, inf_strat = join_expr ctx e2 e2' g1 g2 g_acc inf_strat false in
      ctx, Dgaussian (e1, e2), g_acc, inf_strat
    | Dcategorical (e1, e2, e3), Dcategorical (e1', e2', e3') ->
      let ctx, e1, g_acc, inf_strat = join_expr ctx e1 e1' g1 g2 g_acc inf_strat false in
      let ctx, e2, g_acc, inf_strat = join_expr ctx e2 e2' g1 g2 g_acc inf_strat false in
      let ctx, e3, g_acc, inf_strat = join_expr ctx e3 e3' g1 g2 g_acc inf_strat false in
      ctx, Dcategorical (e1, e2, e3), g_acc, inf_strat
    | Dbeta (e1, e2), Dbeta (e1', e2') ->
      let ctx, e1, g_acc, inf_strat = join_expr ctx e1 e1' g1 g2 g_acc inf_strat false in
      let ctx, e2, g_acc, inf_strat = join_expr ctx e2 e2' g1 g2 g_acc inf_strat false in
      ctx, Dbeta (e1, e2), g_acc, inf_strat
    | Dbernoulli e1, Dbernoulli e2 ->
      let ctx, e1, g_acc, inf_strat = join_expr ctx e1 e2 g1 g2 g_acc inf_strat false in
      ctx, Dbernoulli e1, g_acc, inf_strat
    | Dbinomial (e1, e2), Dbinomial (e1', e2') ->
      let ctx, e1, g_acc, inf_strat = join_expr ctx e1 e1' g1 g2 g_acc inf_strat false in
      let ctx, e2, g_acc, inf_strat = join_expr ctx e2 e2' g1 g2 g_acc inf_strat false in
      ctx, Dbinomial (e1, e2), g_acc, inf_strat
    | Dbetabinomial (e1, e2, e3), Dbetabinomial (e1', e2', e3') ->
      let ctx, e1, g_acc, inf_strat = join_expr ctx e1 e1' g1 g2 g_acc inf_strat false in
      let ctx, e2, g_acc, inf_strat = join_expr ctx e2 e2' g1 g2 g_acc inf_strat false in
      let ctx, e3, g_acc, inf_strat = join_expr ctx e3 e3' g1 g2 g_acc inf_strat false in
      ctx, Dbetabinomial (e1, e2, e3), g_acc, inf_strat
    | Dnegativebinomial (e1, e2), Dnegativebinomial (e1', e2') ->
      let ctx, e1, g_acc, inf_strat = join_expr ctx e1 e1' g1 g2 g_acc inf_strat false in
      let ctx, e2, g_acc, inf_strat = join_expr ctx e2 e2' g1 g2 g_acc inf_strat false in
      ctx, Dnegativebinomial (e1, e2), g_acc, inf_strat
    | Dgamma (e1, e2), Dgamma (e1', e2') ->
      let ctx, e1, g_acc, inf_strat = join_expr ctx e1 e1' g1 g2 g_acc inf_strat false in
      let ctx, e2, g_acc, inf_strat = join_expr ctx e2 e2' g1 g2 g_acc inf_strat false in
      ctx, Dgamma (e1, e2), g_acc, inf_strat
    | Dpoisson e1, Dpoisson e2 ->
      let ctx, e1, g_acc, inf_strat = join_expr ctx e1 e2 g1 g2 g_acc inf_strat false in
      ctx, Dpoisson e1, g_acc, inf_strat
    | Dstudentt (e1, e2, e3), Dstudentt (e1', e2', e3') ->
      let ctx, e1, g_acc, inf_strat = join_expr ctx e1 e1' g1 g2 g_acc inf_strat false in
      let ctx, e2, g_acc, inf_strat = join_expr ctx e2 e2' g1 g2 g_acc inf_strat false in
      let ctx, e3, g_acc, inf_strat = join_expr ctx e3 e3' g1 g2 g_acc inf_strat false in
      ctx, Dstudentt (e1, e2, e3), g_acc, inf_strat
    | Ddelta e1, Ddelta e2 ->
      let ctx, e1, g_acc, inf_strat = join_expr ctx e1 e2 g1 g2 g_acc inf_strat false in
      ctx, Ddelta e1, g_acc, inf_strat
    | Ddelta_sampled, Ddelta_sampled -> 
      ctx, Ddelta_sampled, g_acc, inf_strat
    | d1, d2 ->
      let inf_strat = mark_as_lost (Edistr d1) g1 inf_strat in
      let inf_strat = mark_as_lost (Edistr d2) g2 inf_strat in
      ctx, Dunk, g_acc, inf_strat
  in

  (* Format.printf "Joining %s and %s\n" (string_of_expr e1) (string_of_expr e2); *)
  (* Format.printf "g1:\n%s\n" (SymState.to_string g1); *)
  (* Format.printf "g2:\n%s\n" (SymState.to_string g2); *)
  (* Format.printf "ctx:\n%s\n" (string_of_ctx ctx); *)

  (* Join expression by value, and merging the random variables if 1-1 match at the top level. Save as new variable name *)
  let ctx, e, g_acc, inf_strat = join_expr ctx e1 e2 g1 g2 SymState.empty inf_strat true in

  (* Format.printf "Result: %s\n\n" (string_of_expr e); *)
  (* Format.printf "g_acc:\n%s\n" (SymState.to_string g_acc); *)
  (* Format.printf "g1 renamings:%s\n\n" (Hashtbl.fold (fun k v acc -> Format.sprintf "%s\n%s -> %s" acc (string_of_ident k) (string_of_ident v)) g1_renamings ""); *)
  (* Format.printf "g2 renamings:%s\n\n" (Hashtbl.fold (fun k v acc -> Format.sprintf "%s\n%s -> %s" acc (string_of_ident k) (string_of_ident v)) g2_renamings ""); *)

  (* Delete v1 in g1 and v2 in *)
  let g1 = Hashtbl.fold (fun old_name _ g1 ->
    SymState.remove old_name g1
  ) g1_renamings g1 in
  let g2 = Hashtbl.fold (fun old_name _ g2 ->
    SymState.remove old_name g2
  ) g2_renamings g2 in

  (* Format.printf "g1:\n%s\n" (SymState.to_string g1); *)
  (* Format.printf "g2:\n%s\n" (SymState.to_string g2); *)

  (* Rename given ctx with v1, v2 to new name *)
  let ctx = Hashtbl.fold (fun old_name new_name ctx ->
    Hashtbl.filter_map_inplace (fun _ e ->
      let e = rename_expr old_name new_name e in
      Some e
    ) ctx; ctx
  ) g1_renamings ctx in
  let ctx = Hashtbl.fold (fun old_name new_name ctx ->
    Hashtbl.filter_map_inplace (fun _ e ->
      let e = rename_expr old_name new_name e in
      Some e
    ) ctx; ctx
  ) g2_renamings ctx in

  (* Format.printf "ctx_renamed:\n%s\n" (string_of_ctx ctx); *)

  (* Rename references to v1, v2 in g1 and g2 to new_var *)
  let g1 = Hashtbl.fold (fun old_name new_name g1 ->
    SymState.map (fun s ->
      let d = rename_distr old_name new_name s.distr in
      { s with distr = d }
    ) g1
  ) g1_renamings g1 in
  let g2 = Hashtbl.fold (fun old_name new_name g2 ->
    SymState.map (fun s ->
      let d = rename_distr old_name new_name s.distr in
      { s with distr = d }
    ) g2
  ) g2_renamings g2 in

  (* Format.printf "g1_renamed:\n%s\n" (SymState.to_string g1); *)
  (* Format.printf "g2_renamed:\n%s\n" (SymState.to_string g2); *)
  
  (* Copy things in g1 and g2 to g_acc, merging by key *)
  let ctx, g_acc, inf_strat = SymState.fold (fun rv s1 (ctx, g_acc, inf_strat) ->
    match SymState.find_opt rv g_acc with
    | Some _ -> ctx, g_acc, inf_strat
    | None ->
      begin match SymState.find_opt rv g2 with
      | Some s2 -> 
        let used_rvs1 = SymState.get_randomvars g1 (Edistr s1.distr) true in
        let used_rvs2 = SymState.get_randomvars g2 (Edistr s2.distr) true in
        let g1 = SymState.filter (fun rv _ -> RVSet.mem rv used_rvs1) g1 in
        let g2 = SymState.filter (fun rv _ -> RVSet.mem rv used_rvs2) g2 in

        let names = PVSet.union s1.name s2.name in
        let ctx, d, g_acc, inf_strat = join_distribution ctx s1.distr s2.distr g1 g2 g_acc inf_strat in
        ctx, SymState.add rv {name=names; distr=d} g_acc, inf_strat
      | None -> ctx, SymState.add rv s1 g_acc, inf_strat
      end
  ) g1 (ctx, g_acc, inf_strat) in
  
  let g_acc = SymState.fold (fun rv s2 g_acc ->
    match SymState.find_opt rv g_acc with
    | Some _ -> g_acc
    | None ->
      begin match SymState.find_opt rv g1 with
      | Some _ -> failwith (Format.sprintf "join_symstate_by_keys: should've already added %s to g_acc already" (RandomVar.to_string rv))
      | None -> SymState.add rv s2 g_acc
      end
  ) g2 g_acc in
  
  (* Format.printf "g_acc:\n%s\n" (SymState.to_string g_acc); *)

  let ctx, e, g_acc = if use_old_name then
    (* Rename from old_renamings everywhere *)

    (* let _ = Format.printf "old_renamings:%s\n\n" (Hashtbl.fold (fun k v acc -> Format.sprintf "%s\n%s -> %s" acc (string_of_ident k) (string_of_ident v)) old_renamings "") in *)

    Hashtbl.fold (fun old_name new_name (ctx, e, g_acc) ->
      SymState.rename ctx e g_acc old_name new_name
    ) old_renamings (ctx, e, g_acc)
    
  else ctx, e, g_acc in
  
  (* Format.printf "ctx:\n%s\n" (string_of_ctx ctx); *)
  (* Format.printf "e: %s\n\n" (string_of_expr e); *)
  (* Format.printf "g_acc:\n%s\n" (SymState.to_string g_acc); *)

  let g_acc = SymState.clean ctx e g_acc in

  (* Format.printf "g_acc_clean:\n%s\n" (SymState.to_string g_acc); *)

  (* Format.printf "Done joining\n\n"; *)

  ctx, e, g_acc, inf_strat

module AbstractSSI = struct

  let intervene : ctx -> InferenceStrategy.t -> RandomVar.t -> abs_distribution -> SymState.t -> ctx * InferenceStrategy.t * SymState.t =
  fun ctx inf_strat rv d g ->
    let ctx, d, g, inf_strat = eval_distribution ctx g d inf_strat in
    let inf_strat =
      match d with
      | Ddelta_sampled -> 
        let s = SymState.find rv g in
        PVSet.fold (fun x inf_strat ->
          InferenceStrategy.add x Approx inf_strat  
        ) s.name inf_strat
      | _ -> inf_strat
    in
    let s = SymState.find rv g in
    let g = SymState.add rv {s with distr=d} g in
    ctx, inf_strat, g

  let is_member_list : RandomVar.t -> RandomVar.t list -> SymState.t -> bool =
  fun rv rvs g ->
    List.exists (fun rv' -> (SymState.find rv g).distr == (SymState.find rv' g).distr) rvs

  let is_member_set : RandomVar.t -> RVSet.t -> SymState.t -> bool =
  fun rv rvs g ->
    RVSet.exists (fun rv' -> (SymState.find rv g).distr == (SymState.find rv' g).distr) rvs

  (* Returns true if expression e depends on random variable rv *)
  let rec depends_on : abs_expr -> RandomVar.t -> SymState.t -> bool -> bool -> bool =
  fun e rv g transitive conservative ->
    match e with
    | Econst _ -> false
    | Eunk -> conservative
    | Erandomvar rv' ->
      if is_const e g then false
      else
        if SymState.find rv g == SymState.find rv' g then 
          true
        else
          if transitive then
            let d = SymState.find rv' g in
            depends_on_distribution d.distr rv g transitive conservative
          else false
    | Etuple es | Elist es ->
      List.exists (fun e -> depends_on e rv g transitive conservative) es
    | Eadd (e1, e2) | Emul (e1, e2) | Ediv (e1, e2)
    | Eintadd (e1, e2) | Eintmul (e1, e2) | Ecmp (_, e1, e2) ->
      depends_on e1 rv g transitive conservative || depends_on e2 rv g transitive conservative
    | Eunop (_, e1) | Einttofloat e1 ->
      depends_on e1 rv g transitive conservative
    | Eif (e1, e2, e3) ->
      depends_on e1 rv g transitive conservative || 
      depends_on e2 rv g transitive conservative || 
      depends_on e3 rv g transitive conservative
    | Edistr d ->
      depends_on_distribution d rv g transitive conservative
  and depends_on_distribution : abs_distribution -> RandomVar.t -> SymState.t -> bool -> bool -> bool =
  fun d rv g transitive conservative ->
    match d with
    | Dgaussian (e1, e2) | Dbeta (e1, e2) | Dbinomial (e1, e2) 
    | Dnegativebinomial (e1, e2) | Dgamma (e1, e2) ->
      depends_on e1 rv g transitive conservative || depends_on e2 rv g transitive conservative
    (* | DmvNormal of expr * expr *)
    | Dcategorical (e1, e2, e3) | Dbetabinomial (e1, e2, e3) | Dstudentt (e1, e2, e3) -> 
      depends_on e1 rv g transitive conservative || 
      depends_on e2 rv g transitive conservative || 
      depends_on e3 rv g transitive conservative
    | Dbernoulli e | Dpoisson e | Ddelta e ->
      depends_on e rv g transitive conservative
    | Ddelta_sampled -> false
    | Dunk -> conservative

  let rec indirectly_depends_on : abs_expr -> RandomVar.t -> SymState.t -> bool =
  fun e rv g ->
    match e with
    | Econst _ -> false
    | Eunk -> false
    | Erandomvar rv' ->
      if is_const e g then false
      else
        if not (SymState.find rv g == SymState.find rv' g) then 
          let d = SymState.find rv' g in
          indirectly_depends_on_distribution d.distr rv g
        else
          false
    | Etuple es | Elist es ->
      List.exists (fun e -> indirectly_depends_on e rv g) es
    | Eadd (e1, e2) | Emul (e1, e2) | Ediv (e1, e2)
    | Eintadd (e1, e2) | Eintmul (e1, e2) | Ecmp (_, e1, e2) ->
      indirectly_depends_on e1 rv g || indirectly_depends_on e2 rv g
    | Eunop (_, e1) | Einttofloat e1 ->
      indirectly_depends_on e1 rv g
    | Eif (e1, e2, e3) ->
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
      depends_on e1 rv g true false || depends_on e2 rv g true false
    (* | DmvNormal of expr * expr *)
    | Dcategorical (e1, e2, e3) | Dbetabinomial (e1, e2, e3) | Dstudentt (e1, e2, e3) -> 
      depends_on e1 rv g true false || 
      depends_on e2 rv g true false || 
      depends_on e3 rv g true false
    | Dbernoulli e | Dpoisson e | Ddelta e ->
      depends_on e rv g true false
    | Ddelta_sampled -> false
    | Dunk -> false
  
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
        Some (eval_add a1 a2, eval_add b1 b2)
      | _ -> None
      end
    | Emul (e1, e2) ->
      begin match is_affine e1 rv g, is_affine e2 rv g with
      | Some (a1, b1), Some(a2, b2) ->
        begin match a1, a2 with
        | Econst (Cfloat 0.), Econst (Cfloat 0.) ->
          Some (Econst (Cfloat 0.), eval_mul b1 b2)
        | a1, Econst (Cfloat 0.) ->
          Some (eval_mul a1 b2, eval_mul b1 b2)
        | Econst (Cfloat 0.), a2 ->
          Some (eval_mul b1 a2, eval_mul b1 b2)
        | _ -> None
        end
      | _ -> None
      end
    | Ediv (e1, e2) ->
      begin match is_affine e1 rv g, is_affine e2 rv g with
      | Some (a1, b1), Some (a2, b2) ->
        begin match a2 with
        | Econst (Cfloat 0.) -> Some(eval_div a1 b2, eval_div b1 b2)
        | _ -> None
        end
      | _ -> None
      end
    | Eunop (op, e1) ->
      begin match is_affine e1 rv g with
      | Some(a, b) ->
        begin match a with
        | Econst (Cfloat 0.) -> Some (Econst (Cfloat 0.), eval_unop op b)
        | _ -> None
        end
      | _ -> None
      end
    | Ecmp _ | Eintadd _ | Eintmul _ -> None
    | Eif _ -> None
    | Etuple _ | Elist _ | Edistr _ -> None
    | Einttofloat _ -> None
    | Eunk -> 
      (* Conservatively assume it is not affine wrt rv *)
      None

  let get_parents : RandomVar.t -> SymState.t -> RandomVar.t list =
  fun rv g ->
    let rec get_parents_expr : abs_expr -> RandomVar.t list =
    fun e ->
      match e with
      | Erandomvar rv -> if is_const e g then [] else [rv]
      | Eadd (e1, e2) | Emul (e1, e2) | Ediv (e1, e2)
      | Eintadd (e1, e2) | Eintmul (e1, e2) | Ecmp (_, e1, e2) -> 
        List.append (get_parents_expr e1) (get_parents_expr e2)
      | Eunop (_, e1) | Einttofloat e1 -> get_parents_expr e1
      | Eif (e1, e2, e3) -> 
        List.append (List.append (get_parents_expr e1) (get_parents_expr e2)) (get_parents_expr e3)
      | Elist es | Etuple es ->
        List.fold_left (fun acc e -> List.append acc (get_parents_expr e)) [] es
      | Econst _ | Eunk -> []
      | Edistr _ -> failwith "get_parents_expr: unexpected expression"
    in
    
    match (SymState.find rv g).distr with
    | Dgaussian (e1, e2) | Dbeta (e1, e2) | Dbinomial (e1, e2) | Dnegativebinomial (e1, e2) 
    | Dgamma (e1, e2) ->
      List.append (get_parents_expr e1) (get_parents_expr e2)
    (* | DmvNormal of expr * expr *)
    | Dcategorical (e1, e2, e3) | Dbetabinomial (e1, e2, e3) | Dstudentt (e1, e2, e3) -> 
      List.append (List.append (get_parents_expr e1) (get_parents_expr e2)) (get_parents_expr e3)
    | Dbernoulli e | Dpoisson e | Ddelta e ->
      get_parents_expr e
    | Ddelta_sampled | Dunk -> [] 

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
    match (SymState.find rv_child g).distr with
    | Dgaussian (e1, e2) | Dbeta (e1, e2) | Dbinomial (e1, e2) 
    | Dnegativebinomial (e1, e2) | Dgamma (e1, e2) ->
      ((depends_on e1 rv_parent g false false) ||
      (depends_on e2 rv_parent g false false)) &&
      (not (indirectly_depends_on e1 rv_parent g)) &&
      (not (indirectly_depends_on e2 rv_parent g))
    (* | DmvNormal of expr * expr *)
    | Dcategorical (e1, e2, e3) | Dbetabinomial (e1, e2, e3) | Dstudentt (e1, e2, e3) -> 
      ((depends_on e1 rv_parent g false false) ||
      (depends_on e2 rv_parent g false false) ||
      (depends_on e3 rv_parent g false false)) &&
      (not (indirectly_depends_on e1 rv_parent g)) &&
      (not (indirectly_depends_on e2 rv_parent g)) &&
      (not (indirectly_depends_on e3 rv_parent g))
    | Dbernoulli e | Dpoisson e | Ddelta e ->
      (depends_on e rv_parent g false false) 
      &&
      (not (indirectly_depends_on e rv_parent g))
    | Ddelta_sampled -> 
      (* Never needs to do this, but technically swapping with a constant
         never creates a cycle *)
      true
    | Dunk -> 
      (* Overapproximating by assuming it doesn't create a cycle *)
      true

  let gaussian_marginal : RandomVar.t -> RandomVar.t -> abs_distribution -> abs_distribution -> SymState.t -> abs_distribution option =
  fun rv1 rv2 d1 d2 g ->
    match d1, d2 with
    | (Dgaussian(mu_0, var_0), Dgaussian(mu, var)) ->
      begin match is_affine mu rv1 g with
      | Some(a, b) ->
        if (not (depends_on mu_0 rv2 g true true)) &&
            (not (depends_on var_0 rv2 g true true)) &&
            (not (depends_on var rv1 g true true)) then
          let mu' = Eadd ((Emul (a, mu_0)), b) in
          let var' = Eadd ((Emul(Eunop (Squared, a), var_0)), var) in
          Some(Dgaussian(mu', var'))
        else
          None
      | None -> None
      end
    | _ -> None

  let gaussian_posterior : RandomVar.t -> RandomVar.t -> abs_distribution -> abs_distribution -> SymState.t -> abs_distribution option =
  fun rv1 rv2 d1 d2 g ->
    match d1, d2 with
    | (Dgaussian(mu_0, var_0), Dgaussian(mu, var)) ->
      begin match is_affine mu rv1 g with
      | Some(a, b) ->
        if (not (depends_on mu_0 rv2 g true true)) &&
          (not (depends_on var_0 rv2 g true true)) &&
          (not (depends_on var rv1 g true true)) then

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

  let beta_bernoulli_marginal : RandomVar.t -> RandomVar.t -> abs_distribution -> abs_distribution -> SymState.t -> abs_distribution option =
  fun rv1 rv2 d1 d2 g ->
    match d1, d2 with
    | Dbeta(a, b), Dbernoulli(Erandomvar rv) ->
      if rv = rv1 &&
        (not (depends_on a rv2 g true true)) &&
        (not (depends_on b rv2 g true true)) 
      then
        Some(Dbernoulli(Ediv(a, Eadd(a, b))))
      else
        None
  | _ -> None

  let beta_bernoulli_posterior : RandomVar.t -> RandomVar.t -> abs_distribution -> abs_distribution -> SymState.t -> abs_distribution option =
  fun rv1 rv2 d1 d2 g ->
    match d1, d2 with
    | Dbeta(a, b), Dbernoulli(Erandomvar rv) ->
      if rv = rv1 &&
        (not (depends_on a rv2 g true true)) &&
        (not (depends_on b rv2 g true true)) 
      then
        Some(Dbeta(Eadd(a, Eif(Erandomvar rv2, Econst(Cfloat 1.), Econst(Cfloat 0.))),
          Eadd(b, Eif(Erandomvar(rv2), Econst(Cfloat 0.), Econst(Cfloat 1.)))))
      else
        None
    | _ -> None

  let beta_binomial_marginal : RandomVar.t -> RandomVar.t -> abs_distribution -> abs_distribution -> SymState.t -> abs_distribution option =
  fun rv1 rv2 d1 d2 g ->
    match d1, d2 with
    | Dbeta(a, b), Dbinomial(Econst n, Erandomvar rv) ->
      if rv = rv1 &&
        (not (depends_on a rv2 g true true)) &&
        (not (depends_on b rv2 g true true))
      then
        Some(Dbetabinomial(Econst n, a, b))
      else
        None
    | _ -> None

  let beta_binomial_posterior : RandomVar.t -> RandomVar.t -> abs_distribution -> abs_distribution -> SymState.t -> abs_distribution option =
  fun rv1 rv2 d1 d2 g ->
    match d1, d2 with
    | Dbeta(a, b), Dbinomial(Econst n, Erandomvar rv) ->
      if rv = rv1 &&
        (not (depends_on a rv2 g true true)) &&
        (not (depends_on b rv2 g true true))
      then
        Some(Dbeta(Eadd(a, Einttofloat(Erandomvar rv2)),
          Eadd(b, Eadd(Einttofloat(Econst n), Emul(Econst (Cfloat (-1.)), Einttofloat(Erandomvar rv2))))))
      else
        None
  | _ -> None

  let gamma_poisson_marginal : RandomVar.t -> RandomVar.t -> abs_distribution -> abs_distribution -> SymState.t -> abs_distribution option =
  fun rv1 rv2 d1 d2 g ->
    match d1, d2 with
    | Dgamma(Econst a, b), Dpoisson(Erandomvar rv) ->
      if rv = rv1 && 
        (not (depends_on b rv2 g true true)) 
      then
        let a = 
          match a with
          | Cfloat a ->  Cint (int_of_float a)
          | Cunk -> Cunk
          | _ -> failwith "gamma_poisson_marginal: a is not a float"
        in
        Some(Dnegativebinomial(Econst a, Ediv(b, Eadd(Econst(Cfloat 1.), b))))
     else
        None
    | _ -> None
  
  let gamma_poisson_posterior : RandomVar.t -> RandomVar.t -> abs_distribution -> abs_distribution -> SymState.t -> abs_distribution option =
  fun rv1 rv2 d1 d2 g ->
    match d1, d2 with
    | Dgamma(Econst a, b), Dpoisson(Erandomvar rv) ->
      if rv = rv1 && 
        (not (depends_on b rv2 g true true))
      then
        let a = 
          match a with
          | Cfloat a ->  Cint (int_of_float a)
          | Cunk -> Cunk
          | _ -> failwith "gamma_poisson_marginal: a is not a float"
        in
        Some(Dgamma(Eadd(Econst a, Einttofloat(Erandomvar rv2)), Eadd(b, Einttofloat(Econst(Cint 1)))))
     else
        None
    | _ -> None

  let gamma_normal_marginal : RandomVar.t -> RandomVar.t -> abs_distribution -> abs_distribution -> SymState.t -> abs_distribution option =
  fun rv1 rv2 d1 d2 g ->
    match d1, d2 with
    | Dgamma(a, b), Dgaussian(Econst mu, Ediv(Econst(Cfloat 1.), Erandomvar(rv))) ->
      if rv == rv1 &&
        (not (depends_on a rv2 g true true)) &&
        (not (depends_on b rv2 g true true))
      then
        Some(Dstudentt(Econst mu, Ediv(b, a), Emul(Econst(Cfloat 2.), a)))
      else 
        None
    | _ -> None

  let gamma_normal_posterior : RandomVar.t -> RandomVar.t -> abs_distribution -> abs_distribution -> SymState.t -> abs_distribution option =
    fun rv1 rv2 d1 d2 g ->
      match d1, d2 with
    | Dgamma(a, b), Dgaussian(Econst mu, Ediv(Econst(Cfloat 1.), Erandomvar(rv))) ->
      if rv == rv1 &&
        (not (depends_on a rv2 g true true)) &&
        (not (depends_on b rv2 g true true))
      then
        let a' = Eadd(a, Econst (Cfloat 0.5)) in
        let b' = Eadd(b, Emul(Econst (Cfloat 0.5),
          Eunop(Squared, Eadd(Erandomvar(rv2), Emul(Econst(Cfloat (-1.)), Econst mu))))) in
        Some (Dgamma(a', b'))
      else 
        None
    | _ -> None

  let bernoulli_marginal : RandomVar.t -> RandomVar.t -> abs_distribution -> abs_distribution -> SymState.t -> abs_distribution option =
  fun rv1 rv2 d1 d2 g ->
    match d1, d2 with
    | Dbernoulli p1, Dbernoulli p2 ->
      if depends_on p2 rv1 g false true &&
        (not (depends_on p1 rv2 g true true))
      then
        let p2' = Eadd(Emul(p1, subst_rv p2 rv1 (Econst (Cbool true))),
          Emul(Eadd(Econst (Cfloat 1.), Emul(Econst(Cfloat (-1.)), p1)), subst_rv p2 rv1 (Econst (Cbool false)))) in
        Some(Dbernoulli p2')
      else
        None
    | _ -> None

  let bernoulli_posterior : RandomVar.t -> RandomVar.t -> abs_distribution -> abs_distribution -> SymState.t -> abs_distribution option =
  fun rv1 rv2 d1 d2 g ->
    match d1, d2 with
    | Dbernoulli p1, Dbernoulli p2 ->
      if depends_on p2 rv1 g false true &&
        (not (depends_on p1 rv2 g true true))
      then
        let p2' = Eadd(Emul(p1, subst_rv p2 rv1 (Econst (Cbool true))),
          Emul(Eadd(Econst (Cfloat 1.), Emul(Econst(Cfloat (-1.)), p1)), subst_rv p2 rv1 (Econst (Cbool false)))) in
        
        let p1'_num_sub = Eif (Erandomvar rv2, p2, Eadd(Econst (Cfloat 1.), Emul(Econst(Cfloat (-1.)), p2))) in
        let p1'_num = Emul(p1, subst_rv p1'_num_sub rv1 (Econst (Cbool true))) in
        let p1'_denom = Eif (Erandomvar rv2, p2', Eadd(Econst (Cfloat 1.), Emul(Econst(Cfloat (-1.)), p2'))) in
        Some(Dbernoulli (Ediv(p1'_num, p1'_denom)))
      else
        None
    | _ -> None

  let swap : ctx -> InferenceStrategy.t -> RandomVar.t -> RandomVar.t -> SymState.t 
    -> ctx * bool * InferenceStrategy.t * SymState.t =
  fun ctx inf_strat rv1 rv2 g ->
    let d1 = (SymState.find rv1 g).distr in
    let d2 = (SymState.find rv2 g).distr in
    
    let ctx, d1, g, inf_strat = eval_distribution ctx g d1 inf_strat in
    let ctx, d2, g, inf_strat = eval_distribution ctx g d2 inf_strat in
    let dist_marg, dist_post = 
    match d1, d2 with
      | Dgaussian (_, _), Dgaussian (_, _) ->
        gaussian_marginal rv1 rv2 d1 d2 g, gaussian_posterior rv1 rv2 d1 d2 g
      | Dbeta(_, _), Dbernoulli(_) ->
        beta_bernoulli_marginal rv1 rv2 d1 d2 g, beta_bernoulli_posterior rv1 rv2 d1 d2 g
      | Dbeta(_, _), Dbinomial(_, _) ->
        beta_binomial_marginal rv1 rv2 d1 d2 g, beta_binomial_posterior rv1 rv2 d1 d2 g
      | Dgamma(_, _), Dpoisson(_) ->
        gamma_poisson_marginal rv1 rv2 d1 d2 g, gamma_poisson_posterior rv1 rv2 d1 d2 g
      | Dgamma(_, _), Dgaussian(_, _) ->
        (* let () = if !i >= 0 then exit(1) else incr i in *)
        gamma_normal_marginal rv1 rv2 d1 d2 g, gamma_normal_posterior rv1 rv2 d1 d2 g
      | Dbernoulli _, Dbernoulli _ ->
        bernoulli_marginal rv1 rv2 d1 d2 g, bernoulli_posterior rv1 rv2 d1 d2 g
      | _ -> None, None
    in
    match dist_marg, dist_post with
    | Some(dist_marg), Some(dist_post) ->
      let ctx, inf_strat, g = intervene ctx inf_strat rv2 dist_marg g in
      let ctx, inf_strat, g = intervene ctx inf_strat rv1 dist_post g in
      ctx, true, inf_strat, g
    | _ -> ctx, false, inf_strat, g

  let hoist : ctx -> InferenceStrategy.t -> RandomVar.t -> SymState.t -> ctx * InferenceStrategy.t * SymState.t =
  fun ctx inf_strat rv g ->
    let rec hoist_inner : ctx -> InferenceStrategy.t -> RandomVar.t -> RVSet.t -> SymState.t 
      -> ctx * InferenceStrategy.t * SymState.t =
    fun ctx inf_strat rv_child ghost_roots g ->
      let parents = List.rev (topo_sort (get_parents rv_child g) g) in

      (* Format.printf "Hoisting %s with ghost roots [%s] and parents [%s]\n" (RandomVar.to_string rv_child)
        (String.concat ", " (List.map RandomVar.to_string (RVSet.elements ghost_roots)))
        (String.concat ", " (List.map RandomVar.to_string parents)); *)

      (* Format.printf "%s\n" (SymState.to_string g); *)

      let rec hoist_parents : ctx -> InferenceStrategy.t -> RandomVar.t list -> RVSet.t -> SymState.t 
        -> ctx * InferenceStrategy.t * SymState.t =
      fun ctx inf_strat parents ghost_roots g ->
        match parents with
        | [] -> ctx, inf_strat, g
        | rv_parent :: rvs ->
          (* Format.printf "Got parent: %s\n" (RandomVar.to_string rv_parent); *)
          let ctx, inf_strat, g = 
            if not (is_member_set rv_parent ghost_roots g) then
              (* let _ = Format.printf "Recursing into %s\n" (RandomVar.to_string rv_parent) in *)
              hoist_inner ctx inf_strat rv_parent ghost_roots g
            else ctx, inf_strat, g
          in
          hoist_parents ctx inf_strat rvs (RVSet.add rv_parent ghost_roots) g
      in

      let rec swap_with_parents : ctx -> InferenceStrategy.t -> RandomVar.t list -> SymState.t 
        -> ctx * InferenceStrategy.t * SymState.t =
      fun ctx inf_strat parents g ->
        match parents with
        | [] -> ctx, inf_strat, g
        | rv_parent :: rvs ->
          if not (is_member_set rv_parent ghost_roots g) then
            begin 
            (* let () = Format.printf "Swapping %s with %s\n" (RandomVar.to_string rv_parent) (RandomVar.to_string rv_child) in *)

            (* Behavior is be less conservative on whether a swap is illegal
               and be more conservative on whether a swap is possible
               when encountering unknown expressions and distributions *)
            (if not (can_swap rv_parent rv_child g) then
              (* let () = 
                Format.printf "parent %s; parent's parents: [%s]\n" (RandomVar.to_string rv_parent)
                  (String.concat ", " (List.map RandomVar.to_string (get_parents rv_parent g)))
              in *)
              (* let () = 
                Format.printf "child %s; child's parents: [%s]\n" (RandomVar.to_string rv_child)
                  (String.concat ", " (List.map RandomVar.to_string (get_parents rv_child g)))
              in *)
              (* let sym_state_s = SymState.to_string g in
              Format.printf "%s\n" sym_state_s; *)
              failwith (Format.sprintf "Cannot swap parent %s and child %s" 
                (RandomVar.to_string rv_parent) (RandomVar.to_string rv_child) ));

            let s_parent = SymState.find rv_parent g in
            begin match s_parent.distr with
            | Dunk -> 
              let inf_strat = PVSet.fold (fun pv inf_strat ->
                let inf_strat = InferenceStrategy.add pv Dynamic inf_strat in
                inf_strat
              ) s_parent.name inf_strat in
              ctx, inf_strat, g
            | _ ->
              (* Format.printf "Starting swap\n"; *)
              
              let ctx, did_swap, inf_strat, g = swap ctx inf_strat rv_parent rv_child g in
              if did_swap then
                (* let _ = 
                  Format.printf "Swapped %s and %s\n" (RandomVar.to_string rv_parent) (RandomVar.to_string rv_child)
                in *)
                let res = swap_with_parents ctx inf_strat rvs g in 
                res
              else 
                (* let _ = Format.printf "%s\n" (SymState.to_string g) in *)
                (* let _ = 
                  Format.printf "Failed to swap %s and %s\n" (RandomVar.to_string rv_parent) (RandomVar.to_string rv_child) in *)
                raise (NonConjugate rv_parent)
              end
            end
          else
            (* let _ = Format.printf "Parent %s is a ghost root\n" (RandomVar.to_string rv_parent) in *)
            swap_with_parents ctx inf_strat rvs g
      in

      let ctx, inf_strat, g = hoist_parents ctx inf_strat parents ghost_roots g in
      (* Format.printf "Done hoisting parents for %s\n" (RandomVar.to_string rv_child); *)
      let parents = List.rev parents in
      (* Format.printf "Begin swapping child %s with parents [%s]\n" (RandomVar.to_string rv_child)
        (String.concat ", " (List.map RandomVar.to_string parents)); *)
      let ctx, inf_strat, g = swap_with_parents ctx inf_strat parents g in
      (* Format.printf "Done hoisting %s\n" (RandomVar.to_string rv_child); *)

      ctx, inf_strat, g
    in

    hoist_inner ctx inf_strat rv RVSet.empty g
  
  (* Abstract Semi-symbolic inference interface *)

  let assume : ctx -> InferenceStrategy.t -> ProgVar.t -> abs_expr -> SymState.t -> 
    ctx * RandomVar.t * InferenceStrategy.t * SymState.t =
  fun ctx inf_strat x e g ->
    match e with
    | Edistr d -> 
      let varname = get_temp () in
      let ctx, d, g, inf_strat = eval_distribution ctx g d inf_strat in
      let g = SymState.add varname { name = PVSet.singleton x; distr = d } g in
      let inf_strat = 
        if not (x.modul = Some "Temp") then 
          InferenceStrategy.add x Exact inf_strat
        else
          inf_strat
      in
      ctx, varname, inf_strat, g
    | _ -> failwith "SymState.add: Not a distribution"

  let rec value : ctx -> InferenceStrategy.t -> identifier -> SymState.t -> ctx * InferenceStrategy.t * SymState.t =
  fun ctx inf_strat rv g ->
    try 
      let ctx, inf_strat, g = hoist ctx inf_strat rv g in
      (* Format.printf "Hoisted!\n"; *)
      let ctx, inf_strat, g = intervene ctx inf_strat rv Ddelta_sampled g in
      (* Format.printf "Intervened!\n"; *)
      ctx, inf_strat, g
    with NonConjugate rv_parent ->
      (* Format.printf "Value Non-conjugate: %s\n" (RandomVar.to_string rv_parent); *)
      let ctx, inf_strat, g' = value ctx inf_strat rv_parent g in
      value ctx inf_strat rv g'

  let rec value_expr : ctx -> InferenceStrategy.t -> abs_expr -> SymState.t -> ctx *
    InferenceStrategy.t * SymState.t =
  fun ctx inf_strat e g ->
    match e with 
    | Econst _ | Eunk -> ctx, inf_strat, g
    | Erandomvar rv -> if is_const e g then ctx, inf_strat, g else value ctx inf_strat rv g
    | Etuple es | Elist es ->
      List.fold_left (fun (ctx, inf_strat, g) e -> value_expr ctx inf_strat e g) (ctx, inf_strat, g) es
    | Eadd (e1, e2) | Emul (e1, e2) | Ediv (e1, e2) 
    | Eintadd (e1, e2) | Eintmul (e1, e2) | Ecmp (_, e1, e2) ->
      let ctx, inf_strat, g = value_expr ctx inf_strat e1 g in
      value_expr ctx inf_strat e2 g
    | Eunop (_, e1) | Einttofloat e1 -> value_expr ctx inf_strat e1 g
    | Eif (e1, e2, e3) ->
      let ctx, inf_strat, g = value_expr ctx inf_strat e1 g in
      let ctx, inf_strat, g = value_expr ctx inf_strat e2 g in
      value_expr ctx inf_strat e3 g
    | Edistr _ -> 
      failwith "SymState.value_expr: not a random variable"

  let assume_patt : ctx -> InferenceStrategy.t -> pattern -> annotation -> abs_expr -> SymState.t -> 
    ctx * InferenceStrategy.t * SymState.t * abs_expr =
    fun ctx inf_strat patt a es g ->
    let rec add_patt : pattern -> abs_expr -> ctx * InferenceStrategy.t * SymState.t * RandomVar.t list -> ctx * InferenceStrategy.t * SymState.t * RandomVar.t list =
      fun patt es (ctx, inf_strat, g, xs) ->
        match patt, es with
        | Pid id, es -> 
          begin match es with
          | Etuple _ -> failwith "SymState.add_patt: Cannot sample multiple distributions"
          | _ -> 
            let ctx, x, inf_strat, g = assume ctx inf_strat id es g in
            let ctx, inf_strat, g =
              match a with
              | Mufextern.Aapprox -> value ctx inf_strat x g
              | _ -> ctx, inf_strat, g
            in
            ctx, inf_strat, g, x :: xs
          end
        | Ptuple (p :: ps), Etuple (e :: es) ->
          add_patt (Ptuple ps) (Etuple es) (add_patt p e (ctx, inf_strat, g, xs))
        | Pany, _ -> ctx, inf_strat, g, xs
        | _, _ -> failwith "SymState.add_patt: Invalid sample expression"
    in
    let ctx, inf_strat, g, xs = add_patt patt es (ctx, inf_strat, g, []) in
    let xs = List.rev xs in
    let e = 
      match xs with
      | [] -> failwith "SymState.add_patt: Invalid sample expression"
      | [x] -> Erandomvar x
      | _ -> Etuple (List.map (fun x -> Erandomvar x) xs)
    in
    ctx, inf_strat, g, e

  let rec observe : ctx -> InferenceStrategy.t -> RandomVar.t -> abs_expr -> SymState.t -> ctx * InferenceStrategy.t * SymState.t =
  fun ctx inf_strat rv v g ->
    try 
      (* Format.printf "Observe: %s\n" (RandomVar.to_string rv); *)
      let ctx, inf_strat, g = hoist ctx inf_strat rv g in
      intervene ctx inf_strat rv (Ddelta v) g
    with NonConjugate rv_parent ->
      (* Format.printf "Observe Non-conjugate: %s\n" (RandomVar.to_string rv_parent); *)
      let ctx, inf_strat, g' = value ctx inf_strat rv_parent g in
      observe ctx inf_strat rv v g'

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

let ops = [
  "add";
  "mul";
  "sub";
  "div";
  "int_add";
  "exp";
  "eq";
  "lt";
  "split";
  "int_to_float";
  "int_of_float_det";
  "float_of_int_det";
  "sub_int";
  "read";
  "mean_int";
  "mean_float";
  "mean_bool";
]

let get_func : string -> declaration list -> (pattern * expr) option =
fun name decls ->
  match List.find_opt (fun d ->
    match d with
    | Ddecl _ | Dopen _ -> false
    | Dfun (s, _, _) -> s = name
  ) decls with
  | Some (Dfun (_, p, e)) -> Some (p, e)
  | _ -> None
  
let infer : program -> InferenceStrategy.t =
fun p ->

  let ctx = Hashtbl.create 10 in
  let g = SymState.empty in
  let inf_strat = InferenceStrategy.empty in
  let decls, e = p in

  let user_functions = List.filter (fun d ->
    match d with
    | Dfun (s, _, _) -> 
      if List.mem s ops then failwith "infer: function name is reserved";
      true
    | _ -> false
  ) decls in

  (* Format.printf "user functions: %s\n\n" (String.concat ", " (List.map (fun d ->
    match d with
    | Dfun (s, _, _) -> s
    | _ -> failwith "infer: invalid function"
  ) user_functions)); *)

  let rec infer' : InferenceStrategy.t -> ctx -> SymState.t -> expr -> 
   InferenceStrategy.t * ctx * SymState.t * abs_expr =
  fun inf_strat ctx g e ->
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
      inf_strat, ctx, g, Econst c
    | Eresample -> inf_strat, ctx, g, Econst Cunit
    | Evar x -> inf_strat, ctx, g, ctx_find x ctx
    | Etuple es  | Epair es ->
      let inf_strat, ctx, g, es = 
        List.fold_left (fun (inf_strat, ctx, g, es) e ->
          let inf_strat, ctx, g, es' = infer' inf_strat ctx g e in
          inf_strat, ctx, g, es @ [es']
        ) (inf_strat, ctx, g,[]) es in
      inf_strat, ctx, g, Etuple es
    | Elist es ->
      let inf_strat, ctx, g, es = 
        List.fold_left (fun (inf_strat, ctx, g, es) e ->
          let inf_strat, ctx, g, es' = infer' inf_strat ctx g e in
          inf_strat, ctx, g, es @ [es']
        ) (inf_strat, ctx, g,[]) es in
      inf_strat, ctx, g, Elist es
    | Eapp (e1, e2) -> infer_app inf_strat ctx g e1 e2
    | Eif (e1, e2, e3) ->
      (* No side effects so represented as ite *)
      let inf_strat1, ctx, g1, e1 = infer' inf_strat ctx g e1 in
      begin match e1 with
      | Econst (Cbool true) -> 
        infer' inf_strat1 ctx g1 e2
      | Econst (Cbool false) ->
        infer' inf_strat1 ctx g1 e3
      | _ ->
        (* Widen *)
        let inf_strat2, ctx, g2, e2 = infer' inf_strat1 ctx g1 e2 in
        let inf_strat3, ctx, g3, e3 = infer' inf_strat2 ctx g2 e3 in

        if not (is_const e1 g1) then
          (* let () = 
          Format.printf "e: %s\n" (string_of_expr (Eif(e1, e2, e3)));
          Format.printf "g':\n%s\n" (SymState.to_string g3);
          Format.printf "ctx':\n%s\n" (string_of_ctx ctx) in *)
          (* State gets threaded through *)
          inf_strat3, ctx, g3, Eif(e1, e2, e3)
        else
          let ctx' = Hashtbl.copy ctx in
          let ctx', e23, g', inf_strat' = join_by_value ctx' e2 e3 g2 g3 inf_strat true in
          (* let () =
          Format.printf "e23: %s\n" (string_of_expr e23);
          Format.printf "g':\n%s\n" (SymState.to_string g');
          Format.printf "ctx':\n%s\n" (string_of_ctx ctx') in *)
          inf_strat', ctx', g', e23
      end
    | Eifeval (e1, e2, e3) ->
      let inf_strat1, ctx1, g1, e1 = infer' inf_strat ctx g e1 in
      let ctx1, inf_strat1, g1 = AbstractSSI.value_expr ctx1 inf_strat1 e1 g1 in
      (* Only one gets executed *)
      let inf_strat2, _, g2, e2 = infer' inf_strat1 ctx1 g1 e2 in
      let inf_strat3, _, g3, e3 = infer' inf_strat1 ctx1 g1 e3 in
      (* Format.printf "g2:\n%s\n" (SymState.to_string g2); *)
      (* Format.printf "g3:\n%s\n" (SymState.to_string g3); *)
      (* Format.printf "strat2:\n%s\n" (InferenceStrategy.to_string inf_strat2); *)
      (* Format.printf "strat3:\n%s\n" (InferenceStrategy.to_string inf_strat3); *)
      (* Widen *)
      let inf_strat = InferenceStrategy.join inf_strat2 inf_strat3 in
      (* Format.printf "strat:\n%s\n" (InferenceStrategy.to_string inf_strat); *)
      (* TODO: not sure if it shoudl ctx or ctx3 or ctx2 U ctx3 *)
      let ctx, e23, g, inf_strat' = join_by_value ctx e2 e3 g2 g3 inf_strat true in
      (* Format.printf "g:\n%s\n" (SymState.to_string g); *)
      (* Format.printf "e23: %s\n" (string_of_expr e23); *)
      inf_strat', ctx, g, e23
    | Elet (p, e1, e2) ->
      let inf_strat, ctx, g, e1 = infer' inf_strat ctx g e1 in
      let inf_strat, ctx, g, e2 = infer' inf_strat (ctx_add p e1 ctx) g e2 in
      let ctx = ctx_remove p ctx in
      inf_strat, ctx, g, e2
    | Esample (p, a, e1) ->
      let inf_strat, ctx, g, e1 = infer' inf_strat ctx g e1 in
      let ctx, inf_strat, g, xs = AbstractSSI.assume_patt ctx inf_strat p a e1 g in
      inf_strat, ctx, g, xs
    | Eobserve (e1, e2) ->
      let inf_strat, ctx, g, e1 = infer' inf_strat ctx g e1 in
      let inf_strat, ctx, g, e2 = infer' inf_strat ctx g e2 in
      let rv = get_obs () in
      let ctx, x, inf_strat, g = AbstractSSI.assume ctx inf_strat rv e1 g in
      let ctx, inf_strat, g = AbstractSSI.value_expr ctx inf_strat e2 g in
      (* If we don't know what e2 is, we know it must be constant *)
      let e2 =
        match e2 with
        | Eunk -> Econst Cunk
        | _ -> e2
      in
      let ctx, inf_strat, g = AbstractSSI.observe ctx inf_strat x e2 g in
      inf_strat, ctx, g, Econst Cunit
    | Evalue e1 ->
      let inf_strat, ctx, g, e1 = infer' inf_strat ctx g e1 in
      let ctx, inf_strat, g = AbstractSSI.value_expr ctx inf_strat e1 g in
      inf_strat, ctx, g, e1
    | Edistr d ->
      begin match d with
      | Dgaussian (e1, e2) ->
        let inf_strat, ctx, g, e1 = infer' inf_strat ctx g e1 in
        let inf_strat, ctx, g, e2 = infer' inf_strat ctx g e2 in
        inf_strat, ctx, g, Edistr (Dgaussian (e1, e2))
      | Dcategorical (e1, e2, e3) ->
        let inf_strat, ctx, g, e1 = infer' inf_strat ctx g e1 in
        let inf_strat, ctx, g, e2 = infer' inf_strat ctx g e2 in
        let inf_strat, ctx, g, e3 = infer' inf_strat ctx g e3 in
        (* TODO: e3 is really a function *)
        inf_strat, ctx, g, Edistr (Dcategorical (e1, e2, e3))
      | Duniformint (e1, e2) ->
        (* Uniform int is a wrapper for categorical *)
        let inf_strat, ctx, g, e1 = infer' inf_strat ctx g e1 in
        let inf_strat, ctx, g, e2 = infer' inf_strat ctx g e2 in
        let prob = match e1, e2 with
        | Econst Cint i1, Econst Cint i2 ->
          let range = i2 - i1 + 1 in
          Econst (Cfloat (1.0 /. (float_of_int range)))
        | _ -> Econst Cunk
        in
        inf_strat, ctx, g, Edistr (Dcategorical (e1, e2, prob))
      | Dbeta (e1, e2) ->
        let inf_strat, ctx, g, e1 = infer' inf_strat ctx g e1 in
        let inf_strat, ctx, g, e2 = infer' inf_strat ctx g e2 in
        inf_strat, ctx, g, Edistr (Dbeta (e1, e2))
      | Dbernoulli e1 ->
        let inf_strat, ctx, g, e1 = infer' inf_strat ctx g e1 in
        inf_strat, ctx, g, Edistr (Dbernoulli e1)
      | Dbinomial (e1, e2) ->
        let inf_strat, ctx, g, e1 = infer' inf_strat ctx g e1 in
        let inf_strat, ctx, g, e2 = infer' inf_strat ctx g e2 in
        inf_strat, ctx, g, Edistr (Dbinomial (e1, e2))
      | Dbetabinomial (e1, e2, e3) ->
        let inf_strat, ctx, g, e1 = infer' inf_strat ctx g e1 in
        let inf_strat, ctx, g, e2 = infer' inf_strat ctx g e2 in
        let inf_strat, ctx, g, e3 = infer' inf_strat ctx g e3 in
        inf_strat, ctx, g, Edistr (Dbetabinomial (e1, e2, e3))
      | Dnegativebinomial (e1, e2) ->
        let inf_strat, ctx, g, e1 = infer' inf_strat ctx g e1 in
        let inf_strat, ctx, g, e2 = infer' inf_strat ctx g e2 in
        inf_strat, ctx, g, Edistr (Dnegativebinomial (e1, e2))
      | Dexponential e1 ->
        let inf_strat, ctx, g, e1 = infer' inf_strat ctx g e1 in
        inf_strat, ctx, g, Edistr (Dgamma(Econst (Cfloat 1.), e1))
      | Dgamma (e1, e2) ->
        let inf_strat, ctx, g, e1 = infer' inf_strat ctx g e1 in
        let inf_strat, ctx, g, e2 = infer' inf_strat ctx g e2 in
        inf_strat, ctx, g, Edistr (Dgamma (e1, e2))
      | Dpoisson e1 ->
        let inf_strat, ctx, g, e1 = infer' inf_strat ctx g e1 in
        inf_strat, ctx, g, Edistr (Dpoisson e1)
      | Ddelta e1 ->
        let inf_strat, ctx, g, e1 = infer' inf_strat ctx g e1 in
        inf_strat, ctx, g, Edistr (Ddelta e1)
      end
    | Efun _ -> failwith "infer: fun is internal"
  and infer_no_func : InferenceStrategy.t -> ctx -> SymState.t -> expr -> abs_expr -> 
    InferenceStrategy.t * ctx * SymState.t * abs_expr =
  fun inf_strat ctx g e1 e2 ->
    (* Assuming e1 is an identifier *)
    match e1 with
    | Evar f ->
      begin match f with
      | {modul=None; name="add"} -> 
        begin match e2 with
        | Etuple(e1::[e2]) -> inf_strat, ctx, g, eval_add e1 e2
        | Eunk -> inf_strat, ctx, g, Eunk
        | _ -> failwith "infer: invalid add"
        end
      | {modul=None; name="mul"} -> 
        begin match e2 with
        | Etuple(e1::[e2]) -> inf_strat, ctx, g, eval_mul e1 e2
        | Eunk -> inf_strat, ctx, g, Eunk
        | _ -> failwith "infer: invalid mul"
        end
      | {modul=None; name="sub"} ->
        begin match e2 with
        | Etuple(e1::[e2]) -> inf_strat, ctx, g, eval_sub e1 e2
        | Eunk -> inf_strat, ctx, g, Eunk
        | _ -> failwith "infer: invalid sub"
        end
      | {modul=None; name="div"} ->
        begin match e2 with
        | Etuple(e1::[e2]) -> inf_strat, ctx, g, eval_div e1 e2
        | Eunk -> inf_strat, ctx, g, Eunk
        | _ -> failwith "infer: invalid div"
        end
      | {modul=None; name="int_add"} ->
        begin match e2 with
        | Etuple(e1::[e2]) -> inf_strat, ctx, g, eval_int_add e1 e2
        | Eunk -> inf_strat, ctx, g, Eunk
        | _ -> failwith "infer: invalid int_add"
        end
      | {modul=None; name="exp"} ->
        begin match e2 with
        | Etuple([e1]) -> inf_strat, ctx, g, eval_unop Exp e1
        | Eunk -> inf_strat, ctx, g, Eunk
        | _ -> failwith "infer: invalid exp"
        end
      | {modul=None; name="eq"} ->
        begin match e2 with
        | Etuple(e1::[e2]) -> inf_strat, ctx, g, eval_cmp Eq e1 e2
        | Eunk -> inf_strat, ctx, g, Eunk
        | _ -> failwith "infer: invalid eq"
        end
      | {modul=None; name="lt"} ->
        begin match e2 with
        | Etuple(e1::[e2]) -> inf_strat, ctx, g, eval_cmp Lt e1 e2
        | Eunk -> inf_strat, ctx, g, Eunk
        | _ -> failwith "infer: invalid lt"
        end
      | {modul=None; name="split"} ->
        begin match e2 with
        | Etuple e_inner -> inf_strat, ctx, g, Etuple e_inner
        | Eunk -> inf_strat, ctx, g, Etuple [Eunk; Eunk]
        | _ -> failwith "infer: invalid split"
        end
      | {modul=None; name="int_to_float"} ->
        begin match e2 with
        | Econst Cint i -> inf_strat, ctx, g, Econst (Cfloat (float_of_int i))
        | Econst Cunk -> inf_strat, ctx, g, Econst Cunk
        | Etuple _ -> failwith "infer: invalid int_to_float"
        | Eunk -> inf_strat, ctx, g, Eunk
        | _ -> failwith "infer: invalid int_to_float"
        end
      | {modul=None; name="int_of_float_det"} ->
        begin match e2 with
        | Econst Cfloat f -> inf_strat, ctx, g, Econst (Cint (int_of_float f))
        | Econst Cunk -> inf_strat, ctx, g, Econst Cunk
        | Etuple _ -> failwith "infer: invalid int_of_float_det"
        | Eunk -> inf_strat, ctx, g, Eunk
        | _ -> failwith "infer: invalid int_of_float_det"
        end
      | {modul=None; name="float_of_int_det"} ->
        begin match e2 with
        | Econst Cint i -> inf_strat, ctx, g, Econst (Cfloat (float_of_int i))
        | Econst Cunk -> inf_strat, ctx, g, Econst Cunk
        | Etuple _ -> failwith "infer: invalid float_of_int_det"
        | Eunk -> inf_strat, ctx, g, Econst Cunk
        | _ -> failwith "infer: invalid float_of_int_det"
        end
      | {modul=None; name="sub_int"} ->
        begin match e2 with
        | Etuple(e1::[e2]) -> inf_strat, ctx, g, eval_sub_int e1 e2
        | Eunk -> inf_strat, ctx, g, Eunk
        | _ -> failwith "infer: invalid sub_int"
        end
      | {modul=None; name="read"} -> inf_strat, ctx, g, Eunk
      | {modul=None; name="mean_int"} 
      | {modul=None; name="mean_float"}
      | {modul=None; name="mean_bool"} -> inf_strat, ctx, g, Econst Cunk
      | {modul=None; name} ->
        (* User defined functions just get inlined *)
        let func = get_func name user_functions in
        begin match func with
        | Some (p, e_body) ->
          (* create mapping of parameters to arguments *)
          let inf_strat, ctx, g, e_body = infer' inf_strat (ctx_add p e2 ctx) g e_body in
          let ctx = ctx_remove p ctx in
          inf_strat, ctx, g, e_body
        | None -> failwith "infer_ops: invalid function call"
        end
      | {modul=Some "List"; name="length"} -> 
        begin match e2 with
        | Elist es -> inf_strat, ctx, g, Econst (Cint (List.length es))
        | Eunk -> inf_strat, ctx, g, Econst Cunk
        | _ -> failwith "infer_ops: invalid List.length"
        end
      | {modul=Some "List"; name="hd"} -> 
        begin match e2 with
        | Elist (e::_) -> inf_strat, ctx, g, e
        | Eunk -> inf_strat, ctx, g, Eunk
        | _ -> failwith "infer_ops: invalid List.hd"
        end
      | {modul=Some "List"; name="tl"} ->
        begin match e2 with
        | Elist (_::es) -> inf_strat, ctx, g, Elist es
        | Eunk -> inf_strat, ctx, g, Eunk
        | _ -> failwith "infer_ops: invalid List.tl"
        end
      | {modul=Some "List"; name="cons"} ->
        begin match e2 with
        | Etuple (e1::[Elist es]) -> inf_strat, ctx, g, Elist (e1::es)
        | Etuple (e1::[Eunk]) -> inf_strat, ctx, g, Elist [e1; Eunk]
        | Eunk -> inf_strat, ctx, g, Eunk
        | _ -> failwith "infer_ops: invalid List.cons"
        end
      | {modul=Some "List"; name="rev"} ->
        begin match e2 with
        | Elist es -> inf_strat, ctx, g, Elist (List.rev es)
        | Eunk -> inf_strat, ctx, g, Eunk
        | _ -> failwith "infer_ops: invalid List.rev"
        end
      (* TODO: list functions passing around list functions *)
      | {modul=Some "List";_} | {modul=Some "Array"; _} ->
        failwith "infer_ops: not implemented"
      | _ -> failwith "infer_ops: invalid function call"
      end
    | _ -> failwith "infer_ops: invalid function call"
  and infer_app : InferenceStrategy.t -> ctx -> SymState.t -> expr -> expr -> 
    InferenceStrategy.t * ctx * SymState.t * abs_expr =
  fun inf_strat ctx g e1 e2 ->
    (* Can't pass a function around except to List/Array functions
       so we can't run infer' on function arguments *)
    match e1 with 
    | Evar f ->
      begin match f with
      | {modul=Some "List"; name="init"} ->
        begin match e2 with
        | Etuple (n::[f]) ->
          let inf_strat, ctx, g, n = infer' inf_strat ctx g n in
          begin match n with
          | Econst Cint n ->
            let init = List.init n (fun i -> (Econst (Cint i) : expr)) in
            infer_app inf_strat ctx g 
              (Evar {modul=Some "List";name="map"})
              (Etuple [f; Elist init])
          | Eunk -> 
            let inf_strat, ctx, g, _ = infer_no_func inf_strat ctx g f Eunk in
            inf_strat, ctx, g, Eunk
          | _ -> failwith "infer_app: invalid List.init"
          end
        | _ -> failwith "infer_app: invalid List.init"
        end
      | {modul=Some "List"; name="iter"} ->
        begin match e2 with
        | Etuple (f::[l]) ->
          let inf_strat, ctx, g, _ = infer_app inf_strat ctx g 
              (Evar {modul=Some "List";name="map"})
              (Etuple [f; l]) in
              inf_strat, ctx, g, Econst Cunit
        | _ -> failwith "infer_app: invalid List.iter"
        end
      | {modul=Some "List"; name="map"} ->
        begin match e2 with
        | Etuple (f::[l]) ->
          let inf_strat, ctx, g, l = infer' inf_strat ctx g l in
          begin match l with
          | Elist args ->
            let inf_strat, ctx, g, e_inner = List.fold_left (fun (inf_strat, ctx, g, es) arg ->
              let inf_strat, ctx, g, e_res = infer_no_func inf_strat ctx g f arg in
              inf_strat, ctx, g, e_res::es
            ) (inf_strat, ctx, g, []) args in
            inf_strat, ctx, g, Elist (List.rev e_inner)
          | Eunk -> 
            let inf_strat, ctx, g, e_inner = infer_no_func inf_strat ctx g f Eunk in
            inf_strat, ctx, g, e_inner
          | _ -> failwith "infer_app: invalid List.map"
          end
        | _ -> failwith "infer_app: invalid List.map"
        end
      | {modul=Some "List"; name="fold"} 
      | {modul=Some "List"; name="fold_resample"} ->
        begin match e2 with
        | Etuple (f::[l; acc]) ->
          let inf_strat, ctx, g, l = infer' inf_strat ctx g l in
          let inf_strat, ctx_before, g_before, acc = infer' inf_strat ctx g acc in

          begin match l with
          | Elist args ->
            let rec iter inf_strat ctx_pre g_pre acc args =
              match args with
              | [] ->  inf_strat, ctx_pre, g_pre, acc
              | [arg] -> 
                let inf_strat, ctx, g, res = infer_no_func inf_strat ctx_pre g_pre f (Etuple [acc; arg]) in
                (* Format.printf "Step:\n%s" (SymState.to_string g); *)
                (* Format.printf "Ret: %s\n" (string_of_expr res); *)
                inf_strat, ctx, g, res
              | arg::args ->
                (* Format.printf "Prev: %s\n" (string_of_expr acc); *)
                (* Format.printf "Pre:\n%s\n" (SymState.to_string g_pre); *)

                let inf_strat, ctx, g, res = infer_no_func inf_strat ctx_pre g_pre f (Etuple [acc; arg]) in

                (* Format.printf "Step:\n%s" (SymState.to_string g); *)
                (* Format.printf "Res: %s\n\n" (string_of_expr res); *)

                (* let ctx_post, res_post, g_post, inf_strat_post = join_by_value ctx acc res g_pre g inf_strat true in *)

                (* Format.printf "Post:\n%s\n" (SymState.to_string g_post); *)
                (* Format.printf "Ret: %s\n" (string_of_expr res_post); *)
                (* Format.printf "-----------------\n"; *)
                
                iter inf_strat ctx g res args
            in
            let inf_strat, ctx, g, res = iter inf_strat ctx_before g_before acc args in
            inf_strat, ctx, g, res
          | Eunk -> 
            (* Compute fixpoint *)
            let rec iter inf_strat ctx_pre g_pre acc =
              (* Format.printf "Prev: %s\n" (string_of_expr acc); *)
              (* Format.printf "Pre:\n%s\n" (SymState.to_string g_pre); *)
              (* Format.printf "Strat:\n%s\n" (InferenceStrategy.to_string inf_strat); *)
              (* Format.printf "Ctx:\n%s\n" (string_of_ctx ctx_pre); *)
              
              let inf_strat, ctx, g, res = infer_no_func inf_strat ctx_pre g_pre f (Etuple [acc; Eunk]) in

              (* Format.printf "Step:\n%s" (SymState.to_string g); *)
              (* Format.printf "Res: %s\n\n" (string_of_expr res); *)
              (* Format.printf "Strat:\n%s\n" (InferenceStrategy.to_string inf_strat); *)
              (* Format.printf "Ctx:\n%s\n" (string_of_ctx ctx); *)
              
              let ctx_post = Hashtbl.copy ctx in
              let ctx_post, res_post, g_post, inf_strat_post = join_by_value ctx_post acc res g_pre g inf_strat true in
              
              (* let () = if !i >= 1 then exit(2) else incr i in *)

              (* Format.printf "Post:\n%s\n" (SymState.to_string g_post); *)
              (* Format.printf "Ret: %s\n" (string_of_expr res_post); *)
              (* Format.printf "Strat:\n%s\n" (InferenceStrategy.to_string inf_strat_post); *)
              (* Format.printf "Ctx:\n%s\n" (string_of_ctx ctx_post); *)
              (* Format.printf "-----------------\n";               *)

              (* if equal then return g else return g_post *)
              if SymState.equal g_pre g_post && acc = res_post then 
                inf_strat_post, ctx, g, res
              else 
                iter inf_strat_post ctx_post g_post res_post
            in
            let inf_strat, ctx, g, res = iter inf_strat ctx_before g_before acc in
            inf_strat, ctx, g, res
          | _ -> failwith "infer_app: invalid List.fold"
          end
        | _ -> failwith "infer_app: invalid List.fold"
        end
      | _ -> 
        let inf_strat, ctx, g, e2 = infer' inf_strat ctx g e2 in
        infer_no_func inf_strat ctx g e1 e2
      end
    | _ -> failwith "infer_app: invalid function"
  in

  let inf_strat, _ctx, _g', _res = infer' inf_strat ctx g e in

  (* For debug *)
  (* let sym_state_s = SymState.to_string g' in *)
  (* Format.printf "g:\n%s\n" sym_state_s; *)

  (* Format.printf "ctx:%s\n" (string_of_ctx ctx); *)

  (* Format.printf "Res: %s\n" (string_of_expr res); *)

  inf_strat