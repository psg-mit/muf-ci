type constant =
  | Cbool of bool
  | Cint of int
  | Cfloat of string
  | Cstring of string
  | Cunit
[@@deriving show]

type identifier =
  { modul: string option; name: string }
[@@deriving show, map, fold]

(* type type_expression =
  | Tany
  | Tvar of string
  | Ttuple of type_expression list
  | Tconstr of string * type_expression list
[@@deriving show, map, fold] *)

type pattern =
  | Pid of identifier
  | Ptuple of pattern list
  | Punit
  (* | Ptype of 'p * type_expression *)
  | Pany
[@@deriving show, map, fold]

type annotation =
  | Aexact
  | Aapprox
  | Adynamic
[@@deriving show, map, fold]

type expr =
  | Econst of constant
  | Evar of identifier
  | Etuple of expr list
  | Eapp of expr * expr
  | Eif of expr * expr * expr
  | Eifeval of expr * expr * expr
  | Elet of pattern * expr * expr
  | Esample of pattern * annotation * expr
  | Eobserve of expr * expr
  | Evalue of expr
  | Elist of expr list
  | Epair of expr list
  | Efun of pattern * expr
[@@deriving show, map, fold]

(* type distribution =
  | Beta of expr * expr
  | Bernoulli of expr
  | Binomial of expr * expr
  | Gaussian of expr * expr
  | Gamma of expr * expr
  | Poisson of expr
[@@deriving show, map, fold] *)

(* type type_declaration =
  | TKabstract_type
  | TKabbrev of type_expression
  | TKvariant_type of (string * type_expression list option) list
  | TKrecord of (string * type_expression) list
[@@deriving show, map, fold] *)

type declaration =
  | Ddecl of pattern * expr
  | Dfun of string * pattern * expr
  (* | Dtype of (string * string list * type_declaration) list *)
  | Dopen of string
[@@deriving show, map, fold]

type program = declaration list * expr
[@@deriving show]

(* Compile program *)
open Ast_helper

let rv_n = ref 0
let get_rv x =
  rv_n := !rv_n + 1;
  x ^ (string_of_int (!rv_n))

let used_rv_names = ref []

let v_n = ref 0
let get_v () =
  v_n := !v_n + 1;
  "v" ^ (string_of_int (!v_n))

let temp_var () = 
  {modul=None; name=get_v ()}

let rec cps_pass (e: expr) (k: expr) : expr =
  match e with
  | Evar _ | Econst _ -> Eapp(k, e)
  | Esample _ | Eobserve _ -> Eapp(e, k)
  | Efun _ -> failwith "Fun is internal"
  | Eif (e1, e2, e3) ->
    (* Does not contain observe/sample in e2 or e3 *)
    (* Symbolic ite *)
    let guard_temp = temp_var () in
    let then_temp = temp_var () in
    let else_temp = temp_var () in

    let k = Eapp(k, Eif(Evar guard_temp, Evar then_temp, Evar else_temp)) in

    cps_pass e1 (Efun (Pid guard_temp,
      cps_pass e2 (Efun (Pid then_temp,
        cps_pass e3 (Efun (Pid else_temp, k))))))
  | Eifeval (e1, e2, e3) ->
    (* Eagerly evals e1 *)
    (* Used for when there's side effects expected *)
    let guard_temp = temp_var () in
    let k_temp = temp_var () in

    let k' = 
      Efun (Pid guard_temp, 
        Elet (Pid k_temp, k,
        Eifeval(Evar guard_temp, 
          cps_pass e2 (Evar k_temp), 
          cps_pass e3 (Evar k_temp)))) in
    cps_pass e1 k'
  | Eapp (e1, e2) ->
    (* e1 should be atomic *)
    begin match e1 with
    (* All list and array functions should be CPS *)
    | Evar {modul=Some "List"; _} | Evar {modul=Some "Array"; _} -> 
      Eapp(e, k)
    | _ -> 
      let temp = temp_var () in
      let k' = Efun(Pid temp, Eapp(k, Evar temp)) in
      cps_pass e2 k'
    end
  | Elet (x, e1, e2) ->
    let temp = temp_var () in
    let k' = Efun(Pid temp, Elet(x, Evar temp, cps_pass e2 k)) in
    cps_pass e1 k'
  | Evalue e1 ->
    let temp = temp_var () in
    let k' = Efun(Pid temp, Eapp(k, Evalue (Evar temp))) in
    cps_pass e1 k'
  | Elist l ->
    let temps = List.map (fun _ -> temp_var ()) l in
    let k' = Eapp(k, Elist (List.map (fun e -> Evar e) temps)) in
    List.fold_left2 (fun k temp e ->
      let k' = Efun(Pid temp, k) in
      cps_pass e k'
    ) k' (List.rev temps) (List.rev l)
  | Epair l ->
    let temps = List.map (fun _ -> temp_var ()) l in
    let k' = Eapp(k, Epair (List.map (fun e -> Evar e) temps)) in
    List.fold_left2 (fun k temp e ->
      let k' = Efun(Pid temp, k) in
      cps_pass e k'
    ) k' (List.rev temps) (List.rev l)
  | Etuple l ->
    let temps = List.map (fun _ -> temp_var ()) l in
    let k' = Eapp(k, Etuple (List.map (fun e -> Evar e) temps)) in
    List.fold_left2 (fun k temp e ->
      let k' = Efun(Pid temp, k) in
      cps_pass e k'
    ) k' (List.rev temps) (List.rev l)

let with_loc x : 'a with_loc = 
  { txt = x;
    loc = Location.none; }

let lident (x: identifier) : Longident.t =
  match x.modul with
  | None -> Longident.Lident x.name
  | Some m -> Longident.Ldot (Longident.Lident m, x.name)

let lid (x: identifier) : Ast_helper.lid =
  with_loc (lident x)

let compile_const (c: constant) : Parsetree.expression =
  let const = 
    match c with
    | Cbool b ->
      let b =
        with_loc (Longident.Lident (string_of_bool b))
      in
      Exp.construct b None 
    | Cint i -> Exp.constant (Const.int i)
    | Cfloat f -> Exp.constant (Const.float f)
    | Cstring s -> Exp.constant (Const.string s)
    | Cunit -> Exp.construct (with_loc (Longident.Lident "()")) None
  in
  Exp.apply (Exp.ident (with_loc (Longident.Lident "const"))) [Nolabel, const]

let rec compile_pattern (p: pattern) : Parsetree.pattern =
  match p with
  | Pid id -> Pat.var (with_loc id.name)
  | Ptuple ps -> Pat.tuple (List.map compile_pattern ps)
  (* | Ptype (p, t) -> mk_pattern (M.Ptype (convert_pattern p, t)) *)
  | Pany -> Pat.any ()
  (* This is a hack since unit can't be used, just give it a name
     and say it's unit type *)
  | Punit -> 
    Pat.constraint_ 
      (Pat.var (with_loc ("_unit_" ^ get_v ()))) 
      (Typ.constr (with_loc (Longident.Lident "unit expr")) [])

let rec get_rv_name (p: pattern) : Parsetree.expression =
  match p with
  | Pid { name = x; _ } -> 
    let x = if List.mem x !used_rv_names then get_rv x else x in
    used_rv_names := x :: !used_rv_names;
    Exp.constant (Const.string x)
  | Ptuple ps -> Exp.tuple (List.map get_rv_name ps)
  (* | Ptype (p, t) -> mk_expression (M.Etype (convert_pattern p, t)) *)
  | Pany -> Exp.constant (Const.string "")
  | Punit -> failwith "RV cannot be unit"

let rec annotation_pass (e: expr) : expr =
  match e with
  | Esample (p, a, e) ->
    let e = annotation_pass e in
    begin match a with
    | Aapprox -> Evalue (Esample (p, Adynamic, e))
    | _ -> Esample (p, a, e)
    end
  | Eobserve (p, e) -> Eobserve (p, annotation_pass e)
  | Efun (p, e) -> Efun (p, annotation_pass e)
  | Eif (e1, e2, e3) -> 
    Eif (annotation_pass e1, annotation_pass e2, annotation_pass e3)
  | Eifeval (e1, e2, e3) -> 
    Eifeval (annotation_pass e1, annotation_pass e2, annotation_pass e3)
  | Eapp (e1, e2) -> Eapp (annotation_pass e1, annotation_pass e2)
  | Elet (x, e1, e2) -> Elet (x, annotation_pass e1, annotation_pass e2)
  | Evalue e1 -> Evalue (annotation_pass e1)
  | Elist l -> Elist (List.map annotation_pass l)
  | Epair l -> Epair (List.map annotation_pass l)
  | Etuple l -> Etuple (List.map annotation_pass l)
  | Evar _ | Econst _ -> e

(* TODO: analyze when to use semi-symbolic ite *)
(* If body has resample/observe, turn into regular if then else *)
(* let rec ite_pass (e: expr) : expr = *)
  
let rec compile_sample (p: pattern) (a: annotation) (e: expr) : Parsetree.expression =
  match p, e with
  (* Single vars *)
  | Pid _, Evar _ | Pid _, Eapp _ | Pany, _ ->
    let inner = Exp.apply (Exp.ident (with_loc (Longident.Lident "sample"))) 
                  [Nolabel, get_rv_name p; Nolabel, compile_expr' e] in
    begin match a with 
    | Aapprox -> 
      let value = Exp.ident (with_loc (Longident.Lident "value")) in
      Exp.apply value [Nolabel, inner]
    | _ -> inner
    end
  (* Tuple vars *)
  | Ptuple [p'], Etuple [e'] -> compile_sample p' a e'
  | Ptuple (p'::ps), Etuple (e'::es) ->
    let first = compile_sample p' a e' in
    let rest = compile_sample (Ptuple ps) a (Etuple es) in
    Exp.tuple [first; rest]
  | _ -> failwith "Invalid sample expression"

and compile_expr' (e: expr) : Parsetree.expression =
  match e with
  | Econst c -> compile_const c
  | Evar id -> Exp.ident (lid id)
  | Etuple es -> 
    (* Etuple is never the final except for unit *)
    Exp.tuple (List.map compile_expr' es)
  | Eapp (e1, e2) -> Exp.apply (compile_expr' e1) [Nolabel, compile_expr' e2]
  | Eif (e1, e2, e3) -> 
    Exp.apply (Exp.ident (with_loc (Longident.Lident "ite"))) 
      [Nolabel, compile_expr' e1; 
       Nolabel, compile_expr' e2; 
       Nolabel, compile_expr' e3]
  | Eifeval (e1, e2, e3) ->
    (* regular if then else, only instantiated by compiler *)
    let value = Exp.ident (with_loc (Longident.Lident "value")) in
    let get_const = Exp.ident (with_loc (Longident.Lident "get_const")) in
    let e1' = Exp.apply get_const [Nolabel, 
                (Exp.apply value [Nolabel, compile_expr' e1])] in
    Exp.ifthenelse e1' (compile_expr' e2) (Some (compile_expr' e3))
  | Elet (p, e1, e2) -> 
    Exp.let_ Nonrecursive
      [ { Parsetree.pvb_pat = compile_pattern p;
        pvb_expr = compile_expr' e1;
        pvb_attributes = [];
        pvb_loc = Location.none; } ]
      (compile_expr' e2)
  | Esample (p, a, e) -> compile_sample p a e
  | Eobserve (e1, e2) -> 
    Exp.apply (Exp.ident (with_loc (Longident.Lident "observe"))) 
      [Nolabel, compile_expr' e1; Nolabel, compile_expr' e2]
  | Elist es ->
    let rec aux es =
      match es with
      | [] -> Exp.construct (with_loc (Longident.Lident "[]")) None
      | e::es -> Exp.construct (with_loc (Longident.Lident "::")) (Some (Exp.tuple [compile_expr' e; aux es]))
    in
    Exp.apply (Exp.ident (with_loc (Longident.Lident "lst"))) 
      [Nolabel, aux es]
  | Epair es ->
    (* (e1, e2, e3) = (e1, (e2, e3)) *)
    let rec aux es =
      match es with
      | [] | _::[] -> failwith "Invalid pair expression"
      | [e1; e2] -> Exp.apply (Exp.ident (with_loc (Longident.Lident "pair"))) [Nolabel, compile_expr' e1; Nolabel, compile_expr' e2]
      | e1::es -> Exp.apply (Exp.ident (with_loc (Longident.Lident "pair"))) [Nolabel, compile_expr' e1; Nolabel, aux (es)]
    in
    aux es
  | Evalue e ->
    Exp.apply (Exp.ident (with_loc (Longident.Lident "value"))) [Nolabel, compile_expr' e]
  | Efun (p, e) ->
    Exp.fun_ Nolabel None (compile_pattern p) (compile_expr' e)

let compile_expr (cps: bool) (e: expr) : Parsetree.expression =
  let e = annotation_pass e in
  (* let e = ite_pass e in *)
  let e = if cps then 
    let e = cps_pass e (Evar {modul=None; name = "muf_k"}) in
    Exp.fun_ Nolabel None (Pat.var (with_loc "muf_k")) (compile_expr' e)
  else 
    compile_expr' e
  in
  e

let compile_declarations (output: string) (ds: declaration list) : Parsetree.structure_item list =
  List.map (fun d ->
    match d with
    | Ddecl (p, e) -> 
      Str.value Nonrecursive [ Vb.mk (compile_pattern p) (compile_expr false e) ]
    | Dfun (s, p, e) ->
      let e = if s = output then 
        compile_expr false e 
      else
        compile_expr true e
      in
        Str.value Nonrecursive
          [ Vb.mk (Pat.var (with_loc s))
              (Exp.fun_ Nolabel None (compile_pattern p) e) ]
    (* | Dtype (s, ss, td) -> M.Dtype (s, ss, td) *)
    | Dopen s ->
      Str.open_ (Opn.mk (Mod.ident (with_loc (Longident.Lident s))))
  ) ds

let compile_program (output: string) (p: program) : Parsetree.structure =
  let decls, e = p in

  let main = 
    Str.value Nonrecursive 
      [ Vb.mk (Pat.var (with_loc "main"))
          (Exp.fun_ Nolabel None (Pat.any ()) 
              (compile_expr true e)) ] in
  let compiled_program = (compile_declarations output decls)@[main] in
  compiled_program