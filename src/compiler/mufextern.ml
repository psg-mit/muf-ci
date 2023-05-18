type constant =
  | Cbool of bool
  | Cint of int
  | Cfloat of string
  | Cstring of string
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
  | Elet of pattern * expr * expr
  | Esample of pattern * annotation * expr
  | Eobserve of expr * expr
  | Evalue of expr
  | Elist of expr list
  | Epair of expr list
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

let varnum = ref 0
let get_varnum _ =
  varnum := !varnum + 1;
  !varnum

let used_rv_names = ref []

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
  in
  Exp.apply (Exp.ident (with_loc (Longident.Lident "const"))) [Nolabel, const]

let rec compile_pattern (p: pattern) : Parsetree.pattern =
  match p with
  | Pid id -> Pat.var (with_loc id.name)
  | Ptuple ps -> Pat.tuple (List.map compile_pattern ps)
  (* | Ptype (p, t) -> mk_pattern (M.Ptype (convert_pattern p, t)) *)
  | Pany -> Pat.any ()

let rec get_rv_name (p: pattern) : Parsetree.expression =
  match p with
  | Pid { name = x; _ } -> 
    let x = if List.mem x !used_rv_names then x ^ (string_of_int (get_varnum ())) else x in
    used_rv_names := x :: !used_rv_names;
    Exp.constant (Const.string x)
  | Ptuple ps -> Exp.tuple (List.map get_rv_name ps)
  (* | Ptype (p, t) -> mk_expression (M.Etype (convert_pattern p, t)) *)
  | Pany -> Exp.constant (Const.string "")
  
let rec compile_sample (p: pattern) (a: annotation) (e: expr) : Parsetree.expression =
  match p, e with
  (* Single vars *)
  | Pid _, Evar _ | Pid _, Eapp _ | Pany, _ ->
    let inner = Exp.apply (Exp.ident (with_loc (Longident.Lident "sample"))) [Nolabel, get_rv_name p; Nolabel, compile_expr e] in
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

and compile_expr (e: expr) : Parsetree.expression =
  match e with
  | Econst c -> compile_const c
  | Evar id -> Exp.ident (lid id)
  | Etuple es -> Exp.tuple (List.map compile_expr es)
  | Eapp (e1, e2) -> Exp.apply (compile_expr e1) [Nolabel, compile_expr e2]
  | Eif (e1, e2, e3) -> 
    (* TODO: analyze when to use semi-symbolic ite *)
    Exp.apply (Exp.ident (with_loc (Longident.Lident "ite"))) [Nolabel, compile_expr e1; Nolabel, compile_expr e2; Nolabel, compile_expr e3]
    (* Exp.ifthenelse (compile_expr e1) (compile_expr e2) (Some (compile_expr e3)) *)
  | Elet (p, e1, e2) -> 
    Exp.let_ Nonrecursive
      [ { Parsetree.pvb_pat = compile_pattern p;
      pvb_expr = compile_expr e1;
      pvb_attributes = [];
      pvb_loc = Location.none; } ]
      (compile_expr e2)
  | Esample (p, a, e) -> compile_sample p a e
  | Eobserve (e1, e2) -> 
    Exp.apply (Exp.ident (with_loc (Longident.Lident "observe"))) 
      [Nolabel, compile_expr e1; Nolabel, compile_expr e2]
  | Elist es ->
    let rec aux es =
      match es with
      | [] -> Exp.construct (with_loc (Longident.Lident "[]")) None
      | e::es -> Exp.construct (with_loc (Longident.Lident "::")) (Some (Exp.tuple [compile_expr e; aux es]))
    in
    Exp.apply (Exp.ident (with_loc (Longident.Lident "lst"))) 
      [Nolabel, aux es]
  | Epair es ->
    (* (e1, e2, e3) = (e1, (e2, e3)) *)
    let rec aux es =
      match es with
      | [] | _::[] -> failwith "Invalid pair expression"
      | [e1; e2] -> Exp.apply (Exp.ident (with_loc (Longident.Lident "pair"))) [Nolabel, compile_expr e1; Nolabel, compile_expr e2]
      | e1::es -> Exp.apply (Exp.ident (with_loc (Longident.Lident "pair"))) [Nolabel, compile_expr e1; Nolabel, aux (es)]
    in
    aux es
  | Evalue e ->
    Exp.apply (Exp.ident (with_loc (Longident.Lident "value"))) [Nolabel, compile_expr e]

let compile_declarations (ds: declaration list) : Parsetree.structure_item list =
  List.map (fun d ->
    match d with
    | Ddecl (p, e) -> 
      Str.value Nonrecursive [Vb.mk (compile_pattern p) (compile_expr e)]
    | Dfun (s, p, e) -> 
      Str.value Nonrecursive
        [ Vb.mk (Pat.var (with_loc s))
            (Exp.fun_ Nolabel None (compile_pattern p) (compile_expr e)) ]
    (* | Dtype (s, ss, td) -> M.Dtype (s, ss, td) *)
    | Dopen s ->
      Str.open_ (Opn.mk (Mod.ident (with_loc (Longident.Lident s))))
  ) ds

let compile_program (p: program) : Parsetree.structure =
  let decls, e = p in
  let decls = decls@[Dfun("main", Pany, e)] in
  let compiled_program = compile_declarations decls in
  compiled_program
