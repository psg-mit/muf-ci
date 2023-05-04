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
[@@deriving show, map, fold]

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

type 'm program = declaration list * expr
  (* 'm expr *)
  (* 'm declaration list * ('m expr) *)
[@@deriving show]

(* Converting to internal muf grammar *)

module M = Muf

let varnum = ref 0
let get_varnum _ =
  varnum := !varnum + 1;
  !varnum

let used_rv_names = ref []

let convert_identifier (id: identifier) : M.identifier =
  match id with
  | { modul; name } -> { modul; name }

let convert_const (c: constant) : M.constant =
  match c with
  | Cbool b -> M.Cbool b
  | Cint i -> M.Cint i
  | Cfloat f -> M.Cfloat f
  | Cstring s -> M.Cstring s

let mk_pattern (p: 'm M.pattern M.patt_desc) : 'm M.pattern =
  { patt = p; pmeta = () }

let rec convert_pattern (p: pattern) : 'm M.pattern =
  match p with
  | Pid id -> mk_pattern (M.Pid (convert_identifier id))
  | Ptuple ps -> mk_pattern (M.Ptuple (List.map convert_pattern ps))
  (* | Ptype (p, t) -> mk_pattern (M.Ptype (convert_pattern p, t)) *)
  | Pany -> mk_pattern (M.Pany)

let mk_expression (e: ('m M.pattern, 'm M.expression) M.expr_desc) : 'm M.expression =
  { expr = e; emeta = () }

let rec get_rv_name (p: pattern) : 'm M.expression =
  match p with
  | Pid { name = x; _ } -> 
    let x = if List.mem x !used_rv_names then x ^ (string_of_int (get_varnum ())) else x in
    used_rv_names := x :: !used_rv_names;
    mk_expression (M.Econst (M.Cstring x))
  | Ptuple ps -> mk_expression (M.Etuple (List.map get_rv_name ps))
  (* | Ptype (p, t) -> mk_expression (M.Etype (convert_pattern p, t)) *)
  | Pany -> failwith "Random variables must be named"

let rec convert_sample (p: pattern) (a: annotation) (e: expr) : 'm M.expression =
  match p, e with
  (* Single vars *)
  | Pid _, Evar _ | Pid _, Eapp _ ->
    let inner = mk_expression (M.Esample("prob", 
      mk_expression (M.Etuple [get_rv_name p; convert_expression e]))) in
    begin match a with 
    | Aapprox -> 
      let const = mk_expression (M.Evar { modul=None; name="const" }) in
      let eval = mk_expression (M.Evar { modul=None; name="eval" }) in
      mk_expression (M.Eapp(const, mk_expression (M.Eapp (eval, inner))))
    | _ -> inner
    end
  (* Tuple vars *)
  | Ptuple [p'], Etuple [e'] -> convert_sample p' a e'
  | Ptuple (p'::ps), Etuple (e'::es) ->
    let first = convert_sample p' a e' in
    let rest = convert_sample (Ptuple ps) a (Etuple es) in
    mk_expression (M.Etuple [first; rest])
  | Pany, _-> failwith "Random variables must be named"
  | _ -> failwith "Invalid sample expression"

and convert_expression (e: expr) : 'm M.expression =
  match e with
  | Econst c -> mk_expression (M.Econst (convert_const c))
  | Evar id -> mk_expression (M.Evar (convert_identifier id))
  | Etuple es -> mk_expression (M.Etuple (List.map convert_expression es))
  | Eapp (e1, e2) -> mk_expression (M.Eapp (convert_expression e1, convert_expression e2))
  | Eif (e1, e2, e3) -> mk_expression (M.Eapp (mk_expression (M.Evar { modul=None;name="ite" }),
      mk_expression (M.Etuple [convert_expression e1; convert_expression e2; convert_expression e3])))
  | Elet (p, e1, e2) -> mk_expression (M.Elet (convert_pattern p, convert_expression e1, convert_expression e2))
  | Esample (p, a, e) -> convert_sample p a e
  | Eobserve (e1, e2) -> mk_expression (M.Eobserve ("prob", convert_expression e1, convert_expression e2))

let convert_declarations (ds: declaration list) : 'm M.declaration list =
  let mk_declaration (d: ('m M.pattern, 'm M.expression) M.decl_desc) : 'm M.declaration =
    { decl = d }
  in
  List.map (fun d ->
    match d with
    | Ddecl (p, e) -> mk_declaration (M.Ddecl (convert_pattern p, convert_expression e))
    | Dfun (s, p, e) -> mk_declaration (M.Dfun (s, convert_pattern p, convert_expression e))
    (* | Dtype (s, ss, td) -> M.Dtype (s, ss, td) *)
    | Dopen s ->mk_declaration (M.Dopen s)
  ) ds

let patch_return_expression (state: 'm M.expression) (e: 'm M.expression) : 'm M.expression =
  let rec patch_return_expression' (e: 'm M.expression) : 'm M.expression =
    match e.expr with
    | M.Elet (x, e1, e2) -> mk_expression (M.Elet(x, e1, patch_return_expression' e2))
    | _ -> mk_expression (Etuple [e; state])
  in
  patch_return_expression' e

(* let convert_fold (decls: 'm M.declaration list) (expr: 'm M.expression) : 'm M.declaration * 'm M.expression =
  let ctx = Hashtbl.create 10 in
  List.iter (fun (d) ->
    match d.decl with
    | M.Dfun (f, _, _) -> Hashtbl.add ctx f d
    | _ -> ()
  ) decls;

  let rec convert_nonfolds (state: 'm M.expression) (e: 'm M.expression) : 'm M.expression * 'm M.expression =
    match e.expr with
    | M.Elet (x, e1, e2) ->
      let (e1', state') = convert_nonfolds state e1 in
      let (e2', state'') = convert_nonfolds state' e2 in
      (mk_expression (M.Elet (x, e1', e2')), state'')
    | M.Eapp (e1, e2) ->
      let (e1', state') = convert_nonfolds state e1 in
      let (e2', state'') = convert_nonfolds state' e2 in
      (mk_expression (M.Eapp (e1', e2')), state'')
    | M.Eobserve (s, e1, e2) ->
      let (e1', state') = convert_nonfolds state e1 in
      let (e2', state'') = convert_nonfolds state' e2 in
      (mk_expression (M.Eobserve (s, e1', e2')), state'')
    | M.Esample (s, e1) ->
      let (e1', state') = convert_nonfolds state e1 in
      (mk_expression (M.Esample (s, e1')), state')
    | M.Econst _ | M.Evar _ | M.Etuple _ -> (e, state)
    | _ -> failwith "Invalid expression" *)


let convert_to_intern (particles: int) (verbose: bool) (p: 'm program) : 'm M.program =
  let decls = convert_declarations (fst p) in
  let expr = convert_expression (snd p) in
  let expr = patch_return_expression (mk_expression (M.Etuple [])) expr in

  (* TODO: folds become updates on body stream's global state *)

  (* TODO:  
    - more than one folds
    - uneven length folds
    - no nested folds? or inner fold is importance sampling
  *)

  let body_n : ('m M.pattern, 'm M.expression) M.node =
    { n_type = ([], M.TKrecord []); (* XXX TODO XXX *)
      n_init = mk_expression (M.Etuple []);
      n_step = (mk_pattern (M.Ptuple [mk_pattern (M.Ptuple []); mk_pattern (M.Ptuple [])]), expr); }
  in
  let func : 'm M.declaration = { decl = M.Dnode ("body", [], body_n) } in

  (* Assumes return result is a list of float distributions *)
  let print_output = mk_expression (M.Eapp (mk_expression (M.Evar {modul=Some "List";name="print_float_list"}), 
                                            mk_expression (M.Eapp(mk_expression (M.Evar { modul=None;name="mean_float_list" }),
                                                                  mk_expression (M.Evar { modul=None;name="output" }))))) in

  let print_statuses = mk_expression (M.Eapp (mk_expression (M.Evar {modul=None;name="pp_approx_status"}),
                                              mk_expression (M.Econst (Cbool (not verbose))))) in

  let main_body = 
    mk_pattern (M.Ptuple [mk_pattern (M.Pid { modul=None;name="body" }); 
                          mk_pattern (M.Ptuple [])]), 
    mk_expression (M.Elet(mk_pattern (M.Ptuple [mk_pattern (M.Pid { modul=None;name="output" }); 
                                                mk_pattern (M.Pid { modul=None;name="body" })]),
                          mk_expression (M.Ecall_step(mk_expression (M.Evar { modul=None;name="body" }), 
                                                      mk_expression (M.Etuple []))), 
                          mk_expression (M.Elet(mk_pattern (M.Pany), 
                                                print_statuses, 
                                                mk_expression (M.Elet(mk_pattern (M.Pany), 
                                                                      print_output,
                                                                      mk_expression (M.Etuple [mk_expression (M.Etuple []);
                                                                                               mk_expression (M.Evar { modul=None;name="body" }) ])))))))
  in
  let main_n : ('m M.pattern, 'm M.expression) M.node =
    { n_type = ([], M.TKrecord []); (* XXX TODO XXX *)
      n_init = mk_expression (M.Einfer (mk_expression (M.Econst(M.Cint particles)), 
                                       { modul=None;name="body" }));
      n_step = main_body; }
  in
  let main : 'm M.declaration = { decl = M.Dnode ("main", [], main_n) } in
  
  decls @ [func; main]
  