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
  | Eifeval of expr * expr * expr
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
module VarSet = Set.Make(String)

let varnum = ref 0
let get_varnum _ =
  varnum := !varnum + 1;
  !varnum

let used_rv_names = ref []

let first_name = "muf_first"
let init_funct_name = "muf_init"

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
  | Pany -> mk_expression (M.Econst (M.Cstring ""))

let rec convert_sample (p: pattern) (a: annotation) (e: expr) : 'm M.expression =
  match p, e with
  (* Single vars *)
  | Pid _, Evar _ | Pid _, Eapp _ | Pany, _ ->
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
  | _ -> failwith "Invalid sample expression"

and convert_expression (e: expr) : 'm M.expression =
  match e with
  | Econst c -> mk_expression (M.Econst (convert_const c))
  | Evar id -> mk_expression (M.Evar (convert_identifier id))
  | Etuple es -> mk_expression (M.Etuple (List.map convert_expression es))
  | Eapp (e1, e2) -> mk_expression (M.Eapp (convert_expression e1, convert_expression e2))
  | Eif (e1, e2, e3) -> mk_expression (M.Eapp (mk_expression (M.Evar { modul=None;name="ite" }),
      mk_expression (M.Etuple [convert_expression e1; convert_expression e2; convert_expression e3])))
  | Eifeval (e1, e2, e3) -> mk_expression (M.Eif (convert_expression e1, convert_expression e2, convert_expression e3))
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

let extract_tuple_inner (e: expr) : expr list =
  match e with
  | Etuple es -> es
  | _ -> failwith "Invalid tuple"

let find_funct (decls: declaration list) (f: string) : declaration =
  List.find (fun (d : declaration) ->
    match d with
    | Dfun (f', _, _) -> f = f'
    | _ -> false
  ) decls

let remove_funct (decls: declaration list) (f: string) : declaration list =
  List.filter (fun (d : declaration) ->
    match d with
    | Dfun (f', _, _) -> f <> f'
    | _ -> true
  ) decls

(* Returns (function name, state, return_vars) *)
let rec find_fold (found : (string * (string * string) * pattern) option) (e : expr)
  : (string * (string * string) * pattern) option =
match e with
| Elet (p, e1, e2) ->
  begin match e1 with
  | Eapp (Evar { modul=Some "List"; name="fold_resample" }, 
          Etuple(
            (Evar {modul=None;name=funct_name})::
            (Evar {modul=None;name=init_var})::
            [(Evar {modul=None;name=list_var})])) ->
    begin match found with
    | None -> find_fold (Some (funct_name, (init_var, list_var), p)) e2
    | Some _ -> failwith "More than one fold found"
    end
  | _ -> find_fold found e2
  end
| _ -> found

(* Get init tuple *)
let rec get_inits (init_tuple : pattern list) (e : expr) : pattern list =
  match e with 
  | Elet (p, e1, e2) ->
    begin match e1 with
    | Eapp (Evar { modul=Some "List"; name="fold_resample" }, 
            Etuple(
              (Evar {modul=None;name=_funct_name})::
              (Evar {modul=None;name=_init_var})::
              [(Evar {modul=None;name=_list_var})])) ->
      get_inits init_tuple e2
    | _ -> 
      begin match p with
      | Pany | Ptuple [] -> get_inits init_tuple e2
      | _ -> get_inits (p::init_tuple) e2
      end
    end
  | _ -> List.rev init_tuple

let rec convert_pattern_to_expr (p : pattern) : expr option =
  match p with 
  | Pid id -> Some (Evar id)
  | Ptuple ps -> Some (Etuple (List.filter_map convert_pattern_to_expr ps))
  (* | Ptype of 'p * type_expression *)
  | Pany -> None

let rec convert_expr_to_pattern (e : expr) : pattern =
  match e with 
  | Evar id -> Pid id
  | Etuple es -> Ptuple (List.map convert_expr_to_pattern es)
  | _ -> failwith "Invalid state expression1"
  
let convert_pattern_list_to_expr_tuple (ps : pattern list) : expr =
  Etuple (List.filter_map convert_pattern_to_expr ps)

let get_init_return_expr (init_expr : expr) (e : expr) (skip_unnamed : bool) : expr * expr =
  let rec get_init_return_expr' (init_expr : expr)  (e : expr) : expr * expr =
    match e with 
    | Elet (p, e1, e2) ->
      begin match e1 with
      | Eapp (Evar { modul=Some "List"; name="fold_resample" }, 
              Etuple(
                (Evar {modul=None;name=_funct_name})::
                (Evar {modul=None;name=_init_var})::
                [(Evar {modul=None;name=_list_var})])) ->
        get_init_return_expr' init_expr e2
      | _ -> 
        let init_expr, return_expr = get_init_return_expr' init_expr e2 in
        let init_expr = match p with
          | Pany -> if skip_unnamed then init_expr else Elet (p, e1, init_expr)
          | _ -> Elet (p, e1, init_expr) in
        init_expr, return_expr
      end
    | _ -> init_expr, e
  in
  let init_expr, return_expr = get_init_return_expr' init_expr e in
  init_expr, return_expr

let patch_return_expr (state: expr) (e: expr) : expr =
  let rec patch_return_expr' (e: expr) : expr =
    match e with
    | Elet (_, _, e2) -> patch_return_expr' e2
    | _ -> Etuple [e; state]
  in
  patch_return_expr' e

let unfold_fold_funct (funct : declaration) (init_var : string) (list_var : string) 
  (funct_return_var : pattern) (return_expr : expr) (state : expr) =

  let local_vars = ref VarSet.empty in

  let rec add_vars (p : pattern) : unit =
    match p with
    | Pid id -> local_vars := VarSet.add id.name !local_vars
    | Ptuple ps -> List.iter add_vars ps
    | Pany -> ()
  in
  
  let rec rename_params (add : bool) (funct_name : string) (p : pattern) : pattern =
    match p with
    | Pid id -> 
      if add then add_vars p;
      if VarSet.mem id.name !local_vars then
        Pid { modul=None; name=funct_name ^ "_" ^ id.name }
      else p
    | Ptuple ps -> Ptuple (List.map (rename_params add funct_name) ps)
    | Pany -> Pany
  in
  let rec rename_vars_expr (funct_name : string) (e : expr) : expr =
    match e with
    | Econst _ -> e
    | Evar id -> 
      if VarSet.mem id.name !local_vars then
        Evar { modul=None; name=funct_name ^ "_" ^ id.name }
      else e
    | Etuple es -> Etuple (List.map (rename_vars_expr funct_name) es)
    | Eapp (e1, e2) -> Eapp (e1, rename_vars_expr funct_name e2)
    | Eif (e1, e2, e3) -> Eif (rename_vars_expr funct_name e1, rename_vars_expr funct_name e2, rename_vars_expr funct_name e3)
    | Eifeval (e1, e2, e3) -> Eifeval (rename_vars_expr funct_name e1, rename_vars_expr funct_name e2, rename_vars_expr funct_name e3)
    | Elet (p, e1, e2) -> 
      let p = rename_params true funct_name p in
      let e1 = rename_vars_expr funct_name e1 in
      Elet (p, e1, rename_vars_expr funct_name e2)
    | Esample (p, a, e) -> Esample (rename_params false funct_name p, a, rename_vars_expr funct_name e)
    | Eobserve (e1, e2) -> Eobserve (rename_vars_expr funct_name e1, rename_vars_expr funct_name e2)
  in

  match funct with 
  | Dfun (funct_name, Ptuple (acc::[x]), e) ->
    (* process input by renaming *)
    let funct_name = "muf_" ^ funct_name in
    let acc = rename_params true funct_name acc in
    let x = rename_params true funct_name x in

    (* rename body vars as well *)
    let e = rename_vars_expr funct_name e in

    (* patch return *)
    let state_inner = extract_tuple_inner state in
    let state = Etuple (state_inner@(List.filter_map convert_pattern_to_expr [acc])) in

    let final_return_expr = Etuple [return_expr; state] in

    (* update list *)
    let inner = 
      Elet(Pid { modul=None; name=list_var }, 
           Eapp (Evar { modul=Some "List"; name="tl" }, Evar { modul=None; name=list_var }),
           final_return_expr) in

    (* embed function body *)
    let inner = Elet(funct_return_var, e, inner) in        

    (* create let bindings for params *)
    let acc_var = match convert_pattern_to_expr acc with
      | Some e -> e
      | None -> failwith "Invalid pattern" in

    let inner =
      Elet(acc, 
           Eifeval(Evar { modul=None; name=first_name }, 
               Evar { modul=None; name=init_var },
               acc_var),
           Elet(x, 
                Eapp (Evar { modul=Some "List"; name="hd" }, Evar { modul=None; name=list_var }), 
                inner)) in
    
    (* Exit if list empty *)
    let inner = 
      Elet(Pany,
           Eifeval(Eapp (Evar { modul=Some "List"; name="empty" }, Evar { modul=None; name=list_var }), 
               Eapp (Evar { modul=None; name="exit"}, Econst (Cint 0)),
               Etuple []),
           inner) in
    inner, state
  | _ -> failwith "Invalid fold function"

let rec filter_named_vars (e : expr) : expr =
  match e with
  | Elet(p, e1, e2) ->
    begin match p with
    | Pany | Ptuple [] -> filter_named_vars e2
    | _ -> 
      let e1 = filter_named_vars e1 in
      let e2 = filter_named_vars e2 in
      Elet(p, e1, e2)
    end
  | Etuple es -> Etuple (List.map filter_named_vars es)
  | Eapp (e1, e2) -> Eapp (filter_named_vars e1, filter_named_vars e2)
  | Eif (e1, e2, e3) -> Eif (filter_named_vars e1, filter_named_vars e2, filter_named_vars e3)
  | Eifeval (e1, e2, e3) -> Eifeval (filter_named_vars e1, filter_named_vars e2, filter_named_vars e3)
  | Eobserve (e1, e2) -> Eobserve (filter_named_vars e1, filter_named_vars e2)
  | Esample (p, a, e) -> Esample (p, a, filter_named_vars e)
  | _ -> e

let get_all_init_expr (state : expr) (e : expr) : expr =
  let rec get_all_init_expr' (state : expr) (e : expr) : expr =
    match e with 
    | Elet (p, e1, e2) ->
      let e2 = get_all_init_expr' state e2 in
      begin match e1 with
      (* This hack can be used bc you can't actaully use Eifeval in the program *)
      | Eifeval(Evar{ modul=None; name=guard}, e1', _) -> 
        if guard = first_name then
          begin match p with
          | Pany | Ptuple [] -> e2
          | _ -> 
            let e1' = filter_named_vars e1' in
            Elet(p, e1', e2)
          end
        else
          failwith (Format.sprintf "Unexpected Eifeval guard %s" guard)
      | _ -> e2
      end
    | _ -> state
  in

  let init_expr = get_all_init_expr' state e in

  (* Make all rvs unnamed since these are dummies 
     and leave as dynamic so it doesn't register as approx *)
  let rec make_unnamed (e : expr) : expr =
    match e with 
    | Esample (_, _, e) -> Esample (Pany, Adynamic, make_unnamed e)
    | Etuple es -> Etuple (List.map make_unnamed es)
    | Eapp (e1, e2) -> Eapp (e1, make_unnamed e2)
    | Eif (e1, e2, e3) -> Eif (make_unnamed e1, make_unnamed e2, make_unnamed e3)
    | Eifeval (e1, e2, e3) -> Eifeval (make_unnamed e1, make_unnamed e2, make_unnamed e3)
    | Elet (p, e1, e2) -> Elet (p, make_unnamed e1, make_unnamed e2)
    | Eobserve (e1, e2) -> Eobserve (make_unnamed e1, make_unnamed e2)
    | _ -> e
  in
  make_unnamed init_expr

(* Returns decls without the step function, the unfolded expr, the state, and the init state *)
let unfold_fold (decls: declaration list) (e : expr) : declaration list * expr * expr =
  let init_names = get_inits [] e in
  let init_tuple = convert_pattern_list_to_expr_tuple init_names in
  let init_expr, return_expr = get_init_return_expr init_tuple e false in
    
  let init_expr = 
    Eifeval (Evar {modul = None; name = first_name}, 
            init_expr,
            init_tuple)
  in

  let init_tuple_inner = extract_tuple_inner init_tuple in
  let state = Etuple (Econst (Cbool false)::init_tuple_inner) in

  let fold_res = find_fold None e in
  match fold_res with 
  | None -> 
    (* patch return *)
    let inner = patch_return_expr state e in

    (* exit after first *)
    let inner = 
      Elet(Pany,
           Eifeval(Evar { modul=None; name=first_name }, 
               Etuple [],
               Eapp (Evar { modul=None; name="exit"}, Econst (Cint 0))),
           inner) in

    let inner = 
      Elet(Ptuple init_names,
          init_expr,
          inner) in

    (* This is for the hacky init function to feed to the init *)
    let init_state = Etuple (Econst (Cbool true)::init_tuple_inner) in
    let init_funct_expr = get_all_init_expr init_state inner in
    (* create init function *)
    let init_funct = 
      Dfun (init_funct_name, 
            Ptuple [], 
            init_funct_expr) in
    let decls = decls@[init_funct] in
    
    decls, inner, state
  | Some (funct_name, (init_var, list_var), fold_return_var) ->
    Format.printf "Found fold function %s\n" funct_name;
    Format.printf "Init var %s\n" init_var;
    Format.printf "List var %s\n" list_var;
    Format.printf "Fold return var %s\n" (show_pattern fold_return_var);

    let funct = find_funct decls funct_name in
    let decls = remove_funct decls funct_name in

    let inner, state = unfold_fold_funct 
      funct init_var list_var fold_return_var return_expr state in

    (* create let bindings for inits *)
    let inner =
      Elet(Ptuple init_names,
          init_expr,
          inner) in

    (* This is for the hacky init function to feed to the init *)
    (* Remove the first bool argument *)
    let state_inner = List.tl (extract_tuple_inner state) in
    let init_state = Etuple (Econst (Cbool true)::state_inner) in
    let init_funct_expr = get_all_init_expr init_state inner in
    (* create init function *)
    let init_funct = 
      Dfun (init_funct_name, 
            Ptuple [], 
            init_funct_expr) in
    let decls = decls@[init_funct] in
    
    decls, inner, state

let convert_state_to_param (e : expr) : 'm M.pattern =
  match e with
  (* first item should always be first var *)
  | Etuple (Econst (Cbool _)::s) -> 
    let state = Etuple (Evar { modul=None;name=first_name }::s) in
    convert_pattern (convert_expr_to_pattern state)
  | _ -> failwith "Invalid state expression"

(* TODO:  
  - fold assumed to be at top level let expression 
    and all inputs to it must be saved to variables
  - more than one folds
    - uneven length folds
    - what happens if nested folds?
*)
let convert_to_intern (particles: int) (verbose: bool) (p: 'm program) : 'm M.program =
  let decls, e = p in
  let decls, e, state = unfold_fold decls e in
  let decls = convert_declarations decls in
  let e = convert_expression e in
  let state = convert_state_to_param state in

  let body_n : ('m M.pattern, 'm M.expression) M.node =
    { n_type = ([], M.TKrecord []); (* XXX TODO XXX *)
      n_init = mk_expression (M.Eapp (mk_expression (M.Evar { modul=None;name=init_funct_name }), 
                                     mk_expression (M.Etuple [])));
      n_step = (mk_pattern (M.Ptuple [state; mk_pattern (M.Ptuple [])]), e); }
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
  