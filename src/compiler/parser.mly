%{
  open Mufextern
%}

%token <bool> BOOL
%token <int> INT
%token <string> FLOAT
%token <string> STRING
%token <string> IDENT

%token OPEN
%token LET IN FUN VAL
%token IF THEN ELSE
%token OBSERVE
// %token BOOLT INTT FLOATT
// %token DIST UNIT ARRAY LIST
%token EXACT APPROX

%token DOT
%token EQUAL RARROW LARROW
%token LPAREN RPAREN LSQUARE RSQUARE
%token COMMA SEMI
%token EOF
%token COLON
%token STAR
%token UNDERSCORE

// %token NIL CONS

%start <unit Mufextern.program> program

%%

program:
| e = expr EOF
    { [], e }
| p = list(decl) e = expr EOF
    { p, e }


decl:
| OPEN m = IDENT
    { Dopen m }
(* Function *)
| VAL x = IDENT EQUAL FUN p = patt RARROW e = expr
    { Dfun (x, p, e) }
// | VAL x = patt EQUAL e = expr
//     { Ddecl (x, e) }
// | LET x = IDENT EQUAL STREAM LCURLY INIT EQUAL e_init = expr SEMI step = IDENT p = patt EQUAL e_step = expr RCURLY
//     { begin match step with "step" -> () | _ -> failwith "step expected" end;
//       let n =
//         { n_type = ([], TKrecord []); (* XXX TODO XXX *)
//           n_init = e_init;
//           n_step = (p, e_step); }
//       in
//       { decl = Dnode (x, [], n) } }

simple_expr:
(* Parenthesized expression *)
| LPAREN e = expr RPAREN
    { e }
(* Constants *)
| b = BOOL
    { Econst (Cbool b) }
| i = INT
    { Econst (Cint i) }
| f = FLOAT
    { Econst (Cfloat f) }
| s = STRING
    { Econst (Cstring s) }
(* Variable *)
| x = IDENT
    { Evar { modul = None; name = x } }
| m = IDENT DOT x = IDENT
    { Evar { modul = Some m; name = x } }
(* Unit *)
| LPAREN RPAREN { Etuple [] }
(* Tuple *)
| LPAREN e1 = simple_expr COMMA el = separated_nonempty_list(COMMA, simple_expr) RPAREN
    { Etuple (e1 :: el) }
(* Call unit *)
| e1 = simple_expr LPAREN RPAREN
    { Eapp (e1, Etuple []) }
(* Call *)
| e1 = simple_expr LPAREN e2 = expr RPAREN
    { Eapp (e1, e2) }
(* Call Tuple *)
| e1 = simple_expr LPAREN e2 = simple_expr COMMA el = separated_nonempty_list(COMMA, simple_expr) RPAREN
    { Eapp (e1, Etuple (e2 :: el)) }
(* Array *)
// | LSQUARE el = separated_nonempty_list(SEMI, simple_expr) RSQUARE
//     { mk_expr (Earray el) }
// | LSQUARE RSQUARE
//     { mk_expr (Earray []) }
(* List *)
// | NIL
//     { Evar { modul = Some "List"; name = "nil"} }
// | CONS LPAREN e1 = simple_expr COMMA e2 = simple_expr RPAREN
//     { Eapp (Evar { modul = Some "List"; name = "cons"}, Etuple [e1; e2]) }

expr:
| e = simple_expr
    { e }
(* Conditional *)
| IF v = simple_expr THEN e1 = expr ELSE e2 = expr
    { Eif (v, e1, e2) }
(* Local binding *)
| LET x = patt EQUAL e1 = expr IN e2 = expr
    { Elet (x, e1, e2) }
| LET EXACT x = patt LARROW v = simple_expr IN e = expr
    { Elet (x, Esample (x, Aexact, v), e) }
| LET APPROX x = patt LARROW v = simple_expr IN e = expr
    { Elet (x, Esample (x, Aapprox, v), e) }
| LET x = patt LARROW v = simple_expr IN e = expr
  { Elet (x, Esample (x, Adynamic, v), e) }
| OBSERVE LPAREN e1 = simple_expr COMMA e2 = simple_expr RPAREN
    { Eobserve (e1, e2) }

// typ:
// | INTT { Tany }
// | FLOATT { Tany }
// | BOOLT { Tany }
// | t = typ DIST { Tconstr ("dist", [t]) }
// | UNIT { Ttuple [] }
// | LPAREN t = typ STAR tl = separated_nonempty_list(STAR, typ) RPAREN { Ttuple (t::tl) }
// | UNDERSCORE { Tany }
// | t = typ ARRAY { Tconstr ("array", [t]) }
// | t = typ LIST { Tconstr ("list", [t]) }

patt:
| x = IDENT
    { Pid { modul = None; name = x } }
| LPAREN p1 = patt COMMA pl = separated_nonempty_list(COMMA, patt) RPAREN
    { Ptuple (p1::pl) }
| LPAREN RPAREN { Ptuple [] }
// | x = IDENT COLON t = typ
//     { mk_patt (Ptype (mk_patt (Pid { modul = None; name = x }), t))}
| UNDERSCORE { Pany }