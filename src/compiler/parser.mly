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
%token OBSERVE VALUE RESAMPLE
// %token BOOLT INTT FLOATT
// %token DIST UNIT ARRAY LIST
%token EXACT APPROX

%token DOT
%token EQUAL RARROW LARROW
%token LPAREN RPAREN LSQUARE RSQUARE
%token COMMA SEMI
%token EOF
%token UNDERSCORE

%start <Mufextern.program> program

%%

program:
| e = expr EOF
    { [], e }
| d = decl p = program
    { List.cons d (fst p), snd p }

decl:
| OPEN m = IDENT
    { Dopen m }
(* Function *)
| VAL x = IDENT EQUAL FUN p = patt RARROW e = expr
    { Dfun (x, p, e) }
// | VAL x = patt EQUAL e = expr
//     { Ddecl (x, e) }

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
| LPAREN RPAREN { Econst (Cunit) }
(* Tuple *)
| LPAREN e1 = simple_expr COMMA el = separated_nonempty_list(COMMA, simple_expr) RPAREN
    { Epair (e1 :: el) }
(* Call unit *)
| e1 = simple_expr LPAREN RPAREN
    { Eapp (e1, Econst (Cunit)) }
| VALUE LPAREN e1 = simple_expr RPAREN
    { Evalue e1 }
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
//     { Enil }
// | CONS LPAREN e1 = simple_expr COMMA e2 = simple_expr RPAREN
//     { Econs (e1, e2) }
| LSQUARE e1 = simple_expr RSQUARE
    { Elist ([e1]) }
| LSQUARE e1 = simple_expr SEMI el = separated_nonempty_list(SEMI, simple_expr) RSQUARE
    { Elist (e1 :: el) }

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
| RESAMPLE LPAREN RPAREN
    { Eresample }
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
| LPAREN RPAREN { Punit }
// | x = IDENT COLON t = typ
//     { mk_patt (Ptype (mk_patt (Pid { modul = None; name = x }), t))}
| UNDERSCORE { Pany }