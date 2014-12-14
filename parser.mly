%{
    open Ast
%}

%token EOF
%token <Ast.const> CONST
%token <Ast.ident> IDENT0
%token <Ast.ident> IDENT1
%token PLUS MINUS TIMES
%token UMINUS
%token EQSIGN LP RP
%token LEST LEE GRT GRE EQ NEQ AND OR COLON
%token LB COMMA RB
%token ABST ARROW
%token IF THEN ELSE
%token LET IN
%token CASE OF SEMICOLON
%token DO BEGIN END
%token RETURN

%nonassoc IN
%nonassoc ELSE
%nonassoc ARROW
%left OR
%left AND
%left LEST LEE GRT GRE EQ NEQ
%right COLON
%left PLUS MINUS
%left TIMES
%nonassoc UMINUS

%start prog

%type <Ast.program> prog

%%

prog:
|   defs = def0*; EOF { {defs = defs} }
;

def0:
|   name = IDENT0; args = IDENT1*; EQSIGN; body = expr
        { {name = name; body = List.fold_right (fun x y -> Eabs(x, y)) args body} }
;

def1:
|   name = IDENT1; args = IDENT1*; EQSIGN; body = expr
        { {name = name; body = List.fold_right (fun x y -> Eabs(x, y)) args body} }
;

simple_expr:
|   LP; e = expr; RP { e }
|   var = IDENT1 { Eid var }
|   cst = CONST { Econst cst }
|   LB; l = separated_list(COMMA, expr); RB { Elist l }
;

sep_maybe(t):
    | e = t; SEMICOLON; l = sep_maybe(t) { e::l }
    | e = t { [e] }
    | { [] }
;

nonempty_sep_maybe(t):
    | e = t; SEMICOLON; l = sep_maybe(t) { e::l }
    | e = t { [e] }
;

links:
    | d = def1 { [d] }
    | BEGIN; ds = nonempty_sep_maybe(def1); END { ds }
;

expr:
|   fa = simple_expr+
        { List.fold_left (fun x y -> Eapp(x, y)) (List.hd fa) (List.tl fa) }

|   MINUS; e = expr; %prec UMINUS { Euminus e }

|   e1 = expr; bin = binop; e2 = expr { Ebinop (bin, e1, e2) }

|   ABST; vars = IDENT1+; ARROW; e = expr
        { List.fold_right (fun x y -> Eabs(x, y)) vars e }

|   IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { Econd(e1, e2, e3) }

|   LET; defs = links; IN; ex = expr
    { List.fold_right (fun d e -> Elet(d, e)) defs ex }

|   CASE; matched = expr; OF; BEGIN; LB; RB; ARROW; empty = expr; SEMICOLON;
        hd = IDENT1; COLON; tl = IDENT1; ARROW; nonempty = expr; SEMICOLON?; END
        { Ecase(matched, empty, hd, tl, nonempty) }

|   DO; BEGIN; es = nonempty_sep_maybe(expr); END { Edo es }

|   RETURN; LP; RP; { Ereturn }
;

%inline binop:
| PLUS { Badd }
| MINUS { Bsub }
| TIMES { Bmul }
| LEST { Blt }
| LEE { Ble }
| GRT { Bgt }
| GRE { Bge }
| EQ { Beq }
| NEQ { Bneq }
| AND { Band }
| OR { Bor }
| COLON { Bconc }
;
