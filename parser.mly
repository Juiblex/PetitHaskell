%{
    open Ast
    open Lexing

    let loc startpos endpos =
        { slin = startpos.pos_lnum; scol = startpos.pos_cnum-startpos.pos_bol;
          elin = endpos.pos_lnum; ecol = endpos.pos_cnum-endpos.pos_bol } 

%}

%token EOF
%token <Ast.pconst> CONST
%token <Ast.pident> IDENT0
%token <Ast.pident> IDENT1
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

%type <Ast.pprogram> prog

%%

prog:
|   defs = def0*; EOF { {pdefs = defs} }
;

def0:
|   name = IDENT0; args = IDENT1*; EQSIGN; body = expr
        { {pname = name; pbody =
            (if args = [] then body else
                {pdesc = PEabs(args, body); pos = loc $startpos $endpos})} }
;

def1:
|   name = IDENT1; args = IDENT1*; EQSIGN; body = expr
        { {pname = name; pbody =
            (if args = [] then body else
                {pdesc = PEabs(args, body); pos = loc $startpos $endpos})} }
;

simple_expr:
|   LP; e = expr; RP { e }
|   var = IDENT1 { {pdesc = PEvar var; pos = loc $startpos $endpos} }
|   cst = CONST { {pdesc = PEconst cst; pos = loc $startpos $endpos} }
|   LB; l = separated_list(COMMA, expr); RB
        { {pdesc = PElist l; pos = loc $startpos $endpos} }
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
|   fa = simple_expr+ { if List.length fa = 1 then (List.hd fa) else
        {pdesc = PEapp fa; pos = loc $startpos $endpos} }

|   MINUS; e = expr; %prec UMINUS
        { {pdesc = PEuminus e; pos = loc $startpos $endpos} }

|   e1 = expr; bin = binop; e2 = expr
        { {pdesc = PEbinop(bin, e1, e2); pos = loc $startpos $endpos} }

|   ABST; vars = IDENT1+; ARROW; e = expr
        { {pdesc = PEabs(vars, e); pos = loc $startpos $endpos} }

|   IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr
        { {pdesc = PEcond(e1, e2, e3); pos = loc $startpos $endpos} }

|   LET; defs = links; IN; e = expr
        { {pdesc = PElet(defs, e); pos = loc $startpos $endpos} }

|   CASE; matched = expr; OF; BEGIN; LB; RB; ARROW; empty = expr; SEMICOLON;
        hd = IDENT1; COLON; tl = IDENT1; ARROW; nonempty = expr; SEMICOLON?; END
        { {pdesc = PEcase(matched, empty, hd, tl, nonempty);
            pos = loc $startpos $endpos} }

|   DO; BEGIN; es = nonempty_sep_maybe(expr); END
        { {pdesc = PEdo es; pos = loc $startpos $endpos} }

|   RETURN; LP; RP; { {pdesc = PEreturn; pos = loc $startpos $endpos} }
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
| COLON { Bcons }
;
