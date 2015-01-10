type binop =
    | Badd | Bsub | Bmul 
    | Blt | Ble | Bgt | Bge | Beq | Bneq
    | Band | Bor
    | Bcons
    
(* Parsing abstract syntax, with sugar *)

type position = {
    slin: int;
    elin: int;
    scol: int;
    ecol: int
}

type pident = {
    pid: string;
    pos: position;
}


type pconst = 
    | PCbool of bool
    | PCint of int
    | PCchar of char
    | PCstring of pexpr

and pexpr = {
    pdesc: pdesc;
    pos: position 
}

and pdesc = 
    | PEvar of pident
    | PEconst of pconst
    | PEapp of pexpr list
    | PEabs of pident list * pexpr
    | PEuminus of pexpr
    | PEbinop of binop * pexpr * pexpr
    | PElist of pexpr list
    | PEcond of pexpr * pexpr * pexpr
    | PElet of pdef list * pexpr
    | PEcase of pexpr * pexpr * pident * pident * pexpr
    | PEdo of pexpr list 
    | PEreturn

and pdef = { (* f : x1 ... xn -> v *)
    pname    : pident; (* f *)
    pbody    : pexpr (* \x1 ... xn . v *)
}

type pprogram = {pdefs : pdef list}

(* Typing abstract syntax, sugar-free *)

type typ =
    | Tbool
    | Tchar
    | Tint
    | Tio
    | Tlist of typ
    | Tarrow of typ * typ
    | Tvar of tvar

and tvar = {id: int; mutable def: typ option}

type ident = string

type const =
    | Cbool of bool
    | Cint of int
    | Cchar of char

type texpr = {
    tdesc: tdesc;
    typ: typ
}

and tdesc = 
   | TEvar of ident
   | TEconst of const
   | TEapp of texpr * texpr
   | TEabs of ident * texpr
   | TEbinop of binop * texpr * texpr
   | TEnil (* the empty list *)
   | TEcond of texpr * texpr * texpr
   | TElet of tdef list * texpr
   | TEcase of texpr * texpr * ident * ident * texpr
   | TEdo of texpr list
   | TEreturn

and tdef = {
    tname: ident;
    tbody: texpr
}

type tprogram = {tdefs: tdef list}

(* Lazy abstract syntax *)

type lexpr =
    | LEvar of ident
    | LEconst of const
    | LEapp of lexpr * lexpr
    | LEabs of ident * lexpr
    | LEbinop of binop * lexpr * lexpr
    | LEnil
    | LEcond of lexpr * lexpr * lexpr
    | LElet of ldef list * lexpr
    | LEcase of lexpr * lexpr * ident * ident * lexpr
    | LEdo of lexpr list
    | LEreturn
    | LEthunk of lexpr

and ldef = {
    lname: ident;
    lbody: lexpr
}

type lprogram = {ldefs: ldef list}

(* Closure-converted abstract syntax *)

type cvar =
    | Vvar of ident
    | Varg

type cexpr =
    | CEvar of cvar
    | CEconst of const
    | CEapp of cexpr * cexpr
    | CEclos of ident * cvar list
    | CEbinop of binop * cexpr * cexpr
    | CEnil
    | CEcond of cexpr * cexpr * cexpr
    | CElet of (ident * cexpr) list * cexpr
    | CEcase of cexpr * cexpr * ident * ident * cexpr
    | CEdo of cexpr list
    | CEreturn
    | CEthunk of cexpr
    
and cdef =
    | CDlet of ident * cexpr
    | CDletfun of ident * cexpr


type cprogram = {cdefs: cdef list}

(* Variable-allocated abstract syntax *)
