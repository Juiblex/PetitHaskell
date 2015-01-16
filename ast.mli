type binop =
    | Badd | Bsub | Bmul 
    | Blt | Ble | Bgt | Bge | Beq | Bne
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
    | PCint of int
    | PCchar of char
    | PCbool of bool
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
    | Tint
    | Tchar
    | Tbool
    | Tio
    | Tlist of typ
    | Tarrow of typ * typ
    | Tvar of tvar

and tvar = {id: int; mutable def: typ option}

type ident = string

type const =
    | Cint of int
    | Cchar of char
    | Cbool of bool

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

type cexpr =
    | CEvar of ident
    | CEconst of const
    | CEapp of cexpr * cexpr
    | CEclos of ident * ident list
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
    | CDletfun of ident * ident * ident list * cexpr (* f x [env] = e *)


type cprogram = {cdefs: cdef list}

(* Variable-allocated abstract syntax *)

type vvar =
    | VVglob of ident
    | VVloc of int (* located at n($fp) *)
    | VVclos of int (* located at n(closure address) *)
    | VVarg

type vexpr =
    | VEvar of vvar
    | VEconst of const
    | VEapp of vexpr * vexpr
    | VEclos of ident * vvar list
    | VEbinop of binop * vexpr * vexpr
    | VEnil
    | VEcond of vexpr * vexpr * vexpr
    | VElet of (int * vexpr) list * vexpr
    | VEcase of vexpr * vexpr * int * int * vexpr
    | VEdo of vexpr list
    | VEreturn
    | VEthunk of vexpr

and vdef =
    | VDlet of ident * vexpr * int
    | VDletfun of ident * vexpr * int

type vprogram = {vdefs: vdef list}
