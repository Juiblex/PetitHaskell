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

type tident = string

type const =
    | Cbool of bool
    | Cint of int
    | Cchar of char

type texpr = {
    tdesc: tdesc;
    typ: typ
}

and tdesc = 
   | TEvar of tident
   | TEconst of const
   | TEapp of texpr * texpr
   | TEabs of tident * texpr
   | TEbinop of binop * texpr * texpr
   | TEnil (* the empty list *)
   | TEcond of texpr * texpr * texpr
   | TElet of tdef list * texpr
   | TEcase of texpr * texpr * tident * tident * texpr
   | TEdo of texpr list
   | TEreturn

and tdef = {
    tname: tident;
    tbody: texpr
}

type tprogram = {tdefs: tdef list}

(* Lazy abstract syntax *)

type lident = string

type lexpr =
    | LEvar of lident
    | LEconst of const
    | LEapp of lexpr * lexpr
    | LEabs of lident * lexpr
    | LEbinop of binop * lexpr * lexpr
    | LEnil
    | LEcond of lexpr * lexpr * lexpr
    | LElet of ldef list * lexpr
    | LEcase of lexpr * lexpr * lident * lident * lexpr
    | LEdo of lexpr list
    | LEreturn
    | LEthunk of lexpr

and ldef = {
    lname: lident;
    lbody: lexpr
}

type lprogram = {ldefs: ldef list}
