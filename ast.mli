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
    | PEid of pident
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

type tconst =
    | TCbool of bool
    | TCint of int
    | TCchar of char

type texpr = {
    tdesc: tdesc;
    typ: typ
}

and tdesc = 
   | TEid of tident
   | TEconst of tconst
   | TEapp of texpr * texpr
   | TEabs of tident * texpr
   | TEuminus of texpr
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
