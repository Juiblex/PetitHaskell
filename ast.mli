(* Parsing abstract syntax, with the sugar left *)
type ident = string

type binop =
    | Badd | Bsub | Bmul 
    | Blt | Ble | Bgt | Bge | Beq | Bneq
    | Band | Bor
    | Bconc
    
type pconst = 
    | PCbool of bool
    | PCint of int
    | PCchar of char
    | PCstring of pexpr

and pexpr = 
    | PEid of ident
    | PEconst of pconst
    | PEapp of pexpr * pexpr
    | PEabs of ident list * pexpr
    | PEuminus of pexpr
    | PEbinop of binop * pexpr * pexpr
    | PElist of pexpr list
    | PEcond of pexpr * pexpr * pexpr
    | PElet of pdef list * pexpr
    | PEcase of pexpr * pexpr * ident * ident * pexpr
    | PEdo of pexpr list
    | PEreturn

and pdef = { (* f : x1 ... xn -> v *)
    name    : ident; (* f *)
    body    : pexpr (* \x1 ... xn . v *)
}

type pprogram = {defs : pdef list}
