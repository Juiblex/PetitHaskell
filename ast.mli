type ident = string

type position = {
    slin: int;
    elin: int;
    scol: int;
    ecol: int
}

type binop =
    | Badd | Bsub | Bmul 
    | Blt | Ble | Bgt | Bge | Beq | Bneq
    | Band | Bor
    | Bconc
    
(* Parsing abstract syntax, with the sugar left *)

type pconst = 
    | PCbool of bool
    | PCint of int
    | PCchar of char
    | PCstring of pexpr

and pexpr = 
    | PEid of ident * position
    | PEconst of pconst * position
    | PEapp of pexpr list * position
    | PEabs of ident list * pexpr * position
    | PEuminus of pexpr * position
    | PEbinop of binop * pexpr * pexpr * position
    | PElist of pexpr list * position
    | PEcond of pexpr * pexpr * pexpr * position
    | PElet of pdef list * pexpr * position
    | PEcase of pexpr * pexpr * ident * position * ident * position * pexpr * position
    | PEdo of pexpr list * position
    | PEreturn of position

and pdef = { (* f : x1 ... xn -> v *)
    name    : ident; (* f *)
    body    : pexpr (* \x1 ... xn . v *)
}

type pprogram = {defs : pdef list}
