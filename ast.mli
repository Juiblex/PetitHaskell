(* Parsing abstract syntax, with some sugar left *)
type ident = string

type binop =
    | Badd | Bsub | Bmul 
    | Blt | Ble | Bgt | Bge | Beq | Bneq
    | Band | Bor
    | Bconc
    
type const = 
    | Cbool of bool
    | Cint of int
    | Cchar of char
    | Cstring of expr

and expr = 
    | Eid of ident
    | Econst of const
    | Eapp of expr * expr
    | Eabs of ident * expr
    | Euminus of expr
    | Ebinop of binop * expr * expr
    | Elist of expr list
    | Econd of expr * expr * expr
    | Elet of def * expr
    | Ecase of expr * expr * ident * ident * expr
    | Edo of expr list
    | Ereturn

and def = { (* f : x -> v *)
    name    : ident; (* f *)
    body    : expr (* \x.v *)
}

type program = {defs : def list}
