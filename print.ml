open Ast

(* Just a large function for printing the AST *)

let print_bool = function
    | true -> print_string "True"
    | false -> print_string "False"

let affiche p =
    let prof = ref 0 in
    let hyphens () =
        for i = 0 to 4* !prof - 1 do
            print_char '-'
        done;
        print_string " ";
    in
    let p_binop b =
        hyphens ();
        let s = match b with
            | Badd -> "Badd"
            | Bsub -> "Bsub"
            | Bmul -> "Bmul"
            | Blt -> "Blt"
            | Ble -> "Ble"
            | Bgt -> "Bgt"
            | Bge -> "Bge"
            | Beq -> "Beq"
            | Bneq -> "Bneq"
            | Band -> "Band"
            | Bor -> "Bor"
            | Bconc -> "Bconc"
        in print_endline s
    in
    let p_pos p =
        if p.slin = p.elin then
            Printf.printf "line %d, characters %d-%d\n" p.slin p.scol p.ecol
        else
            Printf.printf "line %d character %d - line %d character %d\n"
                p.slin p.scol p.elin p.ecol
    in
    let rec p_const c =
        hyphens ();
        begin match c with
            | PCbool b -> print_string "PCbool "; print_bool b;
            | PCint i -> print_string "PCint "; print_int i;
            | PCchar c -> Printf.printf "'%c'" c
            | PCstring (PElist (l, p)) -> print_string "PCstring "; p_pos p;
                List.iter p_expr l
            | PCstring _ -> failwith "String error"
        end;
        print_newline ();
    and p_expr exp =
        hyphens ();
        incr prof;
        begin match exp with
            | PEid (i, p) -> print_string "PEid "; p_pos p; print_endline i;
            | PEconst(c, p) -> print_string "PEconst "; p_pos p; p_const c
            | PEapp (el, p) -> print_string "PEapp "; p_pos p;
                List.iter p_expr el;
            | PEabs (is, e, p) -> print_string "PEabs "; p_pos p; 
                List.iter (Printf.printf "%s ") is; print_newline (); p_expr e
            | PEuminus(e, p) -> print_string "PEuminus "; p_pos p; p_expr e
            | PEbinop(b, e1, e2, p) -> print_string "PEbinop "; p_pos p; p_binop b;
                p_expr e1; p_expr e2;
            | PElist(l, p) -> print_string "PElist "; p_pos p; List.iter p_expr l
            | PEcond (e1, e2, e3, p) -> print_string "PEcond "; p_pos p; 
                p_expr e1; p_expr e2; p_expr e3
            | PElet (ds, e, p) -> print_string "PElet "; p_pos p;
                List.iter p_def ds; p_expr e
            | PEcase (e1, e2, i1, p1, i2, p2, e3, p) -> print_string "PEcase ";
                p_pos p; p_expr e1; p_expr e2; p_expr (PEid(i1, p1));
                p_expr (PEid(i2, p2)); p_expr e3
            | PEdo(l, p) -> print_string "PEdo "; p_pos p; List.iter p_expr l
            | PEreturn p-> print_string "PEreturn "; p_pos p 
        end;
        decr prof;
    and p_def d =
        Printf.printf "def: %s" d.name;
        print_newline ();
        incr prof;
        p_expr d.body;
        decr prof;
    in List.iter p_def p.defs
