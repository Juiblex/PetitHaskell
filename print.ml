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
    let rec p_const c =
        hyphens ();
        begin match c with
            | PCbool b -> print_string "PCbool "; print_bool b;
            | PCint i -> print_string "PCint "; print_int i;
            | PCchar c -> Printf.printf "'%c'" c
            | PCstring (PElist l) -> print_endline "PCstring"; List.iter p_expr l
            | PCstring _ -> failwith "String error"
        end;
        print_newline ();
    and p_expr exp =
        hyphens ();
        incr prof;
        begin match exp with
            | PEid i -> print_string "PEid "; print_endline i;
            | PEconst c -> print_endline "PEconst "; p_const c
            | PEapp (e1, e2) -> print_endline "PEapp"; p_expr e1; p_expr e2
            | PEabs (is, e) -> print_string "PEabs ";
                List.iter (Printf.printf "%s ") is; print_newline (); p_expr e
            | PEuminus e -> print_endline "PEuminus"; p_expr e
            | PEbinop(b, e1, e2) -> print_endline "PEbinop"; p_binop b;
                p_expr e1; p_expr e2;
            | PElist l -> print_endline "PElist"; List.iter p_expr l
            | PEcond (e1, e2, e3) -> print_endline "PEcond";
                p_expr e1; p_expr e2; p_expr e3
            | PElet (ds, e) -> print_endline "PElet"; List.iter p_def ds; p_expr e
            | PEcase (e1, e2, i1, i2, e3) -> print_endline "PEcase"; p_expr e1;
                p_expr e2; p_expr (PEid i1); p_expr (PEid i2); p_expr e3
            | PEdo l -> print_endline "PEdo"; List.iter p_expr l
            | PEreturn -> print_endline "PEreturn"
        end;
        decr prof;
    and p_def d =
        Printf.printf "def: %s" d.name;
        print_newline ();
        incr prof;
        p_expr d.body;
        decr prof;
    in List.iter p_def p.defs
