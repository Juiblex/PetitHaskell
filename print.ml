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
            | Cbool b -> print_string "Cbool "; print_bool b;
            | Cint i -> print_string "Cint "; print_int i;
            | Cchar c -> Printf.printf "'%c'" c
            | Cstring (Elist l) -> print_endline "Cstring"; List.iter p_expr l
            | Cstring _ -> failwith "String error"
        end;
        print_newline ();
    and p_expr exp =
        hyphens ();
        incr prof;
        begin match exp with
            | Eid i -> print_string "Eid "; print_endline i;
            | Econst c -> print_endline "Econst "; p_const c
            | Eapp (e1, e2) -> print_endline "Eapp"; p_expr e1; p_expr e2
            | Eabs (i, e) -> print_string "Eabs "; print_endline i; p_expr e
            | Euminus e -> print_endline "Euminus"; p_expr e
            | Ebinop(b, e1, e2) -> print_endline "Ebinop"; p_binop b;
                p_expr e1; p_expr e2;
            | Elist l -> print_endline "Elist"; List.iter p_expr l
            | Econd (e1, e2, e3) -> print_endline "Econd";
                p_expr e1; p_expr e2; p_expr e3
            | Elet (d, e) -> print_endline "Elet"; p_def d; p_expr e
            | Ecase (e1, e2, i1, i2, e3) -> print_endline "Ecase"; p_expr e1;
                p_expr e2; p_expr (Eid i1); p_expr (Eid i2); p_expr e3
            | Edo l -> print_endline "Edo"; List.iter p_expr l
            | Ereturn -> print_endline "Ereturn"
        end;
        decr prof;
    and p_def d =
        Printf.printf "def: %s" d.name;
        print_newline ();
        incr prof;
        p_expr d.body;
        decr prof;
    in List.iter p_def p.defs
