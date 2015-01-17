open Ast

(* Just a large function for printing the AST *)

let bool_to_str = function
    | true -> "True"
    | false -> "False"

let bin_to_str = function
    | Badd -> "Badd"
    | Bsub -> "Bsub"
    | Bmul -> "Bmul"
    | Blt -> "Blt"
    | Ble -> "Ble"
    | Bgt -> "Bgt"
    | Bge -> "Bge"
    | Beq -> "Beq"
    | Bne -> "Bne"
    | Band -> "Band"
    | Bor -> "Bor"
    | Bcons -> "Bcons"

let hyphens p =
    for i = 0 to 4* p - 1 do
        print_char '-'
    done;
    if p <> 0 then print_string " "

let p_pos p =
    if p.slin = p.elin then
        Printf.printf "line %d, characters %d-%d\n" p.slin p.scol p.ecol
    else
        Printf.printf "line %d character %d - line %d character %d\n"
            p.slin p.scol p.elin p.ecol

let paffiche p =
    let prof = ref 0 in
    let rec p_const c =
        hyphens !prof;
        begin match c with
            | PCbool b -> Printf.printf "PCbool %s" (bool_to_str b)
            | PCint i -> Printf.printf "PCint %d" i
            | PCchar c -> Printf.printf "PCchar '%c'" c
            | PCstring { pdesc = PElist l; pos = p } -> 
                print_string "PCstring "; p_pos p;
                List.iter p_expr l
            | PCstring _ -> failwith "String error"
        end;
        print_newline ();
    and p_ident {pid = i; pos = p} =
        Printf.printf "%s " i; p_pos p;
    and p_expr {pdesc = exp; pos = p} =
        hyphens !prof;
        incr prof;
        begin match exp with
            | PEvar id -> print_string "PEvar "; p_ident id
            | PEconst c -> print_string "PEconst "; p_pos p; p_const c
            | PEapp el -> print_string "PEapp "; p_pos p;
                List.iter p_expr el;
            | PEabs(is, e) -> print_string "PEabs "; p_pos p; 
                List.iter p_ident is; p_expr e
            | PEuminus e -> print_string "PEuminus "; p_pos p; p_expr e
            | PEbinop(b, e1, e2) -> Printf.printf "PEbinop %s " (bin_to_str b);
                p_pos p; p_expr e1; p_expr e2
            | PElist l -> print_string "PElist "; p_pos p; List.iter p_expr l
            | PEcond(e1, e2, e3) -> print_string "PEcond "; p_pos p; 
                p_expr e1; p_expr e2; p_expr e3
            | PElet(ds, e) -> print_string "PElet "; p_pos p;
                List.iter p_def ds; p_expr e
            | PEcase(e1, e2, i1, i2, e3) -> print_string "PEcase ";
                p_pos p; p_expr e1; p_expr e2; p_ident i1; p_ident i2; p_expr e3
            | PEdo l -> print_string "PEdo "; p_pos p; List.iter p_expr l
            | PEreturn -> print_string "PEreturn "; p_pos p 
        end;
        decr prof;
    and p_def d =
        hyphens !prof;
        Printf.printf "def: %s\n" d.pname.pid;
        incr prof;
        p_expr d.pbody;
        decr prof;
    in List.iter p_def p.pdefs

let rec trec = function
    | Tbool -> print_string "Bool"
    | Tchar -> print_string "Char"
    | Tint -> print_string "Integer"
    | Tio -> print_string "IO ()"
    | Tlist t -> print_char '['; trec t; print_char ']'
    | Tarrow (t1, t2) -> print_char '('; trec t1; print_char ')';
        print_string " -> "; trec t2
    | Tvar tv -> 
        begin match tv.def with
            | None -> Printf.printf "Tvar %d" tv.id
            | Some t -> trec t
        end

let p_type t = 
    trec t;
    print_newline ()

let taffiche p =
    let prof = ref 0 in
    let rec p_const c =
        hyphens !prof;
        begin match c with
            | Cbool b -> Printf.printf "Cbool %s" (bool_to_str b);
            | Cint i -> Printf.printf "Cint %d" i
            | Cchar c -> Printf.printf "Cchar '%c'" c
        end;
        print_newline ();
    in
    let rec p_expr {tdesc = exp; typ = t} =
        hyphens !prof;
        incr prof;
        begin match exp with
            | TEvar id -> Printf.printf "TEvar %s : " id; p_type t;
            | TEconst c -> print_string "TEconst : "; p_type t; p_const c;
            | TEapp (e1, e2) -> print_string "TEapp : "; p_type t;
                p_expr e1; p_expr e2
            | TEabs (id, e) -> print_string "TEabs : "; p_type t;
                Printf.printf "%s\n" id; p_expr e
            | TEbinop (b, e1, e2) ->
                Printf.printf "TEbinop %s : " (bin_to_str b);
                p_type t; p_expr e1; p_expr e2
            | TEnil -> print_string "TEnil : "; p_type t
            | TEcond (e1, e2, e3) -> print_string "TEcond : "; p_type t;
                p_expr e1; p_expr e2; p_expr e3
            | TElet (defs, e) -> print_string "TElet : "; p_type t;
                List.iter p_def defs; p_expr e
            | TEcase (l, e1, x, xs, e2) -> print_string "TEcase : "; p_type t;
                p_expr l; p_expr e1; Printf.printf "%s:%s\n" x xs; p_expr e2
            | TEdo es -> print_string "TEdo : "; p_type t; List.iter p_expr es
            | TEreturn -> print_string "TEreturn : "; p_type t
        end;
        decr prof;
    and p_def d =
        hyphens !prof;
        Printf.printf "def: %s\n" d.tname;
        incr prof;
        p_expr d.tbody;
        decr prof;
    in List.iter p_def p.tdefs
