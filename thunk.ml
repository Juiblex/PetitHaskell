open Ast

(* In this pass we lazify the program and prefix idents with '_' so that there
 * is no conflict with the assembly code *)

let pref_ = Printf.sprintf "_%s"

let freeze e = LEthunk (LEabs ("_", e))

let rec lazify_e {tdesc = e} = match e with
    | TEvar id -> LEvar (pref_ id)

    | TEconst c -> LEconst c 

    | TEapp (e1, e2) -> LEapp (lazify_e e1, freeze (lazify_e e2))

    | TEabs (x, e) -> LEabs (x, lazify_e e)

    | TEbinop (b, e1, e2) -> LEbinop (b, lazify_e e1, lazify_e e2)

    | TEnil -> LEnil

    | TEcond (e1, e2, e3) ->
        LEcond (lazify_e e1, freeze (lazify_e e2), freeze (lazify_e e3))
    
    | TElet (tdefs, e) -> LElet (List.map lazify_d tdefs, lazify_e e)

    | TEcase (l, e1, x, xs, e2) ->
        LEcase (lazify_e l, freeze (lazify_e e1), pref_ x, pref_ xs,
            freeze (lazify_e e2))

    | TEdo exprs -> LEdo (List.map lazify_e exprs)

    | TEreturn -> LEreturn

and lazify_d {tname = id; tbody = e} = {lname = pref_ id; lbody = freeze (lazify_e e)}

let lazify_p p = {ldefs = List.map lazify_d p.tdefs}
