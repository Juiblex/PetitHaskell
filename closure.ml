open Ast

module Vset = Set.Make(String)

module Fvar = struct (* no collision here since we prefixed everything with '_' *)
    type t = string
    let create = let r = ref 0 in fun () -> incr r; Printf.sprintf "clos%d" !r
end

let fundefs = ref []

let set_to_list s = Vset.fold (fun e l -> e::l) s []

let rec conv_e arg fvars = function
    | LEvar x -> if x = arg then CEvar CVarg else CEvar (CVvar x)

    | LEconst c -> CEconst c

    | LEapp (e1, e2) -> CEapp (conv_e arg fvars e1, conv_e arg fvars e2)

    | LEabs (x, e) ->
        let name = Fvar.create () in
        let args = set_to_list fvars in
        fundefs := (CDletfun (name, args, conv_e x (Vset.add x fvars) e))::(!fundefs);
        CEclos (x, args)

    | LEbinop (b, e1, e2) -> CEbinop (b, conv_e arg fvars e1, conv_e arg fvars e2)

    | LEnil -> CEnil

    | LEcond (e1, e2, e3) ->
        CEcond (conv_e arg fvars e1, conv_e arg fvars e2, conv_e arg fvars e3)

    | LElet (ldefs, e) ->
        let fvars = List.fold_left (fun s d -> Vset.add d.lname s) fvars ldefs in
        let cdefs = List.map (conv_d arg fvars) ldefs in
        CElet (cdefs, conv_e arg fvars e)

    | LEcase (l, e1, x, xs, e2) ->
        let fvars' = Vset.add x (Vset.add xs fvars) in
        CEcase (conv_e arg fvars l, conv_e arg fvars e1, x, xs, conv_e arg fvars' e2)

    | LEdo exprs -> CEdo (List.map (conv_e arg fvars) exprs)

    | LEreturn -> CEreturn

    | LEthunk e -> CEthunk (conv_e arg fvars e)

and conv_d arg fvars ldef = ldef.lname, conv_e arg fvars ldef.lbody

let conv_p p =
    let fvars = List.fold_left (fun s d -> Vset.add d.lname s) Vset.empty p.ldefs in
    let construct d = 
        let name, expr = conv_d "" fvars d in CDlet (name, expr)
    in
    {cdefs = (List.map construct p.ldefs)@(!fundefs)}
