open Ast

module Vset = Set.Make(String)

module Clos = struct
    type t = string
    let create = let r = ref 0 in fun () -> incr r; Printf.sprintf "clos%d" !r
end

let fundefs = ref []

let set_to_list s = Vset.fold (fun e l -> e::l) s []

let rec conv_e arg bvars = function
    | LEvar x -> if x = arg then CEvar CVarg else CEvar (CVvar x)

    | LEconst c -> CEconst c

    | LEapp (e1, e2) -> CEapp (conv_e arg bvars e1, conv_e arg bvars e2)

    | LEabs (x, e) ->
        let name = Clos.create () in
        let args = set_to_list bvars in
        let args = List.filter (fun s -> s <> "_") args in
        fundefs := (CDletfun (name, args, conv_e x (Vset.add x bvars) e))::(!fundefs);
        CEclos (name, args)

    | LEbinop (b, e1, e2) -> CEbinop (b, conv_e arg bvars e1, conv_e arg bvars e2)

    | LEnil -> CEnil

    | LEcond (e1, e2, e3) ->
        CEcond (conv_e arg bvars e1, conv_e arg bvars e2, conv_e arg bvars e3)

    | LElet (ldefs, e) ->
        let bvars = List.fold_left (fun s d -> Vset.add d.lname s) bvars ldefs in
        let cdefs = List.map (conv_d arg bvars) ldefs in
        CElet (cdefs, conv_e arg bvars e)

    | LEcase (l, e1, x, xs, e2) ->
        let bvars' = Vset.add x (Vset.add xs bvars) in
        CEcase (conv_e arg bvars l, conv_e arg bvars e1, x, xs, conv_e arg bvars' e2)

    | LEdo exprs -> CEdo (List.map (conv_e arg bvars) exprs)

    | LEreturn -> CEreturn

    | LEthunk e -> CEthunk (conv_e arg bvars e)

and conv_d arg bvars ldef = ldef.lname, conv_e arg bvars ldef.lbody

let conv_p p =
    let construct d = 
        let name, expr = conv_d "" Vset.empty d in CDlet (name, expr)
    in
    {cdefs = (List.map construct p.ldefs)@(!fundefs)}
