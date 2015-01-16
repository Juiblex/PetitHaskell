open Ast

let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

module Smap = Map.Make(String)

let find env clos arg x =
    if x = arg then VVarg
    else if Smap.mem x env then VVloc (Smap.find x env)
    else if Smap.mem x clos then VVclos (Smap.find x clos)
    else VVglob x

let rec alloc_e env clos arg next = function
    | CEvar v ->
        VEvar (find env clos arg v), next

    | CEconst c -> VEconst c, next

    | CEapp (e1, e2) ->
        let ne1, n1 = alloc_e env clos arg next e1 in
        let ne2, n2 = alloc_e env clos arg next e2 in
        VEapp (ne1, ne2), max n1 n2
    
    | CEclos (id, args) ->
        VEclos (id, List.map (fun a -> find env clos arg a) args), next

    | CEbinop (b, e1, e2) ->
        let ne1, n1 = alloc_e env clos arg next e1 in
        let ne2, n2 = alloc_e env clos arg next e2 in
        VEbinop (b, ne1, ne2), max n1 n2

    | CEnil -> VEnil, next

    | CEcond (e1, e2, e3) ->
        let ne1, n1 = alloc_e env clos arg next e1 in
        let ne2, n2 = alloc_e env clos arg next e2 in
        let ne3, n3 = alloc_e env clos arg next e3 in
        VEcond (ne1, ne2, ne3), max (max n1 n2) n3

    | CElet (defs, e) ->
        let rec alloc_d env next = function
            | [] -> [], env
            | (x, _)::ds ->
                let ns, env = alloc_d (Smap.add x (-next-4) env) (next+4) ds in
                (-next-4)::ns, env
        in
        let posl, env = alloc_d env next defs in
        let next = next + 4 * (List.length defs) in
        let exprs, fps = List.split
            (List.map (fun (_, e) -> alloc_e env clos arg next e) defs) in
        let ne, n = alloc_e env clos arg next e in
        VElet (List.combine posl exprs, ne), max n (List.fold_left max 0 fps) 

    | CEcase (l, e1, x, xs, e2) ->
        let nl, n = alloc_e env clos arg next l in
        let ne1, n1 = alloc_e env clos arg next e1 in
        let env = Smap.add x (-(next+4)) env in
        let env = Smap.add xs (-(next+8)) env in
        let ne2, n2 = alloc_e env clos arg (next+8) e2 in
        VEcase (nl, ne1, -(next+4), -(next+8), ne2), max (max n n1) n2

    | CEdo l ->
        let exprs, ns = List.split (List.map (alloc_e env clos arg next) l) in
        let n = List.fold_left max next ns in
        VEdo exprs, n

    | CEreturn -> VEreturn, next
    
    | CEthunk e ->
        let ne, n = alloc_e env clos arg next e in
        VEthunk ne, n

let alloc_d = function
    | CDlet (id, e) -> 
        Hashtbl.replace genv id ();
        let ne, fpmax = alloc_e Smap.empty Smap.empty "" 0 e in
        VDlet (id, ne, fpmax)

    | CDletfun (f, x, closl, e) ->
        let rec pos cour = function
            | [] -> Smap.empty
            | h::t -> Smap.add h cour (pos (cour+4) t)
        in  
        Hashtbl.replace genv f ();
        let ne, fpmax = alloc_e Smap.empty (pos 8 closl) x 0 e in
        VDletfun (f, ne, fpmax) 

let alloc_p {cdefs = cdefs} = {vdefs = List.map alloc_d cdefs}
