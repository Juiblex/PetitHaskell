open Ast

exception Unification_failure of typ * typ

exception Conflicting_types of position * typ * typ 

exception Undeclared_variable of pident

exception Multiple_definition of pident * pident

exception No_main

exception Wrong_main_type of typ

exception Redefined_primitive of pident

module Var = struct
    type t = tvar
    let compare v1 v2 = Pervasives.compare v1 v2
    let equal v1 v2 = v1.id = v2.id
    let create = let r = ref 0 in fun () -> incr r; {id = !r; def = None}
end

module Vset = Set.Make(Var)

type schema = {vars: Vset.t; styp: typ}

module Smap = Map.Make(String)

module Vmap = Map.Make(Var)

type env = {bindings: schema Smap.t; fvars: Vset.t}

let empty = {bindings = Smap.empty; fvars = Vset.empty}

let check_multiple pids =
    let add_u m d =
        if Smap.mem d.pid m then
            let d0 = Smap.find d.pid m in
            raise (Multiple_definition (d0, d))
        else
            Smap.add d.pid d m
    in ignore (List.fold_left add_u Smap.empty pids)

let rec head = function
    | Tvar {def = Some t} -> head t
    | t -> t

let rec canon t = match head t with
    | Tlist u -> Tlist (canon u)
    | Tarrow (t1, t2) -> Tarrow(canon t1, canon t2)
    | u -> u

let unification_error t1 t2 = raise (Unification_failure (canon t1, canon t2))

let rec occur tv t = match head t with
    | Tlist u -> occur tv u
    | Tarrow (t1, t2) -> occur tv t1 || occur tv t2
    | Tvar tv1 -> Var.equal tv tv1
    | _ -> false

let rec unify typ1 typ2 = 
    let typ1 = head typ1 and typ2 = head typ2 in
    match typ1, typ2 with
        | Tbool, Tbool | Tchar, Tchar | Tint, Tint | Tio, Tio -> ()
        | Tlist t1, Tlist t2 -> unify t1 t2
        | Tarrow (t, t'), Tarrow (u, u') -> unify t u; unify t' u'
        | Tvar tv1, Tvar tv2 when Var.equal tv1 tv2 -> ()
        | Tvar tv, _ -> if occur tv typ2 then unification_error typ1 typ2
            else tv.def <- Some typ2
        | _, Tvar tv -> unify typ2 typ1
        | _ -> unification_error typ1 typ2

let unify_p t1 t2 p =
    try unify t1 t2
    with Unification_failure (t1, t2) ->
        raise (Conflicting_types (p, t1, t2))

let rec fvars t = match head t with
    | Tlist u -> fvars u
    | Tarrow (t1, t2) -> Vset.union (fvars t1) (fvars t2)
    | Tvar tv -> Vset.singleton tv
    | _ -> Vset.empty

let var_set vars =
    Vset.fold (fun tv s -> Vset.union (fvars (Tvar tv)) s) vars Vset.empty

let add var typ e =
    { bindings = Smap.add var { vars = Vset.empty; styp = typ } e.bindings;
    fvars = Vset.union e.fvars (fvars typ) }

let add_gen var typ e =
    { bindings = Smap.add var { vars = Vset.diff (fvars typ) (var_set e.fvars);
                                styp = typ } e.bindings;
    fvars = e.fvars }
    
let find x env = 
    let sch_x = Smap.find x env.bindings in
    let vars_t = Vset.fold (fun v m -> Vmap.add v (Tvar (Var.create ())) m)
        sch_x.vars Vmap.empty in 
    let rec inst t = match head t with
        | Tlist u -> Tlist (inst u)
        | Tarrow (t1, t2) -> Tarrow (inst t1, inst t2)
        | (Tvar tv) as t1 -> (try Vmap.find tv vars_t with Not_found -> t1)
        | u -> u
    in inst sch_x.styp

let rec w env {pdesc = expr; pos = pos} = match expr with
    | PEvar x -> {tdesc = TEvar x.pid;
        typ = try find x.pid env
        with Not_found -> raise (Undeclared_variable x)}

    | PEconst c -> begin match c with
        | PCbool b -> {tdesc = TEconst (Cbool b); typ = Tbool}
        | PCint i -> {tdesc = TEconst (Cint i); typ = Tint}
        | PCchar c -> {tdesc = TEconst (Cchar c); typ = Tchar}
        | PCstring {pdesc = PElist s} ->
            let add_c t l = match t with 
                | ({pdesc = PEconst (PCchar c)} : Ast.pexpr) ->
                    {tdesc = TEbinop (Bcons, {tdesc = TEconst (Cchar c);
                                             typ = Tchar}, l);
                     typ = Tlist Tchar}
                | _ -> failwith "Invalid character"
            in
            List.fold_right add_c s {tdesc = TEnil; typ = Tlist Tchar}
        | PCstring _ -> failwith "Invalid string" (* never happens *)
        end

    | PEapp (e::es) -> 
        let app te1 e2 =
            let te2 = w env e2 in
            let t = Tvar (Var.create ()) in
            unify_p (Tarrow (te2.typ, t)) te1.typ e2.pos;
            {tdesc = TEapp (te1, te2); typ = t}
        in List.fold_left app (w env e) es 

    | PEabs (pids, exp) ->
        check_multiple pids;
        let rec typ_abs il e = match il with
            | [] -> w e exp
            | id::ids -> 
                let v = Tvar (Var.create ()) in
                let e' = add id.pid v e in
                let func = typ_abs ids e' in
                {tdesc = TEabs (id.pid, func); typ = Tarrow(v, func.typ)}
        in
        typ_abs pids env

    | PEuminus e ->
        let te = w env e in
        unify_p te.typ Tint e.pos;
        {tdesc = TEbinop(Bsub, {tdesc = TEconst (Cint 0); typ = Tint}, te);
         typ = Tint}

    | PEbinop (b, e1, e2) ->
        let te1 = w env e1 and te2 = w env e2 in
        let t = begin match b with
            | Badd | Bsub | Bmul -> unify_p te1.typ Tint e1.pos;
                unify_p te2.typ Tint e2.pos; Tint
            | Blt | Ble | Bgt | Bge | Beq | Bne -> unify_p te1.typ Tint e1.pos;
                unify_p te2.typ Tint e2.pos; Tbool
            | Band | Bor -> unify_p te1.typ Tbool e1.pos;
                unify_p te2.typ Tbool e2.pos; Tbool
            | Bcons ->
                unify_p te2.typ (Tlist te1.typ) e2.pos; (Tlist te1.typ)
        end in
        {tdesc = TEbinop(b, te1, te2); typ = t}

    | PElist [] ->
        let t = Tvar (Var.create ()) in {tdesc = TEnil; typ = Tlist t}
    | PElist (e::es) -> 
        let te = w env e in
        let tes = w env {pdesc = (PElist es); pos = pos} in
        begin match tes.typ with
            | Tlist u -> unify_p te.typ u e.pos;
                {tdesc = TEbinop(Bcons, te, tes); typ = Tlist te.typ}
            | u -> raise (Conflicting_types ((List.hd es).pos, u, Tlist te.typ))
        end

    | PEcond (e1, e2, e3) ->
        let te1 = w env e1 in
        let te2 = w env e2 in
        let te3 = w env e3 in
        unify_p te1.typ Tbool e1.pos;
        unify_p te2.typ te3.typ e2.pos;
        {tdesc = TEcond (te1, te2, te3); typ = te2.typ}

    | PElet (pdefs, e) ->
        let env, tdefs = w_pdef env pdefs in
        let te = w env e in
        {tdesc = TElet(tdefs, te); typ = te.typ}
              
    | PEcase (l, e1, x, xs, e2) ->
        if x.pid = xs.pid then
            raise (Multiple_definition (x, xs))
        else
            let tl = w env l in
            let te1 = w env e1 in
            let v = Tvar (Var.create ()) in
            let env = add x.pid v env in
            let env = add xs.pid (Tlist v) env in
            let te2 = w env e2 in
            unify_p te1.typ te2.typ e1.pos;
            unify_p tl.typ (Tlist v) l.pos;
            {tdesc = TEcase (tl, te1, x.pid, xs.pid, te2); typ = te1.typ}

    | PEdo es ->
        let verif e =
            let te = w env e in
            unify_p te.typ Tio e.pos;
            te
        in
        {tdesc = TEdo (List.map verif es); typ = Tio}

    | PEreturn -> {tdesc = TEreturn; typ = Tio}
    | _ -> failwith "Invalid type"

and w_pdef env pdefs =
    check_multiple (List.map (fun d -> d.pname) pdefs);
    let add_p e d = add d.pname.pid (Tvar (Var.create ())) e in
    let add_t e d = add_gen d.tname d.tbody.typ e in
    let env = List.fold_left add_p env pdefs in
    let tdefs_p = List.map (fun {pname = n; pbody = b} ->
        {tname = n.pid; tbody = w env b}, n.pos) pdefs in
    List.iter (fun (d, pos) ->
            unify_p d.tbody.typ
            (w env {pdesc = PEvar {pid = d.tname; pos = pos}; pos = pos}).typ
            pos)
        tdefs_p;
    let tdefs = List.map (fun (x, y) -> x) tdefs_p in
    let env = List.fold_left add_t env tdefs in
    env, tdefs
    
let type_p prog =
    let e = add "div" (Tarrow (Tint, Tarrow (Tint, Tint))) empty in
    let e = add "rem" (Tarrow (Tint, Tarrow (Tint, Tint))) e in
    let e = add_gen "putChar" (Tarrow (Tvar (Var.create ()), Tio)) e in
    let e = add_gen "error" (Tarrow (Tlist Tchar, Tvar (Var.create ()))) e in
    List.iter (fun prim -> let f d = (d.pname.pid = prim) in
        if (List.exists f prog.pdefs)
        then raise (Redefined_primitive (List.find f prog.pdefs).pname))
        ["div"; "rem"; "putChar"; "error"];
    let env, tdefs = w_pdef e prog.pdefs in 
    try
        let m = List.find (fun d -> d.tname = "main") tdefs in
        begin
            try unify m.tbody.typ Tio with Unification_failure (t, _) ->
            raise (Wrong_main_type t)
        end;
        {tdefs = tdefs}
    with Not_found -> raise No_main
