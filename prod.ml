open Ast
open Mips

module Label = struct
    type t = string
    let create = let r = ref 0 in fun () -> incr r; Printf.sprintf "_lab%d" !r
end

let pushn n = sub sp sp oi n

let alloc_heap n =  (* allocates n *words* *)
    push a0 ++
    li v0 9 ++
    li a0 (4*n) ++
    syscall ++
    pop a0

let force = 
    label "force" ++
    lw t0 areg (0, a0) ++
    li t1 0x10 ++
    bgt t0 t1 "force_1" ++
    jr ra ++

    label "force_1" ++
    li t1 0x20 ++
    beq t0 t1 "force_2" ++
    lw a0 areg (4, a0) ++
    jr ra ++

    label "force_2" ++
    push ra ++
    push a0 ++
    push a1 ++
    lw a0 areg (4, a0) ++ (* $a0 : closure address *)
    move a1 a0 ++
    lw a0 areg (4, a0) ++ (* $a0 : closure code address *)
    jalr a0 ++
    move a0 v0 ++
    jal "force" ++
    move t0 a0 ++
    pop a1 ++
    pop a0 ++
    li t1 0x40 ++
    sw t1 areg (0, a0) ++
    sw t0 areg (4, a0) ++
    move a0 t0 ++ (* $a0 : value address *)
    pop ra ++
    jr ra

let putChar_c =
    label "_putChar" ++
    push ra ++
    push a0 ++
    push v0 ++
    jal "force" ++
    lw a0 areg (4, a0) ++
    add a0 a0 oi 48 ++
    li v0 11 ++
    syscall ++
    pop v0 ++
    pop a0 ++
    pop ra ++
    jr ra

let putChar_v =
    comment "début de putChar" ++
    alloc_heap 2 ++
    li t0 0x10 ++
    sw t0 areg (0, v0) ++
    la t0 alab "_putChar" ++
    sw t0 areg (4, v0) ++
    la t0 alab "putChar" ++
    sw v0 areg (0, t0) ++
    comment "fin de putChar"

let alloc_prim typ v = (* for ints, bools and chars *)
    alloc_heap 2 ++
    li t0 typ ++
    sw t0 areg (0, v0) ++
    if v = 0 then
        sw zero areg (4, v0)
    else
        li t0 v ++
        sw t0 areg (4, v0)

let rec compile_e = function
    | VEvar v -> begin match v with
        | VVglob id ->
            la v0 alab id ++
            lw v0 areg (0, v0)
        | VVloc n ->
            lw v0 areg (n, fp)
        | VVclos n ->
            lw v0 areg (n, a1)
        | VVarg ->
            move v0 a0
        end

    | VEconst c -> begin match c with
        | Cint n ->
            alloc_prim 0 n
        | Cchar c ->
            alloc_prim 1 (int_of_char c)
        | Cbool b ->
            alloc_prim 2 (if b then 1 else 0)
        end

    | VEapp (e1, e2) ->
        comment "début app" ++
        compile_f e1 t0 ++
        push a0 ++
        push a1 ++
        push t0 ++
        compile_e e2 ++
        move a0 v0 ++
        pop a1 ++ (* closure address *)
        lw t0 areg (4, a1) ++
        jalr t0 ++
        pop a1 ++
        pop a0 ++
        comment "fin app"

    | VEclos (name, vars) ->
        let pre =
            comment ("début de clôture " ^ name) ++
            alloc_heap (2 + List.length vars) ++
            li t0 0x10 ++
            sw t0 areg (0, v0) ++
            la t0 alab name ++
            sw t0 areg (4, v0)
        in
        let code, _ = List.fold_left (fun (code, n) v ->
            code ++
            comment "début variable clôture" ++
            push v0 ++
            compile_e (VEvar v) ++
            move t0 v0 ++
            pop v0 ++
            sw t0 areg (n, v0) ++
            comment "fin variable clôture",
            n+4) (nop, 8) vars
        in
        let post =
            comment ("fin de clôture " ^ name)
        in pre ++ code ++ post

    | VEbinop (Bcons, e1, e2) -> failwith "TODO"

    | VEbinop (Band, e1, e2) ->
        let fail = Label.create () in
        let ret = Label.create () in
        comment "début and" ++
        compile_f e1 t0 ++
        lw t0 areg (4, t0) ++
        beqz t0 fail ++
        compile_f e2 t0 ++
        lw t0 areg (4, t0) ++
        beqz t0 fail ++
        alloc_prim 1 1 ++
        j ret ++
        label fail ++
        alloc_prim 1 0 ++
        label ret ++
        comment "fin and"

    | VEbinop (Bor, e1, e2) ->
        let succ = Label.create () in
        let ret = Label.create () in
        comment "début or" ++
        compile_f e1 t0 ++
        lw t0 areg (4, t0) ++
        bnez t0 succ ++
        compile_f e2 t0 ++
        lw t0 areg (4, t0) ++
        bnez t0 succ ++
        alloc_prim 1 0 ++
        j ret ++
        label succ ++
        alloc_prim 1 1 ++
        label ret ++
        comment "fin or"

    | VEbinop(b, e1, e2) ->
        let pre typ = 
            comment "début binop" ++
            compile_f e1 t0 ++
            push t0 ++
            compile_f e2 t2 ++
            pop t1 ++
            alloc_heap 2 ++
            li t0 typ ++
            sw t0 areg (0, v0) ++
            lw t1 areg (4, t1) ++
            lw t2 areg (4, t2) 
        in
        let mid, typ = match b with
            | Badd -> add t0 t1 oreg t2, 0
            | Bsub -> sub t0 t1 oreg t2, 0
            | Bmul -> mul t0 t1 oreg t2, 0
            | Blt -> slt t0 t1 t2, 1
            | Ble -> sle t0 t1 t2, 1
            | Bgt -> sgt t0 t1 t2, 1
            | Bge -> sge t0 t1 t2, 1
            | Beq -> seq t0 t1 t2, 1
            | Bne -> sne t0 t1 t2, 1
            | _ -> failwith "never happens"
        in
        pre typ ++
        mid ++
        sw t0 areg (4, v0) ++
        comment "fin binop"

    | VEnil ->
        alloc_heap 1 ++
        li t0 8 ++
        sw t0 areg (0, v0)

    | VEcond (e1, e2, e3) ->
        let fail = Label.create () in
        let ret = Label.create () in
        compile_f e1 t0 ++
        lw t0 areg (4, t0) ++
        beqz t0 fail ++
        compile_e e2 ++
        j ret ++
        label fail ++
        compile_e e3 ++
        label ret   

    | VElet (foo, bar) -> failwith "TODO"

    | VEcase (a, b, c, d, e) -> failwith "TODO"

    | VEdo exprs ->
        let force_e e code =
            compile_f e t0 ++
            code
        in
        comment "début do" ++
        List.fold_right force_e exprs nop ++
        comment "fin do"

    | VEreturn -> nop

    | VEthunk e ->
        comment "début glaçon" ++
        compile_e e ++
        push v0 ++
        alloc_heap 2 ++
        li t0 0x20 ++
        sw t0 areg (0, v0) ++
        pop t0 ++
        sw t0 areg (4, v0) ++
        comment "fin glaçon"

and compile_f e r = (* puts the forced result of e in r *)
    compile_e e ++
    push a0 ++
    move a0 v0 ++
    jal "force" ++
    move r a0 ++
    pop a0

let compile_d (codevars, codefuns, globs) = function
    | VDlet (x, e, fpmax) ->
        let codex =
            comment ("début de " ^ x) ++
            (if fpmax > 0 then pushn fpmax else nop) ++ 
            compile_e e ++
            sw v0 alab x ++
            (if fpmax > 0 then popn fpmax else nop) ++
            comment ("fin de " ^ x)
        in codevars ++ codex, codefuns, globs ++ label x ++ dword [0]
    | VDletfun (f, e, fpmax) ->
        let codef = 
            label f ++
            push fp ++
            push ra ++
            push a0 ++
            push a1 ++
            move fp sp ++
            (if fpmax > 0 then pushn fpmax else nop) ++
            compile_e e ++
            (if fpmax > 0 then popn fpmax else nop) ++
            pop a1 ++
            pop a0 ++
            pop ra ++
            pop fp ++
            jr ra
        in codevars, codefuns ++ codef, globs

let compile_p {vdefs = vdefs} =
    let codevars, codefuns, globs = List.fold_left compile_d (nop, nop, nop) vdefs in
    {text =
        codevars ++
        putChar_v ++
        la t0 alab "main" ++
        lw a0 areg (0, t0) ++
        jal "force" ++ 
        li v0 10 ++
        syscall ++
        codefuns ++
        putChar_c ++
        force;
    data =
        globs ++
        label "putChar" ++
        dword [0]
        }
