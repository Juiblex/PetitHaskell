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

let force_c = 
    label "force" ++
    lw t0 areg (0, a0) ++
    li t1 16 ++
    bgt t0 t1 "force_1" ++
    jr ra ++

    label "force_1" ++
    li t1 32 ++
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
    li t1 64 ++
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
    li v0 11 ++
    syscall ++
    pop v0 ++
    pop a0 ++
    pop ra ++
    jr ra

let putChar_v =
    comment "début de putChar" ++
    alloc_heap 2 ++
    li t0 16 ++
    sw t0 areg (0, v0) ++
    la t0 alab "_putChar" ++
    sw t0 areg (4, v0) ++
    la t0 alab "putChar" ++
    sw v0 areg (0, t0) ++
    comment "fin de putChar"

let error_m =
    label "error_m" ++
    asciiz "error: "

let error_c =
    let loop = Label.create () in
    let ret = Label.create () in
    label "_error" ++
    move s0 a0 ++
    la a0 alab "error_m" ++
    li v0 4 ++
    syscall ++
    move a0 s0 ++
    label loop ++
    jal "force" ++
    lw t0 areg (0, a0) ++
    li t1 8 ++
    beq t0 t1 ret ++
    lw t0 areg (8, a0) ++
    push t0 ++
    lw a0 areg (4, a0) ++
    jal "_putChar" ++
    pop a0 ++
    j loop ++
    label ret ++
    li a0 1 ++
    li v0 17 ++
    syscall

let error_v =
    comment "début de error" ++
    alloc_heap 2 ++
    li t0 16 ++
    sw t0 areg (0, v0) ++
    la t0 alab "_error" ++
    sw t0 areg (4, v0) ++
    la t0 alab "error" ++
    sw v0 areg (0, t0) ++
    comment "fin de error"

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
        comp_force e1 t0 ++
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
            li t0 16 ++
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

    | VEbinop (Bcons, e1, e2) ->
        compile_e e1 ++
        push v0 ++
        compile_e e2 ++
        push v0 ++
        alloc_heap 3 ++
        li t0 4 ++
        sw t0 areg (0, v0) ++
        pop t0 ++
        sw t0 areg (8, v0) ++
        pop t0 ++
        sw t0 areg (4, v0)

    | VEbinop (Band, e1, e2) ->
        let fail = Label.create () in
        let ret = Label.create () in
        comment "début and" ++
        comp_force e1 t0 ++
        lw t0 areg (4, t0) ++
        beqz t0 fail ++
        comp_force e2 t0 ++
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
        comp_force e1 t0 ++
        lw t0 areg (4, t0) ++
        bnez t0 succ ++
        comp_force e2 t0 ++
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
            comp_force e1 t0 ++
            push t0 ++
            comp_force e2 t2 ++
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
        comp_force e1 t0 ++
        lw t0 areg (4, t0) ++
        beqz t0 fail ++
        compile_e e2 ++
        j ret ++
        label fail ++
        compile_e e3 ++
        label ret   

    | VElet (foo, bar) -> failwith "TODO"

    | VEcase (l, e1, x, xs, e2) ->
        let not_empty = Label.create () in
        let ret = Label.create () in
        comment "début case" ++
        comp_force l v0 ++
        lw t0 areg (0, v0) ++
        li t1 4 ++
        beq t0 t1 not_empty ++
        compile_e e1 ++
        j ret ++
        label not_empty ++
        lw t0 areg (4, v0) ++
        sw t0 areg (x, fp) ++
        lw t0 areg (8, v0) ++
        sw t0 areg (xs, fp) ++
        compile_e e2 ++
        label ret ++
        comment "fin case"

    | VEdo exprs ->
        let force_e e code =
            comp_force e t0 ++
            code
        in
        comment "début do" ++
        List.fold_right force_e exprs nop ++
        comment "fin do"

    | VEreturn ->
        alloc_heap 1 ++
        li t0 0 ++
        sw t0 areg (0, v0)

    | VEthunk e ->
        comment "début glaçon" ++
        compile_e e ++
        push v0 ++
        alloc_heap 2 ++
        li t0 32 ++
        sw t0 areg (0, v0) ++
        pop t0 ++
        sw t0 areg (4, v0) ++
        comment "fin glaçon"

and comp_force e r = (* puts the forced result of e in r *)
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
        error_v ++
        la t0 alab "main" ++
        lw a0 areg (0, t0) ++
        jal "force" ++ 
        li v0 10 ++
        syscall ++
        codefuns ++
        putChar_c ++
        error_c ++
        force_c;
    data =
        globs ++
        label "putChar" ++
        dword [0] ++
        label "error" ++
        dword [0] ++
        error_m
        }
