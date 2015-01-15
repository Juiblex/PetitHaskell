open Ast
open Mips

let pushn n = sub sp sp oi n

let alloc_heap n = li v0 9 ++ li a0 (4*n) ++ syscall (* allocates n *words* *)

let force = 
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
    lw a0 areg (4, a0) ++ (* $a0 : closure address *)
    move a1 a0 ++
    lw a0 areg (4, a0) ++ (* $a0 : closure code address *)
    jalr a0 ++
    move a0 v0 ++
    jal "force" ++
    move t0 a0 ++
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
    add a0 a0 oi 48 ++
    li v0 11 ++
    syscall ++
    pop v0 ++
    pop a0 ++
    pop ra ++
    jr ra

let putChar_v =
    alloc_heap 2 ++
    li t0 16 ++
    sw t0 areg (0, v0) ++
    la t0 alab "_putChar" ++
    sw t0 areg (4, v0) ++
    la t0 alab "putChar" ++
    sw v0 areg (0, t0)

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
            alloc_heap 2 ++
            li t0 0 ++
            sw t0 areg (0, v0) ++
            li t0 n ++
            sw t0 areg (4, v0) (* the block address is already in $v0 *)
        | Cchar c ->
            alloc_heap 2 ++
            li t0 2 ++
            sw t0 areg (0, v0) ++
            li t0 (int_of_char c) ++
            sw t0 areg (4, v0)
        | Cbool b ->
            alloc_heap 2 ++
            li t0 1 ++
            sw t0 areg (0, v0) ++
            li t0 (if b then 1 else 0) ++
            sw t0 areg (4, v0)
        end

    | VEapp (e1, e2) ->
        compile_e e1 ++
        move a0 v0 ++
        jal "force" ++
        push a0 ++
        compile_e e2 ++
        pop t0 ++
        lw t1 areg (4, t0) ++ (* address of the code to be executed *)
        move a0 v0 ++ (* argument *)
        move a1 t0 ++ (* bound variables *)
        jalr t1

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

    | VEbinop (Bcons, e1, e2) -> failwith "TODO"

    | VEbinop ((Band | Bor), e1, e2) -> failwith "TODO"

    | VEbinop(b, e1, e2) ->
        let pre typ = 
            compile_e e1 ++
            push v0 ++
            compile_e e2 ++
            push v0 ++
            alloc_heap 2 ++
            pop t2 ++
            pop t1 ++
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
        sw t0 areg (4, v0)

    | VEnil ->
        alloc_heap 1 ++
        li t0 8 ++
        sw t0 areg (0, v0)

    | VEdo exprs ->
        let force_e e code =
            compile_e e ++
            move a0 v0 ++
            jal "force" ++
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
        li t0 32 ++
        sw t0 areg (0, v0) ++
        pop a0 ++
        sw a0 areg (4, v0) ++
        comment "fin glaçon"

    | _ -> failwith "TODO"

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
