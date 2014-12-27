{
    open Ast
    open Lexing
    open Parser

    exception Lexing_error of string

    let newline lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <-
            { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

    let kwd_tbl =
        ["if", IF; "then", THEN; "else", ELSE; "let", LET; "in", IN;
         "case", CASE; "of", OF; "do", DO; "return", RETURN]

    let str_to_cchar = function (* turns an escaped character
    into the character itself *)
        | "\\\\" -> Ast.PCchar '\\'
        | "\\\"" -> Ast.PCchar '"'
        | "\\n" -> Ast.PCchar '\n'
        | "\\t" -> Ast.PCchar '\t'
        | s when String.length s = 1 -> Ast.PCchar s.[0]
        | _ -> raise (Lexing_error "Invalid character constant")

    let loc startpos endpos =
    { slin = startpos.pos_lnum; scol = startpos.pos_cnum-startpos.pos_bol;
      elin = endpos.pos_lnum; ecol = endpos.pos_cnum-endpos.pos_bol }

    let begin_pos = ref dummy_pos

}

let space = ' ' | '\t'
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let integer = digit+
let ident = ['a'-'z'] (letter | '_' | ['\''] | digit)*
let car = [' ' '!' '#'-'[' ']'-'~'] | "\\\\" | "\\\"" | "\\n" | "\\t"
                                 (*   '\\'     '"'     '\n'    '\t' *)
rule token = parse
    | space+ { token lexbuf }
    | '\n' { newline lexbuf; token lexbuf }
    | "--" { comment lexbuf }
    | "\\" { ABST }
    | "->" { ARROW }
    | '+' { PLUS }
    | '-' { MINUS }
    | '*' { TIMES }
    | ">=" { GRE }
    | '>' { GRT }
    | "<=" { LEE }
    | '<' { LEST }
    | '=' { EQSIGN }
    | "==" { EQ }
    | "/=" { NEQ }
    | "&&" { AND }
    | "||" { OR }
    | ':' { COLON }
    | '(' { LP }
    | ')' { RP }
    | '[' { LB }
    | ',' { COMMA }
    | ']' { RB }
    | ';' { SEMICOLON }
    | '{' { BEGIN }
    | '}' { END }
    | '\'' (car as c) '\'' { CONST (str_to_cchar c) }
    | '"' { begin_pos := lexbuf.lex_start_p; str_lex [] lexbuf }
    | "True" { CONST (Ast.PCbool true) }
    | "False" { CONST (Ast.PCbool false) }
    | ident as s {
        if List.exists (fun x -> fst x = s) kwd_tbl then
            List.assoc s kwd_tbl
        else if lexbuf.lex_start_p.pos_cnum = lexbuf.lex_start_p.pos_bol then
            IDENT0 { pid = s; pos = loc lexbuf.lex_start_p lexbuf.lex_curr_p }
        else
            IDENT1 { pid = s; pos = loc lexbuf.lex_start_p lexbuf.lex_curr_p } }
    | integer as s { CONST (Ast.PCint (int_of_string s)) }
    | _ { raise (Lexing_error "Invalid lexem") }
    | eof { EOF }

and comment = parse (* until the end of the line *)
    | '\n' { newline lexbuf; token lexbuf }
    | _ { comment lexbuf }
    | eof { EOF }

and str_lex l = parse (* l is a PEConst list *)
    | '"' { CONST (Ast.PCstring(
        { pdesc = Ast.PElist (List.rev l);
            pos = loc !begin_pos lexbuf.lex_curr_p })) }
    | car as c {
        let x = str_to_cchar c in
        let pos = loc lexbuf.lex_start_p lexbuf.lex_curr_p in
        str_lex ({ pdesc = Ast.PEconst x; pos =  pos }::l) lexbuf }
    | eof { raise (Lexing_error "Unterminated string") }
    | _ { raise (Lexing_error "Invalid character in a string") }

