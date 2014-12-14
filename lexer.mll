{
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

    let str_to_cchar = function (* turns an escaped character into the caracter *)
        | "\\\\" -> Ast.Cchar '\\'
        | "\\\"" -> Ast.Cchar '"'
        | "\\n" -> Ast.Cchar '\n'
        | "\\t" -> Ast.Cchar '\t'
        | s when String.length s = 1 -> Ast.Cchar s.[0]
        | _ -> raise (Lexing_error "Invalid character constant")

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
    | '"' { str_lex [] lexbuf }
    | "True" { CONST (Ast.Cbool true) }
    | "False" { CONST (Ast.Cbool false) }
    | ident as s {
        if List.exists (fun x -> fst x = s) kwd_tbl then
            List.assoc s kwd_tbl
        else if lexbuf.lex_start_p.pos_cnum = lexbuf.lex_start_p.pos_bol then
            IDENT0 s
        else
            IDENT1 s }
    | integer as s { CONST (Ast.Cint (int_of_string s)) }
    | _ { raise (Lexing_error "Invalid lexem") }
    | eof { EOF }

and comment = parse (* until the end of the line *)
    | '\n' { newline lexbuf; token lexbuf }
    | _ { comment lexbuf }
    | eof { EOF }

and str_lex l = parse (* l is a string list *)
    | '"' { let char_l = List.map (fun s -> Ast.Econst (str_to_cchar s)) l in
            CONST (Ast.Cstring (Ast.Elist (List.rev char_l))) }
    | car as c { str_lex (c::l) lexbuf }
    | eof { raise (Lexing_error "Unterminated string") }
    | _ { raise (Lexing_error "Invalid character in a string") }

