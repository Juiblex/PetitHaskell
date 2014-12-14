open Format
open Lexing

let parse_only = ref false
let print_ast = ref false

let ifile = ref ""
let ofile = ref ""

let set_file f s = f := s

let options = 
    ["--parse-only", Arg.Set parse_only,
     "  Do only the parsing";
     "--print-ast", Arg.Set print_ast,
     "  Print the abstract syntax tree"]

let usage = "petitghc [options] file.hs"

let localisation pos = 
    let l = pos.pos_lnum in
    let c = pos.pos_cnum - pos.pos_bol + 1 in
    eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

let () = 
    Arg.parse options (set_file ifile) usage;

    if !ifile == "" then begin
        eprintf "No file to compile.\n";
        exit 1;
    end;
    
    if not (Filename.check_suffix !ifile ".hs") then begin
        eprintf "The file must end in .hs.\n@?";
        Arg.usage options usage;
        exit 1;
    end;

    let f = open_in !ifile in
    let buf = Lexing.from_channel f in
    try
        let p = Parser.prog Lexer.token buf in
        close_in f;
        if !parse_only then exit 0;
        if !print_ast then begin
            Print.affiche p;
            exit 0;
        end;
    with
        | Lexer.Lexing_error c -> 
            localisation (Lexing.lexeme_start_p buf);
            eprintf "Lexing error: %s@." c;
            exit 1
        | Parser.Error ->
            localisation (Lexing.lexeme_start_p buf);
            eprintf "Parsing error@.";
            exit 1
