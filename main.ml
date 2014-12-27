open Format
open Lexing
open Typing
open Ast

let parse_only = ref false
let print_past = ref false
let print_tast = ref false

let ifile = ref ""
let ofile = ref ""

let set_file f s = f := s

let options = 
    ["--parse-only", Arg.Set parse_only,
     "  Do only the parsing";
     "--print-past", Arg.Set print_past,
     "  Print the parsing abstract syntax tree";
     "--print-tast", Arg.Set print_tast,
     "  Print the typed abstract syntax tree"]

let usage = "petitghc [options] file.hs"

let loc_p p =
    Printf.printf "File \"%s\", line %d, characters %d-%d:\n" !ifile
        p.slin p.scol p.ecol

let localisation pos = 
    let l = pos.pos_lnum in
    let c = pos.pos_cnum - pos.pos_bol + 1 in
    Printf.printf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

let () = 
    Arg.parse options (set_file ifile) usage;

    if !ifile == "" then begin
        Printf.printf "No file to compile.\n";
        exit 1;
    end;
    
    if not (Filename.check_suffix !ifile ".hs") then begin
        Printf.printf "The file must end in .hs.\n@?";
        Arg.usage options usage;
        exit 1;
    end;

    let f = open_in !ifile in
    let buf = Lexing.from_channel f in
    try
        let p = Parser.prog Lexer.token buf in
        close_in f;
        if !parse_only then exit 0;
        if !print_past then begin
            Print.paffiche p; 
            exit 0;
        end;
        let p = Typing.type_p p in
        if !print_tast then begin
            Print.taffiche p; 
            exit 0;
        end;
    with
        | Lexer.Lexing_error c -> 
            localisation (Lexing.lexeme_start_p buf);
            Printf.printf "Lexing error: %s\n" c;
            exit 1
        | Parser.Error ->
            localisation (Lexing.lexeme_start_p buf);
            Printf.printf "Parsing error\n";
            exit 1
        | Typing.No_main ->
            Printf.printf "The function \"main\" is not declared\n";
            exit 1
        | Typing.Wrong_main_type t ->
            Printf.printf "Typing error : main is of type "; Print.trec t;
            Printf.printf " instead of IO ()\n";
            exit 1
        | Typing.Conflicting_types(p, t1, t2) ->
            loc_p p;
            Printf.printf "This expression has type "; Print.trec t1;
            Printf.printf " but is expected to have type "; Print.p_type t2;
            exit 1
        | Typing.Undeclared_variable {pid = name; pos = p} ->
            loc_p p;
            Printf.printf "Undeclared variable %s\n" name;
            exit 1
        | Typing.Multiple_definition (pid1, pid2) ->
            loc_p pid2.pos;
            Printf.printf "Multiple declaration of %s.\n" pid1.pid;
            Printf.printf "Previous declaration was here: ";
            Printf.printf "line %d, characters %d-%d\n" pid1.pos.slin
              pid1.pos.scol pid1.pos.ecol;
            exit 1

