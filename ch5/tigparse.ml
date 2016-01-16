open Core.Std
open Lexer
open Lexing
open Absyn
open Types

let parse lexbuf =
    try
        Parser.program Lexer.read lexbuf
    with
        | Lexer.SyntaxError msg -> Printf.fprintf stderr "%s%!\n" msg; exit 1
        | Parser.Error -> begin
              let curr = lexbuf.Lexing.lex_curr_p in
              let lineno = curr.Lexing.pos_lnum in
              let colno = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
              let tok = Lexing.lexeme lexbuf in
              Printf.fprintf stderr "Syntax error at line %d, col %d, token \"%s\"\n" lineno colno tok; exit 1
            end

let parse_file fname = parse (Lexing.from_channel (open_in fname))
let parse_string s = parse (Lexing.from_string s)
let print_ast ast = Format.printf "%s\n" (show_exp ast)
