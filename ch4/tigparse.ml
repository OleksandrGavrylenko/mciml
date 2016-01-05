open Core.Std
open Lexer
open Lexing
open Absyn

let filename = Sys.argv.(1)

let () =
    let inBuffer = open_in filename in
    let lineBuffer = Lexing.from_channel inBuffer in
    try
        let program = Parser.program Lexer.read lineBuffer in
        Format.printf "%s\n" (show_exp program)
    with
        | Lexer.SyntaxError msg -> Printf.fprintf stderr "%s%!\n" msg
        | Parser.Error -> begin
              let curr = lineBuffer.Lexing.lex_curr_p in
              let lineno = curr.Lexing.pos_lnum in
              let colno = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
              let tok = Lexing.lexeme lineBuffer in
              Printf.fprintf stderr "Syntax error at line %d, col %d, token \"%s\"\n" lineno colno tok
            end
