open Core.Std
open Lexer
open Lexing

let filename = Sys.argv.(1)

let () =
    let inBuffer = open_in filename in
    let lineBuffer = Lexing.from_channel inBuffer in
    try
        Parser.program Lexer.read lineBuffer; print_endline "Success!"
    with
        | Lexer.SyntaxError msg -> Printf.fprintf stderr "%s%!\n" msg
        | Parser.Error -> Printf.fprintf stderr "Parse error at offset %d: syntax error.\n%!" (Lexing.lexeme_start lineBuffer)
