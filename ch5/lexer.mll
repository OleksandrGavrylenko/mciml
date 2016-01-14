{
open Parser
exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

}

let digit = ['0'-'9']
let int_ = '-'? digit+
let newline = '\r' | '\n' | "\r\n"
let white = [' ' '\t']+
let id = ['a' - 'z' 'A' - 'Z' '_'] ['a' - 'z' 'A' - 'Z' '0' - '9' '_']*

rule read = parse
    | white { read lexbuf }
    | newline { next_line lexbuf; read lexbuf }
    | '"' { read_string (Buffer.create 17) lexbuf }
    | "/*" { read_comment lexbuf; read lexbuf }
    | "type" { TYPE }
    | "var" { VAR }
    | "function" { FUNCTION }
    | "break" { BREAK }
    | "of" { OF }
    | "end" { END }
    | "in" { IN }
    | "nil" { NIL }
    | "let" { LET }
    | "do" { DO }
    | "to" { TO }
    | "for" { FOR }
    | "while" { WHILE }
    | "else" { ELSE }
    | "then" { THEN }
    | "if" { IF }
    | "array" { ARRAY }
    | ":=" { ASSIGN }
    | '|' { OR }
    | '&' { AND }
    | ">=" { GE }
    | '>' { GT }
    | "<=" { LE }
    | '<' { LT }
    | "<>" { NEQ }
    | '=' { EQ }
    | '/' { DIVIDE }
    | '*' { TIMES }
    | '-' { MINUS }
    | '+' { PLUS }
    | '.' { DOT }
    | '{' { RBRACE }
    | '}' { LBRACE }
    | '[' { RBRACK }
    | ']' { LBRACK }
    | '(' { RPAREN }
    | ')' { LPAREN }
    | ';' { SEMICOLON }
    | ':' { COLON }
    | ',' { COMMA }
    | eof { EOF }
    | int_ { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | id { ID (Lexing.lexeme lexbuf)}

and read_string buf = parse
    | '"' { STRING (Buffer.contents buf) }
    | '\\' '/' { Buffer.add_char buf '/'; read_string buf lexbuf }
    | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
    | '\\' 'b' { Buffer.add_char buf '\b'; read_string buf lexbuf }
    | '\\' 'f' { Buffer.add_char buf '\012'; read_string buf lexbuf }
    | '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf }
    | '\\' 'r' { Buffer.add_char buf '\r'; read_string buf lexbuf }
    | '\\' 't' { Buffer.add_char buf '\t'; read_string buf lexbuf }
    | [^ '"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf);
                      read_string buf lexbuf }
    | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
    | eof { raise (SyntaxError ("String is not terminated")) }

and read_comment = parse
    | '*' '/' { }
    | eof { raise (SyntaxError ("Unterminated comment")) }
    | _ {read_comment lexbuf}
