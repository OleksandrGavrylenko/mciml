%start <unit> program

%token TYPE VAR FUNCTION END BREAK
%token OF IN NIL LET TO DOT
%token IF THEN ELSE FOR WHILE DO ARRAY ASSIGN
%token OR AND
%token GE GT LE LT NEQ EQ
%token PLUS MINUS DIVIDE TIMES
%token RBRACE LBRACE RBRACK LBRACK RPAREN LPAREN SEMICOLON COLON COMMA
%token <string> STRING
%token <int> INT
%token <string> ID
%token EOF

%right THEN ELSE DO
%nonassoc ASSIGN
%left AND OR
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS
%left TIMES DIVIDE
%left UMINUS

%% 

program: exp EOF { () }

exp:
    | LET decs IN expseq END { () }
    | INT { () }
    | STRING { () }
    | lvalue { () }
    | NIL { () }
    | RPAREN LPAREN { () }
    | RPAREN expseq LPAREN { () }
    | lvalue ASSIGN exp { () }
    | IF exp THEN exp { () }
    | IF exp THEN exp ELSE exp { () }
    | FOR ID ASSIGN exp TO exp DO exp { () }
    | WHILE exp DO exp { () }
    | BREAK
    | MINUS exp %prec UMINUS { () }
    | ID RPAREN LPAREN { () }
    | ID RPAREN exp maybeexp LPAREN { () }
    | arith { () }
    | comparison { () }
    | boolean { () }

expseq:
    | exp { () }
    | exp SEMICOLON expseq { () }


decs:
    | { () }
    | dec decs { () }

maybeexp:
    | { () }
    | COMMA exp { () }

lvalue:
    | ID { () }
    | lvalue DOT ID { () }
    | lvalue RBRACK exp LBRACK { () }


arith:
    | exp PLUS exp { () }
    | exp MINUS exp { () }
    | exp TIMES exp { () }
    | exp DIVIDE exp { () }

comparison:
    | exp LT exp { () }
    | exp LE exp { () }
    | exp EQ exp { () }
    | exp NEQ exp { () }
    | exp GE exp { () }
    | exp GT exp { () }

boolean:
    | exp AND exp { () }
    | exp OR exp { () }

dec:
    | tydec { () }
    | vardec { () }
    | fundec { () }

tydec: TYPE ID EQ ty { () }

ty:
    | ID
    | RBRACE tyfields LBRACE { () }
    | ARRAY OF ID { () }

tyfields:
    | { () }
    | ID COLON ID moretyfields { () }

moretyfields:
    | { () }
    | COMMA tyfields { () }

vardec:
    | VAR ID ASSIGN exp { () }
    | VAR ID ASSIGN COLON EQ exp { () }
    | VAR ID ASSIGN ID RBRACK exp LBRACK OF exp { () }

fundec:
    | FUNCTION ID RPAREN tyfields LPAREN EQ exp { () }
    | FUNCTION ID RPAREN tyfields LPAREN COLON ID EQ exp { () }
