%{

open Absyn
let sym = Symbol.sym

%}

%start <Absyn.exp> program

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

%nonassoc THEN DO
%nonassoc ELSE
%nonassoc ASSIGN
%left AND OR
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS
%left TIMES DIVIDE
%left UMINUS

%% 

program: e = exp EOF { e }

exp:
    | LET d = decs IN es = expseq END { LetExp (d, SeqExp es, $startpos)  }
    | LET d = decs IN END { LetExp (d, UnitExp, $startpos)  }
    | i = INT { IntExp i }
    | s = STRING { StringExp (s, $startpos) }
    | v = lvalue { VarExp v }
    | s = ID RBRACE rvs = recordvals LBRACE { RecordExp(rvs, sym s, $startpos) }
    | NIL { NilExp }
    | RPAREN LPAREN { UnitExp }
    | RPAREN es = expseq LPAREN { SeqExp es }
    | v = lvalue ASSIGN e = exp { AssignExp (v, e, $startpos) }
    | IF e1 = exp THEN e2 = exp { IfExp (e1, e2, None, $startpos) }
    | IF e1 = exp THEN e2 = exp ELSE e3 = exp { IfExp (e1, e2, Some e3, $startpos) }
    | FOR s = ID ASSIGN e1 = exp TO e2 = exp DO e3 = exp { ForExp (sym s, ref true, e1, e2, e3, $startpos) }  (* XXX *)
    | WHILE e1 = exp DO e2 = exp { WhileExp (e1, e2, $startpos) }
    | BREAK { BreakExp $startpos }
    | MINUS e = exp %prec UMINUS { e }  (* XXX *)
    | s = ID RPAREN zm = zeromanyexp LPAREN { CallExp (sym s, zm, $startpos)  }
    | a = arith { a }
    | c = comparison { c }
    | b = boolean { b }

expseq:
    | e = exp { [(e, $startpos)] }
    | e = exp SEMICOLON es = expseq { (e, $startpos)::es }

decs:
    | { [] }
    | d = dec ds = decs { d::ds }

zeromanyexp:
    | { [] }
    | e = exp me = maybeexp { e::me }

maybeexp:
    | { [] }
    | COMMA e = exp me = maybeexp { e::me }

lvalue:
    | s = ID { SimpleVar(sym s, $startpos) }
    | v = lvalue DOT s = ID { FieldVar(v, sym s, $startpos) }
    | v = lvalue RBRACK e = exp LBRACK { SubscriptVar(v, e, $startpos) }
    
arith:
    | e1 = exp PLUS e2 = exp { OpExp (e1, PlusOp, e2, $startpos) }
    | e1 = exp MINUS e2 = exp { OpExp (e1, MinusOp, e2, $startpos) }
    | e1 = exp TIMES e2 = exp { OpExp (e1, TimesOp, e2, $startpos) }
    | e1 = exp DIVIDE e2 = exp { OpExp (e1, DivideOp, e2, $startpos) }

comparison:
    | e1 = exp LT e2 = exp { OpExp (e1, LtOp, e2, $startpos) }
    | e1 = exp LE e2 = exp { OpExp (e1, LeOp, e2, $startpos) }
    | e1 = exp EQ e2 = exp { OpExp (e1, EqOp, e2, $startpos) }
    | e1 = exp NEQ e2 = exp{ OpExp (e1, NeqOp, e2, $startpos) }
    | e1 = exp GE e2 = exp { OpExp (e1, GeOp, e2, $startpos) }
    | e1 = exp GT e2 = exp { OpExp (e1, GtOp, e2, $startpos) }

boolean:
    | e1 = exp AND e2 = exp { IfExp(e1, e2, Some(IntExp 0), $startpos) }
    | e1 = exp OR e2 = exp { IfExp(e1, IntExp(1), Some e2, $startpos) }

dec:
    | t = tydec { t }
    | v = vardec { v }
    | f = fundec { f }

tydec: TYPE s = ID EQ ty = ty { TypeDec(sym s, ty, $startpos) }

ty:
    | s = ID { NameTy(sym s, $startpos) }
    | RBRACE tf = tyfields LBRACE { RecordTy tf }
    | ARRAY OF s = ID { ArrayTy(sym s, $startpos) }

tyfields:
    | { [] }
    | s1 = ID COLON s2 = ID mtf = moretyfields {
        {name = sym s1; escape = ref false; typ = sym s2; pos = $startpos}::mtf }

moretyfields:
    | { [] }
    | COMMA tf = tyfields { tf }

recordvals:
    | { [] }
    | s = ID EQ e = exp mrv = morerecordvals { (sym s, e, $startpos)::mrv }

morerecordvals:
    | { [] }
    | COMMA rv = recordvals { rv }

(* TODO factor out the assignment parts
 * Can ArrayExp/RecordExp be defined higher up? Yes they can*)
vardec:
    | VAR s = ID tyo = tyopt ASSIGN e = exp {
        VarDec { name = sym s; escape = ref false; typ = tyo;
                 init = e; pos = $startpos }}
    | VAR s = ID tyo = tyopt ASSIGN t = ID RBRACK e1 = exp LBRACK OF e2 = exp {
        VarDec { name = sym s; escape = ref false; typ = tyo;
                 init = ArrayExp (sym t, e1, e2, $startpos) ; pos = $startpos }}

tyopt:
    | COLON ty = ID { Some (sym ty, $startpos) }
    | { None }

fundec:
    | FUNCTION s = ID RPAREN tf = tyfields LPAREN EQ e = exp {
        FunctionDec { name = sym s; params = tf; result = None; body = e; pos = $startpos} }
    | FUNCTION s = ID RPAREN tf = tyfields LPAREN COLON s2 = ID EQ e = exp {
        FunctionDec { name = sym s; params = tf; result = Some(sym s2, $startpos); body = e; pos = $startpos} }
