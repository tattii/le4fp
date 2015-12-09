%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MINUS MULT LT AND OR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ
%token RARROW FUN REC
%token MATCH WITH EMPTYLIST BAR CONS
%token LBRACKET RBRACKET SEMI

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    Expr SEMISEMI { Exp $1 }
  | LetDeclList SEMISEMI { DeclList $1 }

LetDeclList :
    LetDecl { [$1] }
  | LetDeclList LetDecl { $1 @ [$2] }

LetDecl :
    LET ID EQ Expr { Decl ($2, $4) }
  | LET REC ID EQ FUN ID RARROW Expr { RecDecl ($3, $6, $8) }

Expr :
    IfExpr     { $1 }
  | LetExpr    { $1 }
  | LetRecExpr { $1 }
  | LTExpr     { $1 }
  | LogExpr    { $1 }
  | FunExpr    { $1 }
  | AppExpr    { $1 }
  | MatchExpr  { $1 }
  | ListExpr   { $1 }

LTExpr : 
    PExpr LT PExpr { BinOp (Lt, $1, $3) }
  | PExpr { $1 }

LogExpr : 
    PExpr AND PExpr { BinOp (AND, $1, $3) } 
  | PExpr OR  PExpr { BinOp (OR,  $1, $3) } 
  | PExpr { $1 }

PExpr :
    PExpr PLUS  MExpr { BinOp (Plus, $1, $3) }
  | PExpr MINUS MExpr { BinOp (Minus, $1, $3) }
  | MExpr { $1 }

MExpr : 
    MExpr MULT AppExpr { BinOp (Mult, $1, $3) }
  | AExpr { $1 }

AppExpr :
    AppExpr AExpr { AppExp ($1, $2) }
  | AExpr { $1 }

AExpr :
    INTV { ILit $1 }
  | TRUE { BLit true }
  | FALSE { BLit false }
  | ID { Var $1 }
  | LPAREN Expr RPAREN { $2 }
  | LPAREN InfixOp RPAREN { InfixOpExp ($2) }

LetExpr :
    LET ID EQ Expr IN Expr { LetExp ($2, $4, $6) }

LetRecExpr :
    LET REC ID EQ FUN ID RARROW Expr IN Expr { LetRecExp ($3, $6, $8, $10) }

IfExpr :
    IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }

FunExpr :
    FUN ID RARROW Expr { FunExp ($2, $4) } 

MatchExpr :
    MATCH Expr WITH EMPTYLIST RARROW Expr BAR ID CONS ID RARROW Expr { MatchExp ($2, $6, $8, $10, $12) }

ListExpr :
    EMPTYLIST { ListExp ([]) }
  | LBRACKET ListElems RBRACKET { ListExp ($2) }

ListElems :
    Expr { [$1] }
  | ListElems SEMI Expr { $1 @ [$3] }

InfixOp :
    PLUS { Plus }
  | MINUS { Minus }


