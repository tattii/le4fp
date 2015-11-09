%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT AND OR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    Expr SEMISEMI { Exp $1 }
  | LET ID EQ Expr SEMISEMI { Decl ($2, $4) }

Expr :
    IfExpr  { $1 }
  | LetExpr { $1 }
  | LTExpr  { $1 }
  | LogExpr { $1 }

LTExpr : 
    PExpr LT PExpr { BinOp (Lt, $1, $3) }
  | PExpr { $1 }

LogExpr : 
    PExpr AND PExpr { BinOp (AND, $1, $3) } 
  | PExpr OR  PExpr { BinOp (OR,  $1, $3) } 
  | PExpr { $1 }

PExpr :
    PExpr PLUS MExpr { BinOp (Plus, $1, $3) }
  | MExpr { $1 }

MExpr : 
    MExpr MULT AExpr { BinOp (Mult, $1, $3) }
  | AExpr { $1 }

AExpr :
    INTV { ILit $1 }
  | TRUE { BLit true }
  | FALSE { BLit false }
  | ID { Var $1 }
  | LPAREN Expr RPAREN { $2 }

LetExpr:
    LET ID EQ Expr IN Expr { LetExp ($2, $4, $6) }


IfExpr :
    IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }

   
