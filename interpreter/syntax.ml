(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Minus | Mult | Lt | AND | OR

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp
  | FunExp of id * exp
  | AppExp of exp * exp
  | LetRecExp of id * id * exp * exp
  | MatchExp of exp * exp * id * id * exp
  | ListExp of exp list
  | InfixOpExp of binOp

type decl =
  | Decl of id * exp
  | RecDecl of id * id * exp

type program = 
    Exp of exp
  | DeclList of decl list 

