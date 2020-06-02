{
module Parser where

import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  draw      { DrawT _ }
  apply     { ApplyT _ }
  ':='      { AssignmentT _ }
  ';'       { SemiColonT _ }
  '+'       { OverlayT _ }
  '*'       { ConnectT _ }
  '('       { ParenOpenT _ }
  ')'       { ParenCloseT _ }
  empty     { EmptyGT _ }
  name      { NameT _ $$ }
  filePath  { PathT _ $$ }

%nonassoc ':='
%right ';'
%left '+'
%left '*'

%%

Exp :: { Comm }
Exp : name ':=' Graph_                  { DefVar $1 $3 }
    | draw Graph_                       { Draw $2 }
    | apply filePath Graph_ name name   { Apply $2 $3 $4 $5 }
    | Exp ';' Exp                       { Seq $1 $3 }

Graph_ :: { Graph }
Graph_ : empty              { Empty }
       | name               { Vertex $1 }
       | Graph_ '+' Graph_  { Overlay $1 $3 }
       | Graph_ '*' Graph_  { Connect $1 $3 }
       | '(' Graph_ ')'     { ParenGraph $2 }

{
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error. Maybe at the end of the file?"
parseError (t : _) = error $ "Parse error " ++ (show . token_posn) t ++ "."


type Name = String

data Comm = DefVar Name Graph
          | Draw Graph
          | Apply Name Graph Name Name
          | Seq Comm Comm

data Graph = Empty
           | Vertex Name
           | Overlay Graph Graph
           | Connect Graph Graph
           | ParenGraph Graph

instance Show Comm where
  show (DefVar n g)    = show n ++ " := " ++ show g
  show (Draw g)        = "draw " ++ show g
  show (Apply p n o d) = "apply " ++ show p ++ " to " ++ show n
                          ++ " from " ++ show o ++ " to " ++ show d
  show (Seq c1 c2)     = show c1 ++ "\n" ++ show c2

instance Show Graph where
  show Empty = "empty"
  show (Vertex n) = show n
  show (Overlay g1 g2) = show g1 ++ " + " ++ show g2
  show (Connect g1 g2) = show g1 ++ " * " ++ show g2
  show (ParenGraph g)  = "( " ++ show g ++ " )"
}
