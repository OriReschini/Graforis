{
module Parser where

import Lexer
import AST
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
  vertex    { VertexT _ $$ }
  var       { VarT _ $$ }
  filePath  { PathT _ $$ }

%nonassoc ':='
%right ';'
%left '+'
%left '*'

%%

Exp :: { Comm }
Exp : var ':=' Graph_                     { DefVar $1 $3 }
    | draw Graph_                         { Draw $2 }
    | apply filePath Graph_ vertex vertex { Apply $2 $3 $4 $5 }
    | Exp ';' Exp                         { Seq $1 $3 }

Graph_ :: { Graph }
Graph_ : empty              { Empty }
       | vertex             { Vertex $1 }
       | Graph_ '+' Graph_  { Overlay $1 $3 }
       | Graph_ '*' Graph_  { Connect $1 $3 }
       | var                { Var $1 }
       | '(' Graph_ ')'     { $2 }

{
parseError :: [Token] -> a
parseError [] = error "Parse error. Maybe empty file."
parseError (t : _) = -- error $ (show t)
    error $ "Parse error " ++ (show . token_posn) t ++ "."

}
