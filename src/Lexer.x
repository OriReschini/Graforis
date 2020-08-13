{
module Lexer where

import AST
}

%wrapper "posn"

$digit = 0-9			   -- digits
$alpha = [a-zA-Z]		 -- alphabetic characters


tokens :-

  $white+                               ;
  "--".*                                ;
  draw				                       { \pos s -> DrawT pos }
  apply                              { \pos s -> ApplyT pos }
  ":="                               { \pos s -> AssignmentT pos }
  \;                                 { \pos s -> SemiColonT pos }
  \+                                 { \pos s -> OverlayT pos }
  \*                                 { \pos s -> ConnectT pos }
  \(                                 { \pos s -> ParenOpenT pos }
  \)		          	                 { \pos s -> ParenCloseT pos }
  empty                              { \pos s -> EmptyGT pos }
  [$alpha $digit \_ \']+	           { \pos s -> VertexT pos s }
  \# [$alpha $digit \_ \']+          { \pos s -> VarT pos s }
  [\/\.]* [$alpha $digit \_ \. \/]+  { \pos s -> PathT pos s }

{
data Token =  DrawT AlexPosn
            | ApplyT AlexPosn
            | AssignmentT AlexPosn
            | SemiColonT AlexPosn
            | OverlayT AlexPosn
            | ConnectT AlexPosn
            | ParenOpenT AlexPosn
            | ParenCloseT AlexPosn
            | EmptyGT AlexPosn
            | PathT AlexPosn String
            | VertexT AlexPosn String
            | VarT AlexPosn String
              deriving (Eq,Show)

token_posn (DrawT (AlexPn _ l c)) = Position l c
token_posn (ApplyT (AlexPn _ l c)) = Position l c
token_posn (AssignmentT (AlexPn _ l c)) = Position l c
token_posn (SemiColonT (AlexPn _ l c)) = Position l c
token_posn (OverlayT (AlexPn _ l c)) = Position l c
token_posn (ConnectT (AlexPn _ l c)) = Position l c
token_posn (ParenOpenT (AlexPn _ l c)) = Position l c
token_posn (ParenCloseT (AlexPn _ l c)) = Position l c
token_posn (EmptyGT (AlexPn _ l c)) = Position l c
token_posn (PathT (AlexPn _ l c) _) = Position l c
token_posn (VertexT (AlexPn _ l c) _) = Position l c
token_posn (VarT (AlexPn _ l c) _) = Position l c
}
