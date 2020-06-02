{
module Lexer where
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
  [$alpha $digit \_ \']+	           { \pos s -> NameT pos s }
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
            | NameT AlexPosn String
              deriving (Eq,Show)

token_posn (DrawT p) = p
token_posn (ApplyT p) = p
token_posn (AssignmentT p) = p
token_posn (SemiColonT p) = p
token_posn (OverlayT p) = p
token_posn (ConnectT p) = p
token_posn (ParenOpenT p) = p
token_posn (ParenCloseT p) = p
token_posn (EmptyGT p) = p
token_posn (PathT p _) = p
token_posn (NameT p _) = p 
}
