{
module Tiger.Tokens where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+                       ;
  let                           { \p s -> Token p TokenLet }
  in                            { \p s -> Token p TokenIn }
  end                           { \p s -> Token p TokenEnd }
  break                         { \p s -> Token p TokenBreak }
  type                          { \p s -> Token p TokenType }
  array                         { \p s -> Token p TokenArray }
  of                            { \p s -> Token p TokenOf }
  var                           { \p s -> Token p TokenVar }
  nil                           { \p s -> Token p TokenNil }
  if                            { \p s -> Token p TokenIf }
  then                          { \p s -> Token p TokenThen }
  else                          { \p s -> Token p TokenElse }
  while                         { \p s -> Token p TokenWhile }
  for                           { \p s -> Token p TokenFor }
  do                            { \p s -> Token p TokenDo }
  to                            { \p s -> Token p TokenTo }
  \.                            { \p s -> Token p TokenDot }
  :=                            { \p s -> Token p TokenAssign }
  :                             { \p s -> Token p TokenColon }
  \,                            { \p s -> Token p TokenComma }
  \;                            { \p s -> Token p TokenSemicolon }
  \(                            { \p s -> Token p TokenLParen }
  \)                            { \p s -> Token p TokenRParen }
  \{                            { \p s -> Token p TokenLBrack }
  \}                            { \p s -> Token p TokenRBrack }
  \[                            { \p s -> Token p TokenRBrace }
  \]                            { \p s -> Token p TokenLBrace }
  \+                            { \p s -> Token p TokenPlus }
  \-                            { \p s -> Token p TokenMinus }
  \*                            { \p s -> Token p TokenTimes }
  \/                            { \p s -> Token p TokenDiv }
  =                             { \p s -> Token p TokenEquals }
  \<>                           { \p s -> Token p TokenNotEquals }
  \<=                           { \p s -> Token p TokenLte }
  \>=                           { \p s -> Token p TokenGte }
  \<                            { \p s -> Token p TokenLt }
  \>                            { \p s -> Token p TokenGt }
  \&                            { \p s -> Token p TokenAnd }
  \|                            { \p s -> Token p TokenOr }
  function                      { \p s -> Token p TokenFunction }
  $digit+                       { \p s -> Token p $ TokenInteger (read s) }
  $alpha [$alpha $digit \_ \']* { \p s -> Token p $ TokenSymbol s }
  \" \"                         { (\p s -> Token p $ TokenString $ tail $ init s) }
  \" ([^\"]|\\ \")* \"          { (\p s -> Token p $ TokenString $ tail $ init s) }

{
data Token = Token AlexPosn Token' deriving (Show, Eq)

data Token'
  = TokenLet
  | TokenIn
  | TokenEnd
  | TokenBreak
  | TokenType
  | TokenArray
  | TokenOf
  | TokenVar
  | TokenNil
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenWhile
  | TokenFor
  | TokenDo
  | TokenTo
  | TokenDot
  | TokenAssign
  | TokenColon
  | TokenComma
  | TokenSemicolon
  | TokenLParen | TokenRParen
  | TokenLBrack | TokenRBrack
  | TokenLBrace | TokenRBrace
  | TokenPlus | TokenMinus | TokenTimes | TokenDiv
  | TokenEquals | TokenNotEquals
  | TokenLte | TokenGte | TokenLt | TokenGt
  | TokenAnd | TokenOr
  | TokenFunction
  | TokenInteger Int
  | TokenSymbol String
  | TokenString String
  deriving (Show, Eq)

tokenToPos :: Token -> AlexPosn
tokenToPos (Token p _) = p

scanTokens = alexScanTokens
}