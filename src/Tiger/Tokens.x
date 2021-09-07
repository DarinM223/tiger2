{
module Tiger.Tokens where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+                       ;
  let                           { \p _ -> Token p TokenLet }
  in                            { \p _ -> Token p TokenIn }
  end                           { \p _ -> Token p TokenEnd }
  break                         { \p _ -> Token p TokenBreak }
  type                          { \p _ -> Token p TokenType }
  array                         { \p _ -> Token p TokenArray }
  of                            { \p _ -> Token p TokenOf }
  var                           { \p _ -> Token p TokenVar }
  nil                           { \p _ -> Token p TokenNil }
  if                            { \p _ -> Token p TokenIf }
  then                          { \p _ -> Token p TokenThen }
  else                          { \p _ -> Token p TokenElse }
  while                         { \p _ -> Token p TokenWhile }
  for                           { \p _ -> Token p TokenFor }
  do                            { \p _ -> Token p TokenDo }
  to                            { \p _ -> Token p TokenTo }
  \.                            { \p _ -> Token p TokenDot }
  :=                            { \p _ -> Token p TokenAssign }
  :                             { \p _ -> Token p TokenColon }
  \,                            { \p _ -> Token p TokenComma }
  \;                            { \p _ -> Token p TokenSemicolon }
  \(                            { \p _ -> Token p TokenLParen }
  \)                            { \p _ -> Token p TokenRParen }
  \{                            { \p _ -> Token p TokenLBrack }
  \}                            { \p _ -> Token p TokenRBrack }
  \[                            { \p _ -> Token p TokenLBrace }
  \]                            { \p _ -> Token p TokenRBrace }
  \+                            { \p _ -> Token p TokenPlus }
  \-                            { \p _ -> Token p TokenMinus }
  \*                            { \p _ -> Token p TokenTimes }
  \/                            { \p _ -> Token p TokenDiv }
  =                             { \p _ -> Token p TokenEquals }
  \<>                           { \p _ -> Token p TokenNotEquals }
  \<=                           { \p _ -> Token p TokenLte }
  \>=                           { \p _ -> Token p TokenGte }
  \<                            { \p _ -> Token p TokenLt }
  \>                            { \p _ -> Token p TokenGt }
  \&                            { \p _ -> Token p TokenAnd }
  \|                            { \p _ -> Token p TokenOr }
  function                      { \p _ -> Token p TokenFunction }
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