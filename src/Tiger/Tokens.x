{
module Tiger.Tokens where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+            ;
  let                { \p s -> TokenLet p }
  in                 { \p s -> TokenIn p }
  end                { \p s -> TokenEnd p }
  break              { \p s -> TokenBreak p }
  type               { \p s -> TokenType p }
  array              { \p s -> TokenArray p }
  of                 { \p s -> TokenOf p }
  var                { \p s -> TokenVar p }
  nil                { \p s -> TokenNil p }
  if                 { \p s -> TokenIf p }
  then               { \p s -> TokenThen p }
  else               { \p s -> TokenElse p }
  while              { \p s -> TokenWhile p }
  for                { \p s -> TokenFor p }
  do                 { \p s -> TokenDo p }
  :=                 { \p s -> TokenAssign p }
  :                  { \p s -> TokenColon p }
  \,                 { \p s -> TokenComma p }
  \;                 { \p s -> TokenSemicolon p }
  \(                 { \p s -> TokenLParen p }
  \)                 { \p s -> TokenRParen p }
  \{                 { \p s -> TokenLBrack p }
  \}                 { \p s -> TokenRBrack p }
  \+                 { \p s -> TokenPlus p }
  \-                 { \p s -> TokenMinus p }
  \*                 { \p s -> TokenTimes p }
  \/                 { \p s -> TokenDiv p }
  =                  { \p s -> TokenEquals p }
  \<>                { \p s -> TokenNotEquals p }
  \<=                { \p s -> TokenLte p }
  \>=                { \p s -> TokenGte p }
  \<                 { \p s -> TokenLt p }
  \>                 { \p s -> TokenGt p }
  \&                 { \p s -> TokenAnd p }
  \|                 { \p s -> TokenOr p }
  function           { \p s -> TokenFunction p }
  $digit+            { \p s -> TokenInteger p (read s) }
  $alpha [$alpha $digit \_ \']* { \p s -> TokenSymbol p s }
  \" \"                   {(\p s -> TokenString p $ tail $ init s)}
  \" ([^\"]|\\ \")* \"    {(\p s -> TokenString p $ tail $ init s)}

{
data Token
  = TokenLet AlexPosn
  | TokenIn AlexPosn
  | TokenEnd AlexPosn
  | TokenBreak AlexPosn
  | TokenType AlexPosn
  | TokenArray AlexPosn
  | TokenOf AlexPosn
  | TokenVar AlexPosn
  | TokenNil AlexPosn
  | TokenIf AlexPosn
  | TokenThen AlexPosn
  | TokenElse AlexPosn
  | TokenWhile AlexPosn
  | TokenFor AlexPosn
  | TokenDo AlexPosn
  | TokenAssign AlexPosn
  | TokenColon AlexPosn
  | TokenComma AlexPosn
  | TokenSemicolon AlexPosn
  | TokenLParen AlexPosn | TokenRParen AlexPosn
  | TokenLBrack AlexPosn | TokenRBrack AlexPosn
  | TokenPlus AlexPosn | TokenMinus AlexPosn | TokenTimes AlexPosn | TokenDiv AlexPosn
  | TokenEquals AlexPosn | TokenNotEquals AlexPosn
  | TokenLte AlexPosn | TokenGte AlexPosn | TokenLt AlexPosn | TokenGt AlexPosn
  | TokenAnd AlexPosn | TokenOr AlexPosn
  | TokenFunction AlexPosn
  | TokenInteger AlexPosn Int
  | TokenSymbol AlexPosn String
  | TokenString AlexPosn String
  deriving (Show, Eq)

scanTokens = alexScanTokens
}