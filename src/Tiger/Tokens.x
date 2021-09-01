{
module Tiger.Tokens where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+            ;
  let                { \s -> TokenLet }
  in                 { \s -> TokenIn }
  end                { \s -> TokenEnd }
  break              { \s -> TokenBreak }
  type               { \s -> TokenType }
  array              { \s -> TokenArray }
  of                 { \s -> TokenOf }
  var                { \s -> TokenVar }
  nil                { \s -> TokenNil }
  :=                 { \s -> TokenAssign }
  :                  { \s -> TokenColon }
  \,                 { \s -> TokenComma }
  \;                 { \s -> TokenSemicolon }
  \(                 { \s -> TokenLParen }
  \)                 { \s -> TokenRParen }
  \+                 { \s -> TokenPlus }
  \-                 { \s -> TokenMinus }
  \*                 { \s -> TokenTimes }
  \/                 { \s -> TokenDiv }
  =                  { \s -> TokenEquals }
  \<>                { \s -> TokenNotEquals }
  \<=                { \s -> TokenLte }
  \>=                { \s -> TokenGte }
  \<                 { \s -> TokenLt }
  \>                 { \s -> TokenGt}
  \&                 { \s -> TokenAnd }
  \|                 { \s -> TokenOr }
  function           { \s -> TokenFunction }
  $digit+            { \s -> TokenInteger (read s) }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSymbol s }

{
data Token
  = TokenLet
  | TokenIn
  | TokenEnd
  | TokenBreak
  | TokenType
  | TokenArray
  | TokenOf
  | TokenVar
  | TokenNil
  | TokenAssign
  | TokenColon
  | TokenComma
  | TokenSemicolon
  | TokenLParen | TokenRParen
  | TokenPlus | TokenMinus | TokenTimes | TokenDiv
  | TokenEquals | TokenNotEquals
  | TokenLte | TokenGte | TokenLt | TokenGt
  | TokenAnd | TokenOr
  | TokenFunction
  | TokenInteger Int
  | TokenSymbol String
  deriving (Show, Eq)

scanTokens = alexScanTokens
}