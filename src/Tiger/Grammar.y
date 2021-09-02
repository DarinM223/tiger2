{
module Tiger.Grammar where

import Tiger.Tokens
import Tiger.AST
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  let          { Token _ TokenLet }
  in           { Token _ TokenIn }
  end          { Token _ TokenEnd}
  break        { Token _ TokenBreak }
  type         { Token _ TokenType }
  array        { Token _ TokenArray }
  of           { Token _ TokenOf }
  var          { Token _ TokenVar }
  nil          { Token _ TokenNil }
  if           { Token _ TokenIf }
  then         { Token _ TokenThen }
  else         { Token _ TokenElse }
  while        { Token _ TokenWhile }
  for          { Token _ TokenFor }
  do           { Token _ TokenDo }
  ':='         { Token _ TokenAssign }
  ':'          { Token _ TokenColon }
  ','          { Token _ TokenComma }
  ';'          { Token _ TokenSemicolon }
  '('          { Token _ TokenLParen }
  ')'          { Token _ TokenRParen }
  '{'          { Token _ TokenLBrack }
  '}'          { Token _ TokenRBrack }
  '+'          { Token _ TokenPlus }
  '-'          { Token _ TokenMinus }
  '*'          { Token _ TokenTimes }
  '/'          { Token _ TokenDiv }
  '='          { Token _ TokenEquals }
  '<>'         { Token _ TokenNotEquals }
  '<='         { Token _ TokenLte }
  '>='         { Token _ TokenGte }
  '<'          { Token _ TokenLt }
  '>'          { Token _ TokenGt }
  '&'          { Token _ TokenAnd }
  '|'          { Token _ TokenOr }
  function     { Token _ TokenFunction }
  INT          { Token _ (TokenInteger $$) }
  ID           { Token _ (TokenSymbol $$) }
  STRING       { Token _ (TokenString $$) }

%%

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}