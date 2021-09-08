{
module Tiger.Grammar where

import Tiger.AST
import Tiger.Parser
import Tiger.Symbol
import Tiger.Tokens

}

%name parse
%tokentype { Token }
%error { parseError }
%monad { Parser }

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
  to           { Token _ TokenTo }
  '.'          { Token _ TokenDot }
  ':='         { Token _ TokenAssign }
  ':'          { Token _ TokenColon }
  ','          { Token _ TokenComma }
  ';'          { Token _ TokenSemicolon }
  '('          { Token _ TokenLParen }
  ')'          { Token _ TokenRParen }
  '{'          { Token _ TokenLBrack }
  '}'          { Token _ TokenRBrack }
  '['          { Token _ TokenLBrace }
  ']'          { Token _ TokenRBrace }
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

%nonassoc 'function' 'var' 'type' 'then' 'do' 'of' ':='
%nonassoc 'else'
%left '|'
%left '&'
%nonassoc '=' '<>' '<' '>' '>=' '<='
%left '+' '-'
%left '*' '/'
%left NEG

%%

Exp :: {Exp} 
  : let Decs in Exp end                      { LetExp (tokenToPos $1) (reverse $2) $4 }
  | break                                    { BreakExp (tokenToPos $1) }
  | nil                                      { NilExp (tokenToPos $1) }
  | for Id ':=' Exp to Exp do Exp %prec 'do' { ForExp (tokenToPos $1) $2 $4 $6 $8 }
  | while Exp do Exp %prec 'do'              { WhileExp (tokenToPos $1) $2 $4 }
  | if Exp then Exp else Exp %prec 'else'    { IfExp (tokenToPos $1) $2 $4 (Just $6) }
  | if Exp then Exp %prec 'do'               { IfExp (tokenToPos $1) $2 $4 Nothing }
  | Var ':=' Exp                             { AssignExp (tokenToPos $2) $1 $3 }
  | Id '[' Exp ']' of Exp %prec 'do'         { ArrayExp (tokenToPos $2) $1 $3 $6 }
  | Id '{' RecordFields '}'                  { RecordExp (tokenToPos $2) $1 (reverse $3) }
  | Id '(' Exps ')'                          { FuncallExp (tokenToPos $2) $1 (reverse $3) }
  | Var                                      { VarExp $1 }
  | '(' SeqExps ')'                          { SeqExp (tokenToPos $1) (reverse $2) }
  | Exp '&' Exp                              { OpExp (tokenToPos $2) AndOp $1 $3 }
  | Exp '|' Exp                              { OpExp (tokenToPos $2) OrOp $1 $3 }
  | Exp '=' Exp                              { OpExp (tokenToPos $2) EqOp $1 $3 }
  | Exp '<>' Exp                             { OpExp (tokenToPos $2) NeqOp $1 $3 }
  | Exp '>' Exp                              { OpExp (tokenToPos $2) GtOp $1 $3 }
  | Exp '<' Exp                              { OpExp (tokenToPos $2) LtOp $1 $3 }
  | Exp '>=' Exp                             { OpExp (tokenToPos $2) GteOp $1 $3 }
  | Exp '<=' Exp                             { OpExp (tokenToPos $2) LteOp $1 $3 }
  | Exp '-' Exp                              { OpExp (tokenToPos $2) SubOp $1 $3 }
  | Exp '+' Exp                              { OpExp (tokenToPos $2) AddOp $1 $3 }
  | Exp '*' Exp                              { OpExp (tokenToPos $2) MulOp $1 $3 }
  | Exp '/' Exp                              { OpExp (tokenToPos $2) DivOp $1 $3 }
  | '-' Exp %prec NEG                        { OpExp (tokenToPos $1) SubOp (IntExp 0) $2 }
  | INT                                      { IntExp $1 }
  | STRING                                   { StringExp $1 }

RecordFields :: {[(Pos, Symbol, Exp)]}
  : {- empty -}                 { [] }
  | RecordFields ',' Id '=' Exp { ((tokenToPos $2), $3, $5) : $1 }

Exps :: {[Exp]}
  : {- empty -}  { [] }
  | Exp          { [$1] }
  | Exps ',' Exp { $3 : $1 }

SeqExps :: {[Exp]}
  : {- empty -}     { [] }
  | Exp             { [$1] }
  | SeqExps ';' Exp { $3 : $1 }

Decs :: {[Dec]}
  : {- empty -} { [] }
  | Decs Dec    { $2 : $1 }

Dec :: {Dec}
  : TyDecs  { TyDecs (reverse $1) }
  | VarDec  { VarDec $1 }
  | FunDecs { FunDecs (reverse $1) }

TyDecs :: {[TyDec]}
  : TyDec        { [$1] }
  | TyDecs TyDec { $2 : $1 }

TyDec :: {TyDec}
  : type Id '=' Ty { TyDec (tokenToPos $1) $2 $4 }

VarDec :: {VarDec'}
  : var Id ':' Id ':=' Exp { VarDec' (tokenToPos $1) $2 (Just $4) $6 }
  | var Id ':=' Exp        { VarDec' (tokenToPos $1) $2 Nothing $4 }

FunDecs :: {[FunDec]}
  : FunDec         { [$1] }
  | FunDecs FunDec { $2 : $1 }

FunDec :: {FunDec}
  : function Id '(' Tyfields ')' ':' Id '=' Exp { FunDec (tokenToPos $1) $2 (reverse $4) (Just $7) $9 }
  | function Id '(' Tyfields ')' '=' Exp        { FunDec (tokenToPos $1) $2 (reverse $4) Nothing $7 }

Ty :: {Ty}
  : Id               { IdTy $1 }
  | '{' Tyfields '}' { FieldsTy (tokenToPos $1) (reverse $2) }
  | array of Id      { ArrayOfTy (tokenToPos $1) $3 }

Tyfields :: {[TyField]}
  : {- empty -}            { [] }
  | Id ':' Id              { [TyField (tokenToPos $2) $1 $3] }
  | Tyfields ',' Id ':' Id { TyField (tokenToPos $2) $3 $5 : $1 }

Var :: {Var} 
  : Id              { Var $1 }
  | VarTail         { $1 }     {- Uses VarTail to handle shift/reduce conflict with ArrayExp -}

VarTail :: {Var}
  : Id '.' Id           { RecField (tokenToPos $2) (Var $1) $3 }
  | VarTail '.' Id      { RecField (tokenToPos $2) $1 $3 }
  | Id '[' Exp ']'      { ArraySub (tokenToPos $2) (Var $1) $3 }
  | VarTail '[' Exp ']' { ArraySub (tokenToPos $2) $1 $3 }

Id :: {Symbol}
  : ID {% symbol $1 }

{
parseError :: [Token] -> a
parseError (Token p t:_) =
  error $ "Parse error at position: " ++ show p ++ " with token: " ++ show t
parseError _ = error "Parse error"
}