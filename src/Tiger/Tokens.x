{
module Tokens where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+            ;

{
data Token = Token

scanTokens = alexScanTokens
}