{
module Grammar where

import Tokens
}

%name parse
%tokentype { Token }
%error { parseError }

%%

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}