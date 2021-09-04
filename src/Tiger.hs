module Tiger (main) where

import Tiger.Tokens (scanTokens)
import Tiger.AST (Exp)
import Tiger.Grammar (parse)
import Tiger.Parser (runParser)
import Tiger.Symbol (symbolGen)

test :: String -> IO Exp
test s = do
  gen <- symbolGen
  let tokens = scanTokens s
  runParser gen $ parse tokens

main :: IO ()
main = putStrLn "hello world"
