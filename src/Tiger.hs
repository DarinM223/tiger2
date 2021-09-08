module Tiger where

import Tiger.Tokens (scanTokens)
import Tiger.AST (Exp)
import Tiger.Grammar (parse)
import Tiger.Parser (runParser)
import Tiger.Semant (transExp)
import Tiger.Symbol (symbolGen)
import Tiger.Tc (runTc)
import Tiger.Types (ExpTy, mkEnvs)

testParse :: String -> IO Exp
testParse s = do
  gen <- symbolGen
  let tokens = scanTokens s
  runParser gen $ parse tokens

testTc :: String -> IO (Either () ExpTy)
testTc s = do
  gen <- symbolGen
  let tokens = scanTokens s
  (venv, tenv) <- mkEnvs gen
  runParser gen (parse tokens) >>= runTc gen . transExp venv tenv

main :: IO ()
main = putStrLn "hello world"
