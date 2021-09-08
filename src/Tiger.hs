module Tiger where

import Control.Monad (void)
import Tiger.Tokens (scanTokens)
import Tiger.AST (Exp)
import Tiger.Grammar (parse)
import Tiger.Parser (runParser)
import Tiger.Semant (transExp)
import Tiger.Symbol (symbolGen)
import Tiger.Tc (runTc)
import qualified Data.IntMap as IM

testParse :: String -> IO Exp
testParse s = do
  gen <- symbolGen
  let tokens = scanTokens s
  runParser gen $ parse tokens

testTc :: String -> IO ()
testTc s = do
  gen <- symbolGen
  let tokens = scanTokens s
  runParser gen (parse tokens) >>=
    void . runTc gen . transExp IM.empty IM.empty

main :: IO ()
main = putStrLn "hello world"
