module Tiger where

import Prelude hiding (exp)
import Tiger.Tokens (scanTokens)
import Tiger.AST (Exp)
import Tiger.FindEscape (findEscapes)
import Tiger.Grammar (parse)
import Tiger.MipsFrame (MipsFrame)
import Tiger.Parser (runParser)
import Tiger.Semant (transExp)
import Tiger.Symbol (symbolGen)
import Tiger.Tc (runTc)
import Tiger.Temp (MonadTemp (namedLabel))
import Tiger.Translate (Frag, MonadTranslate (newLevel, functionDec), outermost)
import Tiger.Types (ExpTy, mkEnvs)

testParse :: String -> IO Exp
testParse s = do
  gen <- symbolGen
  let tokens = scanTokens s
  findEscapes <$> runParser gen (parse tokens)

testTc :: String -> IO (Either () ExpTy)
testTc s = do
  gen <- symbolGen
  let tokens = scanTokens s
  exp <- findEscapes <$> runParser gen (parse tokens)
  fmap (fmap fst) $ runTc gen $ do
    (venv, tenv) <- mkEnvs
    name <- namedLabel "main"
    level <- newLevel outermost name []
    transExp level venv tenv exp

testTrans :: String -> IO (Either () (ExpTy, [Frag MipsFrame]))
testTrans s = do
  gen <- symbolGen
  let tokens = scanTokens s
  exp <- findEscapes <$> runParser gen (parse tokens)
  runTc gen $ do
    (venv, tenv) <- mkEnvs
    name <- namedLabel "main"
    level <- newLevel outermost name []
    expTy@(exp', _) <- transExp level venv tenv exp
    functionDec level exp'
    return expTy

main :: IO ()
main = putStrLn "hello world"
