module Tiger where

import Prelude hiding (exp)
import Tiger.Tokens (scanTokens)
import Tiger.AST (Exp)
import Tiger.FindEscape (findEscapes)
import Tiger.Grammar (parse)
import Tiger.MipsFrame (MipsFrame)
import Tiger.Parser (runParser)
import Tiger.Semant (transExp)
import Tiger.Symbol (mkSymbolGen)
import Tiger.Tc (runTc)
import Tiger.Temp (MonadTemp (namedLabel), mkTempGen)
import Tiger.Translate (Frag, MonadTranslate (newLevel, functionDec), outermost)
import Tiger.Types (ExpTy, mkEnvs)

testParse :: String -> IO Exp
testParse s = do
  gen <- mkSymbolGen
  let tokens = scanTokens s
  findEscapes <$> runParser (parse tokens) gen

testTc :: String -> IO (Either () ExpTy)
testTc s = do
  symGen <- mkSymbolGen
  tempGen <- mkTempGen
  let tokens = scanTokens s
  exp <- findEscapes <$> runParser (parse tokens) symGen
  fmap (fmap fst) $ runTc symGen tempGen $ do
    (venv, tenv) <- mkEnvs
    name <- namedLabel "main"
    level <- newLevel outermost name []
    transExp level venv tenv exp

testTrans :: String -> IO [Frag MipsFrame]
testTrans s = do
  symGen <- mkSymbolGen
  tempGen <- mkTempGen
  let tokens = scanTokens s
  exp <- findEscapes <$> runParser (parse tokens) symGen
  fmap (either (const []) (reverse . snd)) $ runTc symGen tempGen $ do
    (venv, tenv) <- mkEnvs
    name <- namedLabel "main"
    level <- newLevel outermost name []
    (exp', _) <- transExp level venv tenv exp
    functionDec level exp'

main :: IO ()
main = putStrLn "hello world"
