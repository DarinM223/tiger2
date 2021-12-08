{-# LANGUAGE RecordWildCards #-}
module Tiger where

import Prelude hiding (exp)
import Tiger.Tokens (scanTokens)
import Tiger.AST (Exp)
import Tiger.FindEscape (findEscapes)
import Tiger.Grammar (parse)
import Tiger.MipsFrame (MipsFrame, MipsRegisters, mkMipsRegisters)
import Tiger.Parser (runParser)
import Tiger.Semant (transExp)
import Tiger.Symbol (SymGen, mkSymbolGen)
import Tiger.Tc (TcState (TcState), runTc)
import Tiger.Temp (MonadTemp (namedLabel), Temp, mkTempGen)
import Tiger.Translate (Frag, MonadTranslate (newLevel, functionDec), outermost)
import Tiger.Types (ExpTy, mkEnvs)

data State = State
  { symGen  :: SymGen
  , tempGen :: IO Temp
  , regs    :: MipsRegisters
  , tcState :: TcState
  }

mkState :: IO State
mkState = do
  symGen <- mkSymbolGen
  tempGen <- mkTempGen
  regs <- mkMipsRegisters tempGen
  let tcState = TcState symGen tempGen undefined undefined undefined
  return State{..}

testParse :: String -> IO Exp
testParse s = do
  State{..} <- mkState
  let tokens = scanTokens s
  findEscapes <$> runParser (parse tokens) symGen

testTc :: String -> IO (Either () ExpTy)
testTc s = do
  State{..} <- mkState
  let tokens = scanTokens s
  exp <- findEscapes <$> runParser (parse tokens) symGen
  fmap (fmap fst) $ flip runTc tcState $ do
    (venv, tenv) <- mkEnvs
    name <- namedLabel "main"
    level <- newLevel outermost name []
    transExp level venv tenv exp

testTrans :: String -> IO [Frag MipsFrame]
testTrans s = do
  State{..} <- mkState
  let tokens = scanTokens s
  exp <- findEscapes <$> runParser (parse tokens) symGen
  fmap (either (const []) (reverse . snd)) $ flip runTc tcState $ do
    (venv, tenv) <- mkEnvs
    name <- namedLabel "main"
    level <- newLevel outermost name []
    (exp', _) <- transExp level venv tenv exp
    functionDec level exp'

main :: IO ()
main = putStrLn "hello world"
