{-# LANGUAGE RecordWildCards #-}
module Tiger where

import Prelude hiding (exp)
import Tiger.Tokens (scanTokens)
import Tiger.AST (Exp)
import Tiger.Canon (linearize, basicBlocks, traceSchedule)
import Tiger.Codegen (Codegen (codegen))
import Tiger.FindEscape (findEscapes)
import Tiger.Grammar (parse)
import Tiger.MipsFrame (MipsFrame, MipsRegisters, mkMipsRegisters)
import Tiger.Parser (runParser)
import Tiger.Semant (transExp)
import Tiger.Symbol (SymGen, mkSymbolGen)
import Tiger.Tc (TcState (TcState), runTc)
import Tiger.Temp
import Tiger.Translate (Frag (..), MonadTranslate (..), levelFrame, outermost)
import Tiger.Tree (Stm)
import Tiger.Types (ExpTy, mkEnvs)

data State = State
  { symGen      :: SymGen
  , tempGen     :: IO Temp
  , regs        :: MipsRegisters
  , tcState     :: TcState
  , tempSupply  :: Supply Temp
  , labelSupply :: Supply Label
  }

mkState :: IO State
mkState = do
  symGen <- mkSymbolGen
  tempGen <- mkTempGen
  tempSupply <- mkSupply tempGen
  labelSupply <- mkSupply (label symGen tempGen)
  regs <- mkMipsRegisters tempGen
  let tcState = TcState symGen tempGen regs undefined undefined
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
  fmap (either (const []) snd) $ flip runTc tcState $ do
    (venv, tenv) <- mkEnvs
    name <- namedLabel "main"
    level <- newLevel outermost name []
    (exp', _) <- transExp level venv tenv exp
    functionDec level exp'

testCanon :: String -> IO [Stm]
testCanon s = do
  State{..} <- mkState
  let tokens = scanTokens s
  exp <- findEscapes <$> runParser (parse tokens) symGen
  let build (S _ ls1 ls2, ts) (ProcFrag stm _) =
        uncurry (traceSchedule ls1) $ basicBlocks ls2 $ linearize ts stm
      build _ (StringFrag _ _) = []
      buildAll = fmap (uncurry build)
               . zip (zip (supplies labelSupply) (supplies tempSupply))
  fmap (mconcat . buildAll . either (const []) snd) $ flip runTc tcState $ do
    (venv, tenv) <- mkEnvs
    name <- namedLabel "main"
    level <- newLevel outermost name []
    (exp', _) <- transExp level venv tenv exp
    functionDec level exp'

testCodegen :: String -> IO ()
testCodegen s = do
  State{..} <- mkState
  let tokens = scanTokens s
  exp <- findEscapes <$> runParser (parse tokens) symGen
  Right (frame, frags) <- flip runTc tcState $ do
    (venv, tenv) <- mkEnvs
    name <- namedLabel "main"
    level <- newLevel outermost name []
    (exp', _) <- transExp level venv tenv exp
    functionDec level exp'
    return $ levelFrame level
  let build (S _ ls1 ls2, S _ ts1 ts2) (ProcFrag stm _)
        = mconcat . fmap (\(ts', stm') -> codegen ts' frame stm')
        . zip (supplies ts1)
        . uncurry (traceSchedule ls1)
        . basicBlocks ls2
        $ linearize ts2 stm
      build _ (StringFrag _ _) = []
      buildAll = mconcat . fmap (uncurry build)
               . zip (zip (supplies labelSupply) (supplies tempSupply))
  mapM_ print $ buildAll frags

main :: IO ()
main = putStrLn "hello world"
