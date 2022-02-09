{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Tiger where

import Prelude hiding (exp)
import Control.Applicative (liftA2)
import Control.Monad (zipWithM)
import Data.List (intersperse)
import Data.Maybe (fromJust, fromMaybe)
import Tiger.Assem (format)
import Tiger.AST (Exp)
import Tiger.Canon (linearize, basicBlocks, traceSchedule)
import Tiger.Codegen (Codegen (codegen))
import Tiger.FindEscape (findEscapes)
import Tiger.Grammar (parse)
import Tiger.MipsFrame (MipsFrame, MipsRegisters, mkMipsRegisters)
import Tiger.Parser (runParser)
import Tiger.RegAlloc (alloc)
import Tiger.Semant (tcIO)
import Tiger.Symbol (SymGen, mkSymbolGen)
import Tiger.Temp
import Tiger.Tokens (scanTokens)
import Tiger.Translate (Frag (..))
import Tiger.Tree (Stm)
import Tiger.Types (ExpTy)
import qualified Data.IntMap.Strict as IM
import qualified Tiger.Frame as F
import qualified Tiger.MipsFrame as F

data State = State
  { symGen      :: SymGen
  , tempGen     :: IO Temp
  , regs        :: MipsRegisters
  , tempIO      :: Temp_ IO
  , frameIO     :: F.Frame_ MipsFrame IO
  , tc          :: Exp -> IO (Maybe (ExpTy, [Frag MipsFrame]))
  , tempSupply  :: Supply Temp
  , labelSupply :: Supply Label
  }

mkState :: IO State
mkState = do
  symGen <- mkSymbolGen
  tempGen <- mkTempGen 0
  tempSupply <- mkSupply tempGen
  labelSupply <- mkSupply (label symGen tempGen)
  regs <- mkMipsRegisters tempGen
  let tempIO  = temp_ symGen tempGen
      frameIO = F.frameIO tempIO regs
  tc <- tcIO tempIO frameIO
  return State{..}

testParse :: String -> IO Exp
testParse s = do
  State{..} <- mkState
  let tokens = scanTokens s
  findEscapes <$> runParser (parse tokens) symGen

testTc :: String -> IO ExpTy
testTc s = do
  State{..} <- mkState
  let tokens = scanTokens s
  exp <- findEscapes <$> runParser (parse tokens) symGen
  fst . fromJust <$> tc exp

testTrans :: String -> IO [Frag MipsFrame]
testTrans s = do
  State{..} <- mkState
  let tokens = scanTokens s
  exp <- findEscapes <$> runParser (parse tokens) symGen
  snd . fromJust <$> tc exp

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
  mconcat . buildAll . snd . fromJust <$> tc exp

testCodegen :: String -> IO ()
testCodegen s = do
  State{..} <- mkState
  let tokens = scanTokens s
  exp <- findEscapes <$> runParser (parse tokens) symGen
  frags <- snd . fromJust <$> tc exp
  let build (S _ ls1 ls2, S _ ts1 ts2) (ProcFrag stm frame)
        = mconcat . fmap (\(ts', stm') -> codegen ts' frame stm')
        . zip (supplies ts1)
        . uncurry (traceSchedule ls1)
        . basicBlocks ls2
        $ linearize ts2 stm
      build _ (StringFrag _ _) = []
      buildAll = mconcat . fmap (uncurry build)
               . zip (zip (supplies labelSupply) (supplies tempSupply))
  mapM_ print $ buildAll frags

addSections :: [Frag MipsFrame] -> [String] -> [String]
addSections a b = ".data":go a b
 where
  go (ProcFrag _ _:_) ss = ".text":ss
  go (_:fs) (s:ss)       = s:go fs ss
  go _ _                 = []

compile :: String -> IO String
compile s = do
  State{frameIO=F.Frame_{..},..} <- mkState
  let tokens = scanTokens s
  exp <- findEscapes <$> runParser (parse tokens) symGen
  frags <- snd . fromJust <$> tc exp
  let
    sayTemp m (Temp t) = fromMaybe (show t) (IM.lookup t m)
    build (S _ ls1 ls2, S _ (S _ ts1 ts2) ts3) (ProcFrag stm frame) = do
      localSupply <- mkSupply (allocLocal frame True)
      let
        instrs = F.procEntryExit2 frame . mconcat
               . fmap (\(ts', stm') -> codegen ts' frame stm')
               . zip (supplies ts1)
               . uncurry (traceSchedule ls1)
               . basicBlocks ls2
               $ linearize ts2 stm
        (instrs', alloc') = alloc (liftA2 (,) ts3 localSupply) instrs frame
      (_, instrs'', _) <- procEntryExit3 frame instrs'
      pure $ mconcat $ intersperse "\n" $ fmap (format (sayTemp alloc')) instrs''
    build _ (StringFrag lab str) = pure $ F.string @MipsFrame lab str
    buildAll = zipWithM build (zip (supplies labelSupply) (supplies tempSupply))
  mconcat . intersperse "\n" . addSections frags <$> buildAll frags

main :: IO ()
main = readFile "queens.tig" >>= compile >>= writeFile "queens.s"
