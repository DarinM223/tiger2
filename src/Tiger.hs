{-# LANGUAGE RecordWildCards #-}
module Tiger where

import Prelude hiding (exp)
import Control.Applicative (liftA2)
import Control.DeepSeq (deepseq)
import Control.Monad (zipWithM)
import Data.List (intersperse)
import Data.Maybe (fromJust, fromMaybe)
import System.Environment (getArgs)
import System.FilePath ((<.>), (</>))
import Tiger.Assem (format)
import Tiger.AST (Exp)
import Tiger.Canon (linearize, basicBlocks, traceSchedule)
import Tiger.Codegen (Codegen (codegen))
import Tiger.FindEscape (findEscapes)
import Tiger.Grammar (parse)
import Tiger.MipsFrame (MipsFrame, MipsRegisters, mkMipsRegisters)
import Tiger.Parser (runParser)
import Tiger.Pretty (printStm, printFrag)
import Tiger.RegAlloc (alloc)
import Tiger.Semant (tcIO)
import Tiger.Symbol (SymGen, mkSymbolGen, symbolId)
import Tiger.Temp
import Tiger.Tokens (scanTokens)
import Tiger.Translate (Frag (..))
import Tiger.Types (ExpTy)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
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
  regs <- mkMipsRegisters tempGen
  let tempIO  = temp_ symGen tempGen
      frameIO = F.frameIO tempIO regs
  tc <- tcIO tempIO frameIO
  tempSupply <- mkSupply tempGen
  labelSupply <- mkSupply (newLabel tempIO)
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

testCanon :: String -> IO ()
testCanon s = do
  State{..} <- mkState
  let tokens = scanTokens s
  exp <- findEscapes <$> runParser (parse tokens) symGen
  let build (S _ ls1 ls2, ts) frag@(ProcFrag stm frame) = do
        putStrLn $ "\n\n" ++ show (F.frameName frame) ++ ":\n"
        printFrag frag
        putStrLn "\n"
        mapM_ (\s' -> printStm 0 s' >> putStrLn "") $
          uncurry (traceSchedule ls1) $ basicBlocks ls2 $ linearize ts stm
      build _ (StringFrag _ _) = pure ()
      buildAll = mapM_ (uncurry build)
               . zip (zip (supplies labelSupply) (supplies tempSupply))
  tc exp >>= buildAll . snd . fromJust

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
addSections = go IS.empty Nothing
 where
  go m Nothing fs@(f@(ProcFrag _ _):_) ss = ".text":go m (Just f) fs ss
  go m Nothing fs@(f@(StringFrag _ _):_) ss = ".data":go m (Just f) fs ss
  go m o@(Just (ProcFrag _ _)) (ProcFrag _ _:fs) (s:ss) = s:go m o fs ss
  go m o@(Just (StringFrag _ _)) (StringFrag lab _:fs) (s:ss)
    | IS.member (symbolId lab) m = go m o fs ss
    | otherwise = s:go (IS.insert (symbolId lab) m) o fs ss
  go m (Just (StringFrag _ _)) fs@(f@(ProcFrag _ _):_) ss =
    ".text":go m (Just f) fs ss
  go m (Just (ProcFrag _ _)) fs@(f@(StringFrag _ _):_) ss =
    ".data":go m (Just f) fs ss
  go _ _ _ _ = []

compile :: String -> IO String
compile s = do
  State{frameIO=F.Frame_{..},..} <- mkState
  let tokens = scanTokens s
  exp <- findEscapes <$> runParser (parse tokens) symGen
  frags <- snd . fromJust <$> tc exp
  let
    fragFormals (ProcFrag _ frame) = length $ F.formals frame
    fragFormals (StringFrag _ _)   = 0
    maxFormals = maximum $ fmap fragFormals frags
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
      -- Make sure instrs' is fully generated before calling procEntryExit3
      (_, instrs'', _) <- instrs' `deepseq` procEntryExit3 frame instrs' maxFormals
      pure $ mconcat $ intersperse "\n" $ fmap (format (sayTemp alloc')) instrs''
    build _ (StringFrag lab str) = pure $ F.string @MipsFrame lab str
    buildAll = zipWithM build (zip (supplies labelSupply) (supplies tempSupply))
  mconcat . intersperse "\n" . addSections frags <$> buildAll frags

main :: IO ()
main = do
  name <- head <$> getArgs
  readFile ("runtime" <.> "s") >>= writeFile ("programs" </> name <.> "s")
  readFile ("programs" </> name <.> "tig")
    >>= compile
    >>= appendFile ("programs" </> name <.> "s")
