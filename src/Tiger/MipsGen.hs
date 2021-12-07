{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Tiger.MipsGen where

import Control.Applicative (liftA2, liftA3)
import Control.Monad.Reader (ReaderT (..), MonadTrans (lift), asks)
import Data.IORef
import Tiger.Codegen
import Tiger.MipsFrame (MipsFrame)
import Tiger.Symbol (Gen, MonadSymbol)
import Tiger.Temp
import Tiger.Translate (MonadPut (put))
import Tiger.Tree
import qualified Tiger.Frame as F

calldefs :: F.Frame frame => frame -> [Temp]
calldefs f = F.ra f:F.rv f:F.callerSaves f

emit :: (Monad m, MonadPut a m) => a -> ReaderT r m ()
emit = lift . put

result :: MonadTemp m => (Temp -> ReaderT r m a) -> ReaderT r m Temp
result f = lift newTemp >>= \t -> t <$ f t

munchStm :: (MonadTemp m, MonadPut Instr m, F.Frame frame)
         => Stm -> ReaderT frame m ()
munchStm (SeqStm a b) = munchStm a >> munchStm b
munchStm (MoveStm (MemExp (BinOpExp Plus e1 (ConstExp i))) e2) = emit =<< liftA3
  (OperInstr ("sw `s1, " ++ show i ++ "(`s0)"))
  (traverse munchExp [e1, e2]) (pure []) (pure Nothing)
munchStm (MoveStm (MemExp (BinOpExp Plus (ConstExp i) e1)) e2) = emit =<< liftA3
  (OperInstr ("sw `s1, " ++ show i ++ "(`s0)"))
  (traverse munchExp [e1, e2]) (pure []) (pure Nothing)
munchStm (MoveStm (MemExp (ConstExp i)) e2) = emit =<< liftA3
  (OperInstr ("sw `s0, " ++ show i ++ "($zero)"))
  (traverse munchExp [e2]) (pure []) (pure Nothing)
munchStm (MoveStm (MemExp e1) e2) = emit =<< liftA3
  (OperInstr "sw `s1, 0(`s0)")
  (traverse munchExp [e1, e2]) (pure []) (pure Nothing)
munchStm (MoveStm (TempExp i) e2) = emit =<< liftA3
  (OperInstr "move `d0, `s0")
  (traverse munchExp [e2]) (pure [i]) (pure Nothing)
munchStm (ExpStm (CallExp e args)) = emit =<< liftA3
  (OperInstr "jalr `s0")
  (liftA2 (:) (munchExp e) (munchArgs 0 args))
  (asks calldefs) (pure Nothing)
munchStm (LabelStm lab) = emit $ OperInstr (show lab ++ ":") [] [] Nothing
munchStm s = error $ "munchStm: unknown statement " ++ show s

munchArgs :: Int -> [Exp] -> m [Temp]
munchArgs = undefined

munchExp :: (MonadTemp m, MonadPut Instr m, F.Frame frame)
         => Exp -> ReaderT frame m Temp
munchExp (MemExp (BinOpExp Plus e1 (ConstExp i))) = result $ \r ->
  emit =<< liftA3 (OperInstr ("lw `d0, " ++ show i ++ "(`s0)"))
    (traverse munchExp [e1]) (pure [r]) (pure Nothing)
munchExp (MemExp (BinOpExp Plus (ConstExp i) e1)) = result $ \r ->
  emit =<< liftA3 (OperInstr ("lw `d0, " ++ show i ++ "(`s0)"))
    (traverse munchExp [e1]) (pure [r]) (pure Nothing)
munchExp (MemExp (ConstExp i)) = result $ \r ->
  emit $ OperInstr ("lw `d0, " ++ show i ++ "($zero)") [] [r] Nothing
munchExp (MemExp e) = result $ \r ->
  emit =<< liftA3 (OperInstr "lw `d0, 0(`s0)")
    (traverse munchExp [e]) (pure [r]) (pure Nothing)
munchExp (BinOpExp Plus e1 (ConstExp i)) = result $ \r ->
  emit =<< liftA3 (OperInstr ("addi `d0, `s0, " ++ show i))
    (traverse munchExp [e1]) (pure [r]) (pure Nothing)
munchExp (BinOpExp Plus (ConstExp i) e1) = result $ \r ->
  emit =<< liftA3 (OperInstr ("addi `d0, `s0, " ++ show i))
    (traverse munchExp [e1]) (pure [r]) (pure Nothing)
munchExp (BinOpExp Plus e1 e2) = result $ \r ->
  emit =<< liftA3 (OperInstr "add `d0, `s0, `s1")
    (traverse munchExp [e1, e2]) (pure [r]) (pure Nothing)
munchExp (ConstExp i) = result $ \r ->
  emit $ OperInstr ("li `d0, " ++ show i) [] [r] Nothing
munchExp (TempExp t) = pure t
munchExp e = error $ "munchExp: unknown expression " ++ show e

data GenState = GenState
  { _symGen   :: Gen
  , _tmpGen   :: IO Temp
  , instrList :: IORef [Instr]
  }

newtype MipsGen a = MipsGen (GenState -> IO a)
  deriving (Functor, Applicative, Monad) via ReaderT GenState IO
  deriving (MonadSymbol, MonadTemp) via FromRecord "_symGen" "_tmpGen" GenState

instance MonadPut Instr MipsGen where
  put f = MipsGen $ flip modifyIORef' (f :) . instrList

instance MonadCodegen MipsGen where
  type Frame' MipsGen = MipsFrame
  codegen f s = runReaderT (munchStm s) f >>
    MipsGen (fmap reverse . readIORef . instrList)

runMipsGen :: Gen -> IO Temp -> MipsGen a -> IO a
runMipsGen sym tmp (MipsGen f) = newIORef [] >>= f . GenState sym tmp