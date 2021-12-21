{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Tiger.MipsGen where

import Control.Applicative (liftA2, liftA3)
import Control.Monad (void)
import Control.Monad.Reader (ReaderT (..), MonadTrans (lift), asks)
import Data.IORef
import Tiger.Codegen
import Tiger.Symbol (MonadSymbol, SymGen)
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
munchStm (MoveStm (MemExp (BinOpExp Plus (ConstExp i) e1)) e2) =
  munchStm (MoveStm (MemExp (BinOpExp Plus e1 (ConstExp i))) e2)
munchStm (MoveStm (MemExp (ConstExp i)) e2) = emit =<< liftA3
  (OperInstr ("sw `s0, " ++ show i ++ "($zero)"))
  (traverse munchExp [e2]) (pure []) (pure Nothing)
munchStm (MoveStm (MemExp e1) e2) = emit =<< liftA3
  (OperInstr "sw `s1, 0(`s0)")
  (traverse munchExp [e1, e2]) (pure []) (pure Nothing)
munchStm (MoveStm (TempExp r) (ConstExp i)) =
  emit $ OperInstr ("li `d0, " ++ show i) [] [r] Nothing
munchStm (MoveStm (TempExp r) (MemExp (BinOpExp Plus e1 (ConstExp i)))) =
  emit =<< liftA3 (OperInstr ("lw `d0, " ++ show i ++ "(`s0)"))
    (traverse munchExp [e1]) (pure [r]) (pure Nothing)
munchStm (MoveStm (TempExp r) (MemExp (BinOpExp Plus (ConstExp i) e1))) =
  munchStm (MoveStm (TempExp r) (MemExp (BinOpExp Plus e1 (ConstExp i))))
munchStm (MoveStm (TempExp r) e2) = emit =<< liftA2
  (MoveInstr "move `d0, `s0") (munchExp e2) (pure r)
munchStm s@(MoveStm _ _) = error $ "munchStm: unknown move statement " ++ show s
munchStm (JumpStm (NameExp lab) _) = emit $ OperInstr "j `j0" [] [] (Just [lab])
munchStm (JumpStm e labs) = emit =<< liftA3
  (OperInstr "jr `s0")
  (traverse munchExp [e]) (pure []) (pure (Just labs))
munchStm (CJumpStm Eq e1 e2 t f) = emit =<< liftA3
  (OperInstr "beq `s0, `s1, `j0")
  (traverse munchExp [e1, e2]) (pure []) (pure (Just [t, f]))
munchStm (CJumpStm Ne e1 e2 t f) = emit =<< liftA3
  (OperInstr "bne `s0, `s1, `j0")
  (traverse munchExp [e1, e2]) (pure []) (pure (Just [t, f]))
munchStm (CJumpStm Ge e1 (ConstExp 0) t f) = emit =<< liftA3
  (OperInstr "bgez `s0, `j0")
  (traverse munchExp [e1]) (pure []) (pure (Just [t, f]))
munchStm (CJumpStm Ge e1 e2 t f) = emit =<< liftA3
  (OperInstr "bge `s0, `s1, `j0")
  (traverse munchExp [e1, e2]) (pure []) (pure (Just [t, f]))
munchStm (CJumpStm Gt e1 (ConstExp 0) t f) = emit =<< liftA3
  (OperInstr "bgtz `s0, `j0")
  (traverse munchExp [e1]) (pure []) (pure (Just [t, f]))
munchStm (CJumpStm Gt e1 e2 t f) = emit =<< liftA3
  (OperInstr "bgt `s0, `s1, `j0")
  (traverse munchExp [e1, e2]) (pure []) (pure (Just [t, f]))
munchStm (CJumpStm Le e1 (ConstExp 0) t f) = emit =<< liftA3
  (OperInstr "blez `s0, `j0")
  (traverse munchExp [e1]) (pure []) (pure (Just [t, f]))
munchStm (CJumpStm Le e1 e2 t f) = emit =<< liftA3
  (OperInstr "ble `s0, `s1, `j0")
  (traverse munchExp [e1, e2]) (pure []) (pure (Just [t, f]))
munchStm (CJumpStm Lt e1 e2 t f) = emit =<< liftA3
  (OperInstr "blt `s0, `s1, `j0")
  (traverse munchExp [e1, e2]) (pure []) (pure (Just [t, f]))
munchStm (CJumpStm Ult e1 e2 t f) = emit =<< liftA3
  (OperInstr "bltu `s0, `s1, `j0")
  (traverse munchExp [e1, e2]) (pure []) (pure (Just [t, f]))
munchStm (CJumpStm Ule e1 e2 t f) = emit =<< liftA3
  (OperInstr "bleu `s0, `s1, `j0")
  (traverse munchExp [e1, e2]) (pure []) (pure (Just [t, f]))
munchStm (CJumpStm Ugt e1 e2 t f) = emit =<< liftA3
  (OperInstr "bgtu `s0, `s1, `j0")
  (traverse munchExp [e1, e2]) (pure []) (pure (Just [t, f]))
munchStm (CJumpStm Uge e1 e2 t f) = emit =<< liftA3
  (OperInstr "bgeu `s0, `s1, `j0")
  (traverse munchExp [e1, e2]) (pure []) (pure (Just [t, f]))
munchStm (ExpStm (CallExp e args)) = emit =<< liftA3
  (OperInstr "jalr `s0")
  (liftA2 (:) (munchExp e) (munchArgs 0 args))
  (asks calldefs) (pure Nothing)
munchStm (ExpStm e) = void $ munchExp e
munchStm (LabelStm lab) = emit $ LabelInstr (show lab ++ ":") lab

munchArgs :: (MonadTemp m, MonadPut Instr m, F.Frame frame)
          => Int -> [Exp] -> ReaderT frame m [Temp]
munchArgs n (e:es) = do
  args <- asks F.argRegs
  if n < length args
    then do
      let dst = args !! n
      src <- munchExp e
      munchStm $ MoveStm (TempExp dst) (TempExp src)
      (dst :) <$> munchArgs (n + 1) es
    else error "munchArgs: too many arguments"
munchArgs _ [] = pure []

munchExp :: (MonadTemp m, MonadPut Instr m, F.Frame frame)
         => Exp -> ReaderT frame m Temp
munchExp (MemExp (BinOpExp Plus e1 (ConstExp i))) = result $ \r ->
  emit =<< liftA3 (OperInstr ("lw `d0, " ++ show i ++ "(`s0)"))
    (traverse munchExp [e1]) (pure [r]) (pure Nothing)
munchExp (MemExp (BinOpExp Plus (ConstExp i) e1)) =
  munchExp (MemExp (BinOpExp Plus e1 (ConstExp i)))
munchExp (MemExp (ConstExp i)) = result $ \r ->
  emit $ OperInstr ("lw `d0, " ++ show i ++ "($zero)") [] [r] Nothing
munchExp (MemExp e) = result $ \r ->
  emit =<< liftA3 (OperInstr "lw `d0, 0(`s0)")
    (traverse munchExp [e]) (pure [r]) (pure Nothing)

munchExp (BinOpExp Plus e1 (ConstExp i)) = result $ \r ->
  emit =<< liftA3 (OperInstr ("addi `d0, `s0, " ++ show i))
    (traverse munchExp [e1]) (pure [r]) (pure Nothing)
munchExp (BinOpExp Plus (ConstExp i) e1) =
  munchExp (BinOpExp Plus e1 (ConstExp i))
munchExp (BinOpExp Plus e1 e2) = result $ \r ->
  emit =<< liftA3 (OperInstr "add `d0, `s0, `s1")
    (traverse munchExp [e1, e2]) (pure [r]) (pure Nothing)

munchExp (BinOpExp Minus e1 (ConstExp i)) =
  munchExp (BinOpExp Plus e1 (ConstExp (-i)))
munchExp (BinOpExp Minus e1 e2) = result $ \r ->
  emit =<< liftA3 (OperInstr "sub `d0, `s0, `s1")
    (traverse munchExp [e1, e2]) (pure [r]) (pure Nothing)

munchExp (BinOpExp Mul e1 e2) = result $ \r ->
  emit =<< liftA3 (OperInstr "mult `d0, `s0, `s1")
    (traverse munchExp [e1, e2]) (pure [r]) (pure Nothing)
munchExp (BinOpExp Div e1 e2) = result $ \r ->
  emit =<< liftA3 (OperInstr "div `d0, `s0, `s1")
    (traverse munchExp [e1, e2]) (pure [r]) (pure Nothing)

munchExp (BinOpExp And e1 (ConstExp i)) = result $ \r ->
  emit =<< liftA3 (OperInstr ("andi `d0, `s0, " ++ show i))
    (traverse munchExp [e1]) (pure [r]) (pure Nothing)
munchExp (BinOpExp And (ConstExp i) e1) =
  munchExp (BinOpExp And e1 (ConstExp i))
munchExp (BinOpExp And e1 e2) = result $ \r ->
  emit =<< liftA3 (OperInstr "and `d0, `s0, `s1")
    (traverse munchExp [e1, e2]) (pure [r]) (pure Nothing)

munchExp (BinOpExp Or e1 (ConstExp i)) = result $ \r ->
  emit =<< liftA3 (OperInstr ("ori `d0, `s0, " ++ show i))
    (traverse munchExp [e1]) (pure [r]) (pure Nothing)
munchExp (BinOpExp Or (ConstExp i) e1) = munchExp (BinOpExp Or e1 (ConstExp i))
munchExp (BinOpExp Or e1 e2) = result $ \r ->
  emit =<< liftA3 (OperInstr "or `d0, `s0, `s1")
    (traverse munchExp [e1, e2]) (pure [r]) (pure Nothing)

munchExp (BinOpExp Xor e1 (ConstExp i)) = result $ \r ->
  emit =<< liftA3 (OperInstr ("xori `d0, `s0, " ++ show i))
    (traverse munchExp [e1]) (pure [r]) (pure Nothing)
munchExp (BinOpExp Xor (ConstExp i) e1) =
  munchExp (BinOpExp Xor e1 (ConstExp i))
munchExp (BinOpExp Xor e1 e2) = result $ \r ->
  emit =<< liftA3 (OperInstr "xor `d0, `s0, `s1")
    (traverse munchExp [e1, e2]) (pure [r]) (pure Nothing)

munchExp (BinOpExp Lshift e1 (ConstExp i)) = result $ \r ->
  emit =<< liftA3 (OperInstr ("sll `d0, `s0, " ++ show i))
    (traverse munchExp [e1]) (pure [r]) (pure Nothing)
munchExp (BinOpExp Lshift e1 e2) = result $ \r ->
  emit =<< liftA3 (OperInstr "sllv `d0, `s0, `s1")
    (traverse munchExp [e1, e2]) (pure [r]) (pure Nothing)

munchExp (BinOpExp Rshift e1 (ConstExp i)) = result $ \r ->
  emit =<< liftA3 (OperInstr ("srl `d0, `s0, " ++ show i))
    (traverse munchExp [e1]) (pure [r]) (pure Nothing)
munchExp (BinOpExp Rshift e1 e2) = result $ \r ->
  emit =<< liftA3 (OperInstr "srlv `d0, `s0, `s1")
    (traverse munchExp [e1, e2]) (pure [r]) (pure Nothing)

munchExp (BinOpExp Arshift e1 (ConstExp i)) = result $ \r ->
  emit =<< liftA3 (OperInstr ("sra `d0, `s0, " ++ show i))
    (traverse munchExp [e1]) (pure [r]) (pure Nothing)
munchExp (BinOpExp Arshift e1 e2) = result $ \r ->
  emit =<< liftA3 (OperInstr "srav `d0, `s0, `s1")
    (traverse munchExp [e1, e2]) (pure [r]) (pure Nothing)

munchExp e@(CallExp _ _) = munchStm (ExpStm e) *> asks F.rv
munchExp (ConstExp i) = result $ \r ->
  emit $ OperInstr ("li `d0, " ++ show i) [] [r] Nothing
munchExp (TempExp t) = pure t
munchExp (NameExp lab) = result $ \r ->
  emit $ OperInstr ("la `d0, " ++ show lab) [] [r] Nothing
munchExp (ESeqExp s e) = munchStm s >> munchExp e

data GenState = GenState
  { _symGen   :: SymGen
  , _tmpGen   :: IO Temp
  , instrList :: IORef [Instr]
  }

newtype Gen a = Gen (GenState -> IO a)
  deriving (Functor, Applicative, Monad) via ReaderT GenState IO
  deriving (MonadSymbol, MonadTemp) via FromRecord "_symGen" "_tmpGen" GenState

instance MonadPut Instr Gen where
  put f = Gen $ flip modifyIORef' (f :) . instrList

codegen :: F.Frame frame => frame -> Stm -> Gen [Instr]
codegen f s = runReaderT (munchStm s) f >>
  Gen (fmap reverse . readIORef . instrList)

runGen :: Gen a -> GenState -> IO a
runGen (Gen f) s = newIORef [] >>= \i -> f s{instrList = i}