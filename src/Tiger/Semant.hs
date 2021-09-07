{-# LANGUAGE TupleSections #-}
module Tiger.Semant where

import Prelude hiding (exp)
import Control.Monad
import Data.Foldable (foldlM)
import Data.List (sortOn)
import Tiger.AST
import Tiger.Tc
import Tiger.Types (TEnv, VEnv, ExpTy)
import qualified Data.IntMap as IM
import qualified Tiger.Types as Types

data OpType = Equality | Comparison | Arithmetic
  deriving (Eq, Show)

opType :: Op -> OpType
opType op = case op of
  { AddOp -> Arithmetic; SubOp -> Arithmetic
  ; MulOp -> Arithmetic; DivOp -> Arithmetic
  ; EqOp -> Equality; NeqOp -> Equality
  ; GtOp -> Comparison; LtOp -> Comparison
  ; GteOp -> Comparison; LteOp -> Comparison
  ; AndOp -> Arithmetic; OrOp -> Arithmetic
  }

checkOpType :: OpType -> Types.Ty -> Bool
checkOpType Arithmetic ty = ty == Types.IntTy
checkOpType Comparison ty = ty == Types.IntTy || ty == Types.StringTy
checkOpType Equality ty = case ty of
  Types.IntTy        -> True
  Types.StringTy     -> True
  Types.RecordTy _ _ -> True
  Types.ArrayTy _ _  -> True
  Types.NilTy        -> True
  _                  -> False

actualTy :: Types.Ty -> Types.Ty
actualTy (Types.NameTy _ (Just ty)) = actualTy ty
actualTy ty                         = ty

breakId :: String
breakId = "123*break"

transVar :: VEnv -> TEnv -> Var -> Tc ExpTy
transVar venv tenv = trVar
 where
  trVar (Var (s, i)) = case IM.lookup i venv of
    Just (Types.VarEntry ty) -> pure ((), actualTy ty)
    _ -> do
      compileError $ "Var " ++ show s ++ " is not in the environment"
      pure ((), Types.IntTy)
  trVar _ = undefined

transExp :: VEnv -> TEnv -> Exp -> Tc ExpTy
transExp venv tenv = trExp
 where
  trExp (VarExp var) = transVar venv tenv var
  trExp (NilExp _) = pure ((), Types.NilTy)
  trExp (SeqExp _ exps) = foldlM (const trExp) ((), Types.UnitTy) exps
  trExp (IntExp _) = pure ((), Types.IntTy)
  trExp (StringExp _) = pure ((), Types.StringTy)
  trExp (OpExp pos op exp1 exp2) = do
    (_, ty) <- trExp exp1
    unless (checkOpType opType' ty) $
      compileError $ "Error (" ++ show pos ++ "): Invalid left type "
                  ++ show ty ++ " for operator type " ++ show opType'
    (_, ty') <- trExp exp2
    unless (checkOpType opType' ty') $
      compileError $ "Error (" ++ show pos ++ "): Invalid right type "
                  ++ show ty' ++ " for operator type " ++ show opType'
    pure ((), Types.IntTy)
   where opType' = opType op
  trExp (FuncallExp pos (s, i) exps) = case IM.lookup i venv of
    Just (Types.FunEntry tys retTy) -> do
      tys' <- fmap snd <$> traverse trExp exps
      unless (tys == tys') $
        compileError $ "Error (" ++ show pos ++ "): Function " ++ s
                    ++ "'s parameter type list " ++ show tys
                    ++ " is different from " ++ show tys'
      pure ((), retTy)
    _ -> do
      compileError $ "Error (" ++ show pos ++ "): Function " ++ s
                  ++ " is not in the environment"
      pure ((), Types.IntTy)
  trExp (RecordExp pos (s, i) fields) = case IM.lookup i tenv of
    Just recTy@(Types.RecordTy fieldTys _) -> do
      let fieldTys' = sortOn (snd . fst) fieldTys
      tys <- sortOn (snd . fst) <$> trFields
      unless (fieldTys' == tys) $
        compileError $ "Error (" ++ show pos ++ "): Record " ++ s
                    ++ "'s fields " ++ show fieldTys'
                    ++ " is different from " ++ show tys
      pure ((), recTy)
    _ -> do
      compileError $ "Error (" ++ show pos ++ "): Record " ++ s
                  ++ " is not in the environment"
      (() ,) <$> (Types.RecordTy <$> trFields <*> unique)
   where
    trFields = traverse (\(sym, e) -> (sym ,) <$> fmap snd (trExp e))
             $ fmap (\(_, sym, e) -> (sym, e)) fields
  trExp (ArrayExp pos (s, i) numExp valueExp) = case IM.lookup i tenv of
    Just arrTy@(Types.ArrayTy valueTy _) -> do
      (_, numTy) <- trExp numExp
      unless (numTy == Types.IntTy) $
        compileError $ "Error (" ++ show pos ++ "): Array " ++ s
                    ++ " must have an integer size"
      (_, valueTy') <- trExp valueExp
      unless (valueTy == valueTy') $
        compileError $ "Error (" ++ show pos ++ "): Expected array type "
                    ++ show valueTy ++ " got " ++ show valueTy'
      pure ((), arrTy)
    _ -> do
      compileError $ "Error (" ++ show pos ++ "): Array " ++ s
                  ++ " is not in the environment"
      (() ,) <$> (Types.ArrayTy <$> (snd <$> trExp valueExp) <*> unique)
  trExp (AssignExp pos var exp) = do
    (_, varTy) <- transVar venv tenv var
    (_, expTy) <- transExp venv tenv exp
    unless (varTy == expTy) $
      compileError $ "Error (" ++ show pos
                  ++ "): Attempting to assign a value of type " ++ show expTy
                  ++ " to variable of type " ++ show varTy
    pure ((), Types.UnitTy)
  trExp (IfExp pos testExp thenExp elseExpMaybe) = do
    (_, testTy) <- transExp venv tenv testExp
    unless (testTy == Types.IntTy) $
      compileError $ "Error (" ++ show pos
                  ++ "): If test expression must have integer type"
    (_, thenTy) <- transExp venv tenv thenExp
    case elseExpMaybe of
      Just elseExp -> do
        (_, elseTy) <- transExp venv tenv elseExp
        unless (thenTy == elseTy) $
          compileError $ "Error (" ++ show pos
                      ++ "): Expected else branch type " ++ show thenTy
                      ++ " got " ++ show elseTy
        pure ((), thenTy)
      Nothing -> pure ((), thenTy)
  trExp (WhileExp pos test body) = do
    (_, testTy) <- transExp venv tenv test
    unless (testTy == Types.IntTy) $
      compileError $ "Error (" ++ show pos
                  ++ "): While test expression must have integer type"
    (_, i) <- symbol breakId
    transExp venv (IM.insert i Types.UnitTy tenv) body
  trExp (LetExp _ decs body) = do
    (venv', tenv') <- foldlM (uncurry transDec) (venv, tenv) decs
    transExp venv' tenv' body
  trExp (ForExp pos sym start end body) = trExp loopExp
   where
    loopExp = LetExp pos [startDec] (WhileExp pos testExp body)
    startDec = VarDec pos sym Nothing start
    testExp = OpExp pos LtOp (VarExp (Var sym)) end
  trExp (BreakExp pos) = do
    (_, i) <- symbol breakId
    unless (IM.member i tenv) $
      compileError $ "Error (" ++ show pos
                  ++ "): break must be within a while or for loop"
    pure ((), Types.UnitTy)

transDec :: VEnv -> TEnv -> Dec -> Tc (VEnv, TEnv)
transDec = undefined

transTy :: TEnv -> Ty -> Types.Ty
transTy = undefined