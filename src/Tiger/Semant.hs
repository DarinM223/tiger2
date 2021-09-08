{-# LANGUAGE TupleSections #-}
module Tiger.Semant where

import Prelude hiding (exp, init)
import Control.Monad
import Data.Foldable (foldl', foldlM, traverse_)
import Data.List (sortOn)
import Tiger.AST
import Tiger.Symbol (Symbol, symbolId)
import Tiger.Tc
import Tiger.Types (TEnv, VEnv, ExpTy, adjustEnv, insertEnv, lookupEnv)
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

checkTy :: Types.Ty -> Types.Ty -> Bool
checkTy a b = actualTy a == actualTy b

lookupFieldTy :: TEnv -> TyField -> Tc Types.Ty
lookupFieldTy tenv (TyField pos _ s) = lookupTy pos tenv s

lookupTy :: Show a => a -> TEnv -> Symbol -> Tc Types.Ty
lookupTy pos tenv s = case lookupEnv s tenv of
  Just ty -> pure ty
  Nothing -> do
    compileError $ "Error (" ++ show pos
                ++ "): Cannot find type with symbol " ++ show s
    pure Types.IntTy

breakId :: String
breakId = "123*break"

transVar :: VEnv -> TEnv -> Var -> Tc ExpTy
transVar venv tenv = trVar
 where
  trVar (Var s) = case lookupEnv s venv of
    Just (Types.VarEntry ty) -> pure ((), actualTy ty)
    _ -> do
      compileError $ "Var " ++ show s ++ " is not in the environment"
      pure ((), Types.IntTy)
  trVar (RecField pos var sym) = do
    (_, varTy) <- trVar var
    case varTy of
      Types.RecordTy fields _ | Just fieldTy <- lookup sym fields ->
        pure ((), fieldTy)
      _ -> do
        compileError $ "Error (" ++ show pos ++ "): record field " ++ show sym
                    ++ " is not in " ++ show varTy
        pure ((), Types.IntTy)
  trVar (ArraySub pos var index) = do
    (_, varTy) <- trVar var
    case varTy of
      Types.ArrayTy elemTy _ -> do
        (_, indexTy) <- transExp venv tenv index
        unless (indexTy == Types.IntTy) $
          compileError $ "Error (" ++ show pos
                      ++ "): array index must have an integer type"
        pure ((), elemTy)
      _ -> do
        compileError $ "Error (" ++ show pos
                    ++ "): attempting to index a value of type " ++ show varTy
        pure ((), Types.IntTy)

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
  trExp (FuncallExp pos name exps) = case lookupEnv name venv of
    Just (Types.FunEntry tys retTy) -> do
      tys' <- fmap snd <$> traverse trExp exps
      unless (tys == tys') $
        compileError $ "Error (" ++ show pos ++ "): Function " ++ show name
                    ++ "'s parameter type list " ++ show tys
                    ++ " is different from " ++ show tys'
      pure ((), retTy)
    _ -> do
      compileError $ "Error (" ++ show pos ++ "): Function " ++ show name
                  ++ " is not in the environment"
      pure ((), Types.IntTy)
  trExp (RecordExp pos name fields) = case lookupEnv name tenv of
    Just recTy@(Types.RecordTy fieldTys _) -> do
      let fieldTys' = sortOn fst fieldTys
      tys <- sortOn fst <$> trFields
      unless (fieldTys' == tys) $
        compileError $ "Error (" ++ show pos ++ "): Record " ++ show name
                    ++ "'s fields " ++ show fieldTys'
                    ++ " is different from " ++ show tys
      pure ((), recTy)
    _ -> do
      compileError $ "Error (" ++ show pos ++ "): Record " ++ show name
                  ++ " is not in the environment"
      (() ,) <$> (Types.RecordTy <$> trFields <*> unique)
   where
    trFields = traverse (\(sym, e) -> (sym ,) <$> fmap snd (trExp e))
             $ fmap (\(_, sym, e) -> (sym, e)) fields
  trExp (ArrayExp pos name numExp valueExp) = case lookupEnv name tenv of
    Just arrTy@(Types.ArrayTy valueTy _) -> do
      (_, numTy) <- trExp numExp
      unless (numTy == Types.IntTy) $
        compileError $ "Error (" ++ show pos ++ "): Array " ++ show name
                    ++ " must have an integer size"
      (_, valueTy') <- trExp valueExp
      unless (valueTy == valueTy') $
        compileError $ "Error (" ++ show pos ++ "): Expected array type "
                    ++ show valueTy ++ " got " ++ show valueTy'
      pure ((), arrTy)
    _ -> do
      compileError $ "Error (" ++ show pos ++ "): Array " ++ show name
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
    i <- symbolId <$> symbol breakId
    transExp venv (IM.insert i Types.UnitTy tenv) body
  trExp (LetExp _ decs body) = do
    (venv', tenv') <- foldlM (uncurry transDec) (venv, tenv) decs
    transExp venv' tenv' body
  trExp (ForExp pos sym start end body) = trExp loopExp
   where
    loopExp = LetExp pos [startDec] (WhileExp pos testExp body)
    startDec = VarDec $ VarDec' pos sym Nothing start
    testExp = OpExp pos LtOp (VarExp (Var sym)) end
  trExp (BreakExp pos) = do
    i <- symbolId <$> symbol breakId
    unless (IM.member i tenv) $
      compileError $ "Error (" ++ show pos
                  ++ "): break must be within a while or for loop"
    pure ((), Types.UnitTy)

transDec :: VEnv -> TEnv -> Dec -> Tc (VEnv, TEnv)
transDec venv tenv0 (TyDecs decs) = do
  let tenv' = foldl' (flip insertHeader) tenv0 decs
  tenv'' <- foldlM setTy tenv' decs
  traverse_ (checkCycles tenv'') decs
  pure (venv, tenv'')
 where
  insertHeader (TyDec _ n _) = insertEnv n (Types.NameTy n Nothing)
  setTy tenv (TyDec _ n ty) = fmap
    (\ty' -> adjustEnv (\_ -> Types.NameTy n (Just ty')) n tenv)
    (transTy tenv ty)
  checkCycles tenv0' (TyDec pos0 s0 _) = go pos0 [] tenv0' s0
   where
    go pos seen _ s | s `elem` seen =
      compileError $ "Error (" ++ show pos
                  ++ "): type declaration cycle detected: "
                  ++ show (s:seen)
    go pos seen tenv s = case lookupEnv s tenv of
      Just (Types.NameTy _ (Just (Types.NameTy s' _))) ->
        go pos (s:seen) tenv s'
      _ -> pure ()
transDec venv tenv (VarDec (VarDec' pos name tyMaybe init)) = do
  (_, initTy) <- transExp venv tenv init
  case tyMaybe of
    -- TODO(DarinM223): compile error if tyI does not exist in tenv
    Just s | Just ty <- lookupEnv s tenv, initTy /= ty ->
      compileError $ "Error (" ++ show pos ++ "): expected type " ++ show ty
                  ++ " got " ++ show initTy
    _ -> pure ()
  let venv' = insertEnv name (Types.VarEntry initTy) venv
  pure (venv', tenv)
transDec venv0 tenv (FunDecs decs) = do
  venv' <- foldlM insertHeader venv0 decs
  traverse_ (check venv') decs
  pure (venv', tenv)
 where
  insertHeader venv dec@FunDec{ funDecName = name } =
    fmap (\h -> insertEnv name h venv) (header dec)
  check venv (FunDec pos funName fields _ body) = do
    (_, bodyTy) <- transExp venv' tenv body
    unless (bodyTy == retTy) $
      compileError $ "Error (" ++ show pos
                  ++ "): expected function return type " ++ show retTy
                  ++ " got " ++ show bodyTy
   where
    venv' = foldl'
      (flip (\(name, ty) -> insertEnv name (Types.VarEntry ty)))
      venv
      (zip (fmap (\(TyField _ name _) -> name) fields) fieldTys)
    (fieldTys, retTy) = case lookupEnv funName venv of
      Just (Types.FunEntry fs r) -> (fs, r)
      _ -> error "Function header not in environment"
  header (FunDec pos _ fields retMaybe _) = Types.FunEntry
    <$> traverse (fmap actualTy . lookupFieldTy tenv) fields
    <*> maybe (pure Types.UnitTy) (fmap actualTy . lookupTy pos tenv) retMaybe

transTy :: TEnv -> Ty -> Tc Types.Ty
transTy tenv (IdTy s) = lookupTy "" tenv s
transTy tenv (FieldsTy _ fields) =
  -- TODO(DarinM223): check for duplicates
  Types.RecordTy <$> traverse convertField fields <*> unique
 where
  convertField field@(TyField _ name _) = (name ,) <$> lookupFieldTy tenv field
transTy tenv (ArrayOfTy pos s) =
  Types.ArrayTy <$> lookupTy pos tenv s <*> unique