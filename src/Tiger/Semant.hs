{-# LANGUAGE TupleSections #-}
module Tiger.Semant where

import Prelude hiding (exp, init)
import Control.Monad
import Data.Bifunctor (second)
import Data.Foldable (foldl', foldlM, for_, traverse_)
import Data.List (sortOn)
import Tiger.AST
import Tiger.Symbol (Symbol, symbolId)
import Tiger.Tc
import Tiger.Temp (newLabel)
import Tiger.Types hiding (Ty (..), EnvEntry (..))
import qualified Data.IntSet as IS
import qualified Tiger.Translate as T
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

lookupFieldTy :: MonadCheck m => TEnv -> TyField Bool -> m Types.Ty
lookupFieldTy tenv (TyField pos _ s _) = lookupTy pos tenv s

lookupTy :: (MonadCheck m, Show a) => a -> TEnv -> Symbol -> m Types.Ty
lookupTy pos tenv s = case lookupEnv s tenv of
  Just ty -> pure ty
  Nothing -> do
    compileError $ "Error (" ++ show pos
                ++ "): Cannot find type with symbol " ++ show s
    pure Types.IntTy

checkDup :: MonadCheck m => [(Symbol, Pos)] -> m ()
checkDup = void . foldlM go IS.empty
 where
  go set (s, pos)
    | IS.member symId set = do
      compileError $ "Error (" ++ show pos ++ "): duplicate symbol " ++ show s
      pure set
    | otherwise = pure $ IS.insert symId set
   where symId = symbolId s

breakId :: String
breakId = "123*break"

transVar :: MonadTc level m => level -> VEnv level -> TEnv -> Var -> m ExpTy
transVar level venv tenv = trVar
 where
  trVar (Var s) = case lookupEnv s venv of
    Just (Types.VarEntry access ty) ->
      pure (T.simpleVar access level, actualTy ty)
    _ -> do
      compileError $ "Var " ++ show s ++ " is not in the environment"
      pure (T.unit, Types.IntTy)
  trVar (RecField pos var sym) = do
    (exp, varTy) <- trVar var
    case actualTy varTy of
      Types.RecordTy fields _ | Just fieldTy <- lookup sym fields ->
        (, fieldTy) <$> T.fieldVar exp sym (fmap fst fields)
      _ -> do
        compileError $ "Error (" ++ show pos ++ "): record field " ++ show sym
                    ++ " is not in " ++ show varTy
        pure (exp, Types.IntTy)
  trVar (ArraySub pos var index) = do
    (exp1, varTy) <- trVar var
    case actualTy varTy of
      Types.ArrayTy elemTy _ -> do
        (exp2, indexTy) <- transExp level venv tenv index
        unless (indexTy == Types.IntTy) $
          compileError $ "Error (" ++ show pos
                      ++ "): array index must have an integer type"
        (, elemTy) <$> T.subscriptVar exp1 exp2
      _ -> do
        compileError $ "Error (" ++ show pos
                    ++ "): attempting to index a value of type " ++ show varTy
        pure (T.unit, Types.IntTy)

transExp :: MonadTc level m => level -> VEnv level -> TEnv -> Exp -> m ExpTy
transExp level venv tenv = trExp
 where
  trExp (VarExp var) = transVar level venv tenv var
  trExp (NilExp _) = pure (T.unit, Types.NilTy)
  trExp (SeqExp _ exps) = do
    results <- traverse trExp exps
    let lastTy = foldl' (const snd) Types.UnitTy results
    (, lastTy) <$> T.seqExp (fmap fst results)
  trExp (IntExp i) = (, Types.IntTy) <$> T.intExp i
  trExp (StringExp s) = (, Types.StringTy) <$> T.stringExp s
  trExp (OpExp pos op exp1 exp2) = do
    (_, ty) <- trExp exp1
    unless (checkOpType opType' ty) $
      compileError $ "Error (" ++ show pos ++ "): Invalid left type "
                  ++ show ty ++ " for operator type " ++ show opType'
    (_, ty') <- trExp exp2
    unless (checkOpType opType' ty') $
      compileError $ "Error (" ++ show pos ++ "): Invalid right type "
                  ++ show ty' ++ " for operator type " ++ show opType'
    unless (ty == ty') $
      compileError $ "Error (" ++ show pos ++ "): expected type "
                  ++ show ty ++ " got " ++ show ty'
    pure (T.unit, Types.IntTy)
   where opType' = opType op
  trExp (FuncallExp pos name exps) = case lookupEnv name venv of
    Just (Types.FunEntry levelFun tys retTy) -> do
      results <- traverse trExp exps
      let tys' = fmap snd results
      unless (tys == tys') $
        compileError $ "Error (" ++ show pos ++ "): Function " ++ show name
                    ++ "'s parameter type list " ++ show tys
                    ++ " is different from " ++ show tys'
      exp <- T.funCallExp levelFun level name $ fmap fst results
      pure (exp, retTy)
    _ -> do
      compileError $ "Error (" ++ show pos ++ "): Function " ++ show name
                  ++ " is not in the environment"
      pure (T.unit, Types.IntTy)
  trExp (RecordExp pos name fields) = do
    checkDup $ fmap (\(p, n, _) -> (n, p)) fields
    case lookupType name tenv of
      Just recTy@(Types.RecordTy fieldTys _) -> do
        let fieldTys' = sortOn fst fieldTys
        results <- sortOn fst <$> trFields fields
        let exps = fmap (fst . snd) results
            tys  = fmap (second snd) results
        unless (fieldTys' == tys) $
          compileError $ "Error (" ++ show pos ++ "): Record " ++ show name
                      ++ "'s fields " ++ show fieldTys'
                      ++ " is different from " ++ show tys
        (, recTy) <$> T.recordExp exps
      _ -> do
        compileError $ "Error (" ++ show pos ++ "): Record " ++ show name
                    ++ " is not in the environment"
        results <- trFields fields
        (T.unit ,) . Types.RecordTy (fmap (second snd) results) <$> unique
   where
    trFields = traverse (\(sym, e) -> (sym ,) <$> trExp e)
             . fmap (\(_, sym, e) -> (sym, e))
  trExp (ArrayExp pos name numExp valueExp) = case lookupType name tenv of
    Just arrTy@(Types.ArrayTy valueTy _) -> do
      (exp1, numTy) <- trExp numExp
      unless (numTy == Types.IntTy) $
        compileError $ "Error (" ++ show pos ++ "): Array " ++ show name
                    ++ " must have an integer size"
      (exp2, valueTy') <- trExp valueExp
      unless (actualTy valueTy == valueTy') $
        compileError $ "Error (" ++ show pos ++ "): Expected array type "
                    ++ show valueTy ++ " got " ++ show valueTy'
      (, arrTy) <$> T.arrayExp exp1 exp2
    _ -> do
      compileError $ "Error (" ++ show pos ++ "): Array " ++ show name
                  ++ " is not in the environment"
      (T.unit ,) <$> (Types.ArrayTy <$> (snd <$> trExp valueExp) <*> unique)
  trExp (AssignExp pos var exp) = do
    (exp1, varTy) <- transVar level venv tenv var
    (exp2, expTy) <- trExp exp
    unless (varTy == expTy) $
      compileError $ "Error (" ++ show pos
                  ++ "): Attempting to assign a value of type " ++ show expTy
                  ++ " to variable of type " ++ show varTy
    (, Types.UnitTy) <$> T.assignExp exp1 exp2
  trExp (IfExp pos testExp thenExp elseExp) = do
    (exp1, testTy) <- trExp testExp
    unless (testTy == Types.IntTy) $
      compileError $ "Error (" ++ show pos
                  ++ "): If test expression must have integer type"
    (exp2, thenTy) <- trExp thenExp
    elseResult <- traverse trExp elseExp
    let elseTy = maybe Types.UnitTy snd elseResult
    unless (thenTy == elseTy) $
      compileError $ "Error (" ++ show pos
                  ++ "): Expected branch type " ++ show elseTy
                  ++ " got " ++ show thenTy
    (, thenTy) <$> T.ifElseExp exp1 exp2 (fmap fst elseResult)
  trExp (WhileExp pos test body) = do
    (exp1, testTy) <- trExp test
    unless (testTy == Types.IntTy) $
      compileError $ "Error (" ++ show pos
                  ++ "): While test expression must have integer type"
    s <- symbol breakId
    done <- newLabel
    let tenv' = insertEnv s (Types.NameTy done Nothing) tenv
    (exp2, bodyTy) <- transExp level venv tenv' body
    unless (bodyTy == Types.UnitTy) $
      compileError $ "Error (" ++ show pos
                  ++ "): While body expression must have unit type"
    (, Types.UnitTy) <$> T.whileExp exp1 exp2 done
  trExp (LetExp _ decs body) = do
    (venv', tenv', stms) <- foldlM
      (\(venv', tenv', exps) ->
        fmap (fmap (exps ++)) . transDec level venv' tenv')
      (venv, tenv, [])
      decs
    (exp, ty) <- transExp level venv' tenv' body
    (, ty) <$> T.letExp stms exp
  trExp (ForExp pos sym start end body esc) = trExp loopExp
   where
    loopExp = LetExp pos [startDec] (WhileExp pos testExp body)
    startDec = VarDec $ VarDec' pos sym Nothing start esc
    testExp = OpExp pos LtOp (VarExp (Var sym)) end
  trExp (BreakExp pos) = do
    s <- symbol breakId
    case lookupEnv s tenv of
      Just (Types.NameTy done _) -> (, Types.UnitTy) <$> T.breakExp done
      _ -> do
        compileError $ "Error (" ++ show pos
                    ++ "): break must be within a while or for loop"
        pure (T.unit, Types.UnitTy)

transDec
  :: MonadTc level m
  => level -> VEnv level -> TEnv -> Dec
  -> m (VEnv level, TEnv, [T.Exp])
transDec _ venv tenv0 (TyDecs decs) = do
  checkDup $ fmap (\dec -> (typeDecName dec, typeDecPos dec)) decs
  let tenv' = foldl' (flip insertHeader) tenv0 decs
  tenv'' <- foldlM setTy tenv' decs
  tenv''' <- foldlM checkCycles tenv'' decs
  pure (venv, tenv''', [])
 where
  insertHeader (TyDec _ n _) = insertEnv n (Types.NameTy n Nothing)
  setTy tenv (TyDec _ n ty) = fmap
    (\ty' -> adjustEnv (\_ -> Types.NameTy n (Just ty')) n tenv)
    (transTy tenv ty)
  checkCycles tenv0' (TyDec pos s0 _) = go [] tenv0' s0
   where
    go seen tenv s | s `elem` seen = do
      compileError $ "Error (" ++ show pos
                  ++ "): type declaration cycle detected: "
                  ++ show (s:seen)
      pure tenv
    go seen tenv s = case lookupEnv s tenv of
      Just (Types.NameTy _ (Just (Types.NameTy s' _))) -> go (s:seen) tenv s'
      Just ty -> pure $ insertEnv s0 ty tenv
      _ -> pure tenv
transDec level venv tenv (VarDec (VarDec' pos name tyMaybe init esc)) = do
  (exp, initTy) <- transExp level venv tenv init
  access <- T.allocLocal level esc
  for_ tyMaybe $ \s -> case lookupType s tenv of
    Just ty -> unless (initTy == ty) $
      compileError $ "Error (" ++ show pos ++ "): expected type " ++ show ty
                  ++ " got " ++ show initTy
    Nothing ->
      compileError $ "Error (" ++ show pos ++ "): type " ++ show s
                  ++ " not in environment"
  let venv' = insertEnv name (Types.VarEntry access initTy) venv
  exp' <- T.assignExp (T.simpleVar access level) exp
  pure (venv', tenv, [exp'])
transDec level venv0 tenv (FunDecs decs) = do
  checkDup $ fmap (\dec -> (funDecName dec, funDecPos dec)) decs
  venv' <- foldlM insertHeader venv0 decs
  traverse_ (check venv') decs
  pure (venv', tenv, [])
 where
  insertHeader venv dec@FunDec{ funDecName = name } =
    fmap (\h -> insertEnv name h venv) (header dec)
  check venv (FunDec pos funName fields _ body) = do
    checkDup $ fmap (\(TyField p n _ _) -> (n, p)) fields
    (exp, bodyTy) <- transExp level' venv' tenv body
    unless (bodyTy == retTy) $
      compileError $ "Error (" ++ show pos
                  ++ "): expected function return type " ++ show retTy
                  ++ " got " ++ show bodyTy
    T.functionDec level' exp
   where
    venv' = foldl'
      (flip (\(name, ty, acc) -> insertEnv name (Types.VarEntry acc ty)))
      venv
      (zip3 (fmap (\(TyField _ name _ _) -> name) fields)
            fieldTys
            (T.formals level'))
    (level', fieldTys, retTy) = case lookupEnv funName venv of
      Just (Types.FunEntry l fs r) -> (l, fs, r)
      _ -> error "Function header not in environment"
  header (FunDec pos name fields retMaybe _) = Types.FunEntry
    <$> T.newLevel level name (fmap (\(TyField _ _ _ esc) -> esc) fields)
    <*> traverse (fmap actualTy . lookupFieldTy tenv) fields
    <*> maybe (pure Types.UnitTy) (fmap actualTy . lookupTy pos tenv) retMaybe

transTy :: MonadTc level m => TEnv -> Ty -> m Types.Ty
transTy tenv (IdTy s) = lookupTy "" tenv s
transTy tenv (FieldsTy _ fields) = do
  checkDup $ fmap (\(TyField p n _ _) -> (n, p)) fields
  Types.RecordTy <$> traverse convertField fields <*> unique
 where
  convertField field@(TyField _ name _ _) = (name ,) <$> lookupFieldTy tenv field
transTy tenv (ArrayOfTy pos s) =
  Types.ArrayTy <$> lookupTy pos tenv s <*> unique