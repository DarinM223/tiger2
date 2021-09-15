{-# LANGUAGE TupleSections #-}
module Tiger.Semant where

import Prelude hiding (exp, init)
import Control.Monad
import Data.Foldable (foldl', foldlM, for_, traverse_)
import Data.List (sortOn)
import Tiger.AST
import Tiger.Symbol (Symbol, symbolId)
import Tiger.Tc
import Tiger.Translate (Level, MonadTranslate (allocLocal, newLevel), formals)
import Tiger.Types hiding (Ty (..), EnvEntry (..))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
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

transVar :: MonadTc m => Level m -> VEnv (Level m) -> TEnv -> Var -> m ExpTy
transVar level venv tenv = trVar
 where
  trVar (Var s) = case lookupEnv s venv of
    Just (Types.VarEntry _ ty) -> pure ((), actualTy ty)
    _ -> do
      compileError $ "Var " ++ show s ++ " is not in the environment"
      pure ((), Types.IntTy)
  trVar (RecField pos var sym) = do
    (_, varTy) <- trVar var
    case actualTy varTy of
      Types.RecordTy fields _ | Just fieldTy <- lookup sym fields ->
        pure ((), fieldTy)
      _ -> do
        compileError $ "Error (" ++ show pos ++ "): record field " ++ show sym
                    ++ " is not in " ++ show varTy
        pure ((), Types.IntTy)
  trVar (ArraySub pos var index) = do
    (_, varTy) <- trVar var
    case actualTy varTy of
      Types.ArrayTy elemTy _ -> do
        (_, indexTy) <- transExp level venv tenv index
        unless (indexTy == Types.IntTy) $
          compileError $ "Error (" ++ show pos
                      ++ "): array index must have an integer type"
        pure ((), elemTy)
      _ -> do
        compileError $ "Error (" ++ show pos
                    ++ "): attempting to index a value of type " ++ show varTy
        pure ((), Types.IntTy)

transExp :: MonadTc m => Level m -> VEnv (Level m) -> TEnv -> Exp -> m ExpTy
transExp level venv tenv = trExp
 where
  trExp (VarExp var) = transVar level venv tenv var
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
    unless (ty == ty') $
      compileError $ "Error (" ++ show pos ++ "): expected type "
                  ++ show ty ++ " got " ++ show ty'
    pure ((), Types.IntTy)
   where opType' = opType op
  trExp (FuncallExp pos name exps) = case lookupEnv name venv of
    Just (Types.FunEntry _ tys retTy) -> do
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
  trExp (RecordExp pos name fields) = do
    checkDup $ fmap (\(p, n, _) -> (n, p)) fields
    case lookupType name tenv of
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
  trExp (ArrayExp pos name numExp valueExp) = case lookupType name tenv of
    Just arrTy@(Types.ArrayTy valueTy _) -> do
      (_, numTy) <- trExp numExp
      unless (numTy == Types.IntTy) $
        compileError $ "Error (" ++ show pos ++ "): Array " ++ show name
                    ++ " must have an integer size"
      (_, valueTy') <- trExp valueExp
      unless (actualTy valueTy == valueTy') $
        compileError $ "Error (" ++ show pos ++ "): Expected array type "
                    ++ show valueTy ++ " got " ++ show valueTy'
      pure ((), arrTy)
    _ -> do
      compileError $ "Error (" ++ show pos ++ "): Array " ++ show name
                  ++ " is not in the environment"
      (() ,) <$> (Types.ArrayTy <$> (snd <$> trExp valueExp) <*> unique)
  trExp (AssignExp pos var exp) = do
    (_, varTy) <- transVar level venv tenv var
    (_, expTy) <- trExp exp
    unless (varTy == expTy) $
      compileError $ "Error (" ++ show pos
                  ++ "): Attempting to assign a value of type " ++ show expTy
                  ++ " to variable of type " ++ show varTy
    pure ((), Types.UnitTy)
  trExp (IfExp pos testExp thenExp elseExp) = do
    (_, testTy) <- trExp testExp
    unless (testTy == Types.IntTy) $
      compileError $ "Error (" ++ show pos
                  ++ "): If test expression must have integer type"
    (_, thenTy) <- trExp thenExp
    elseTy <- maybe (pure Types.UnitTy) (fmap snd . trExp) elseExp
    unless (thenTy == elseTy) $
      compileError $ "Error (" ++ show pos
                  ++ "): Expected branch type " ++ show elseTy
                  ++ " got " ++ show thenTy
    pure ((), thenTy)
  trExp (WhileExp pos test body) = do
    (_, testTy) <- trExp test
    unless (testTy == Types.IntTy) $
      compileError $ "Error (" ++ show pos
                  ++ "): While test expression must have integer type"
    i <- symbolId <$> symbol breakId
    (_, bodyTy) <- transExp level venv (IM.insert i Types.UnitTy tenv) body
    unless (bodyTy == Types.UnitTy) $
      compileError $ "Error (" ++ show pos
                  ++ "): While body expression must have unit type"
    pure ((), Types.UnitTy)
  trExp (LetExp _ decs body) = do
    (venv', tenv') <- foldlM (uncurry (transDec level)) (venv, tenv) decs
    transExp level venv' tenv' body
  trExp (ForExp pos sym start end body esc) = trExp loopExp
   where
    loopExp = LetExp pos [startDec] (WhileExp pos testExp body)
    startDec = VarDec $ VarDec' pos sym Nothing start esc
    testExp = OpExp pos LtOp (VarExp (Var sym)) end
  trExp (BreakExp pos) = do
    i <- symbolId <$> symbol breakId
    unless (IM.member i tenv) $
      compileError $ "Error (" ++ show pos
                  ++ "): break must be within a while or for loop"
    pure ((), Types.UnitTy)

transDec
  :: MonadTc m
  => Level m -> VEnv (Level m) -> TEnv -> Dec -> m (VEnv (Level m), TEnv)
transDec _ venv tenv0 (TyDecs decs) = do
  checkDup $ fmap (\dec -> (typeDecName dec, typeDecPos dec)) decs
  let tenv' = foldl' (flip insertHeader) tenv0 decs
  tenv'' <- foldlM setTy tenv' decs
  tenv''' <- foldlM checkCycles tenv'' decs
  pure (venv, tenv''')
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
  (_, initTy) <- transExp level venv tenv init
  access <- allocLocal level esc
  for_ tyMaybe $ \s -> case lookupType s tenv of
    Just ty -> unless (initTy == ty) $
      compileError $ "Error (" ++ show pos ++ "): expected type " ++ show ty
                  ++ " got " ++ show initTy
    Nothing ->
      compileError $ "Error (" ++ show pos ++ "): type " ++ show s
                  ++ " not in environment"
  let venv' = insertEnv name (Types.VarEntry access initTy) venv
  pure (venv', tenv)
transDec level venv0 tenv (FunDecs decs) = do
  checkDup $ fmap (\dec -> (funDecName dec, funDecPos dec)) decs
  venv' <- foldlM insertHeader venv0 decs
  traverse_ (check venv') decs
  pure (venv', tenv)
 where
  insertHeader venv dec@FunDec{ funDecName = name } =
    fmap (\h -> insertEnv name h venv) (header dec)
  check venv (FunDec pos funName fields _ body) = do
    checkDup $ fmap (\(TyField p n _ _) -> (n, p)) fields
    (_, bodyTy) <- transExp level' venv' tenv body
    unless (bodyTy == retTy) $
      compileError $ "Error (" ++ show pos
                  ++ "): expected function return type " ++ show retTy
                  ++ " got " ++ show bodyTy
   where
    venv' = foldl'
      (flip (\(name, ty, acc) -> insertEnv name (Types.VarEntry acc ty)))
      venv
      (zip3 (fmap (\(TyField _ name _ _) -> name) fields)
            fieldTys
            (formals level'))
    (level', fieldTys, retTy) = case lookupEnv funName venv of
      Just (Types.FunEntry l fs r) -> (l, fs, r)
      _ -> error "Function header not in environment"
  header (FunDec pos name fields retMaybe _) = Types.FunEntry
    <$> newLevel level name (fmap (\(TyField _ _ _ esc) -> esc) fields)
    <*> traverse (fmap actualTy . lookupFieldTy tenv) fields
    <*> maybe (pure Types.UnitTy) (fmap actualTy . lookupTy pos tenv) retMaybe

transTy :: MonadTc m => TEnv -> Ty -> m Types.Ty
transTy tenv (IdTy s) = lookupTy "" tenv s
transTy tenv (FieldsTy _ fields) = do
  checkDup $ fmap (\(TyField p n _ _) -> (n, p)) fields
  Types.RecordTy <$> traverse convertField fields <*> unique
 where
  convertField field@(TyField _ name _ _) = (name ,) <$> lookupFieldTy tenv field
transTy tenv (ArrayOfTy pos s) =
  Types.ArrayTy <$> lookupTy pos tenv s <*> unique