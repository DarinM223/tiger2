{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Tiger.Semant where

import Prelude hiding (exp, init)
import Control.Monad
import Data.Bifunctor (second)
import Data.Foldable (foldl', foldlM, for_, traverse_)
import Data.Functor ((<&>))
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.List (sortOn)
import System.IO (hPutStrLn, stderr)
import Tiger.AST
import Tiger.IntVar (newIntVar, readIntVar, writeIntVar)
import Tiger.Symbol (Symbol, symbolId)
import Tiger.Temp (Temp_ (..), Unique, newUnique)
import Tiger.Translate (Translate (..), Translate_ (..), unit)
import Tiger.Types hiding (Ty (..), EnvEntry (..))
import qualified Data.IntSet as IS
import qualified Tiger.Frame as F
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

breakId :: String
breakId = "123*break"

semant_
  :: forall level m. (Monad m, Translate level)
  => Temp_ m -> m Unique -> (String -> m ()) -> Translate_ level m
  -> (VEnv level -> TEnv -> Exp -> m ExpTy)
semant_ Temp_{..} unique compileError Translate_{..} =
  let
    lookupFieldTy :: TEnv -> TyField Bool -> m Types.Ty
    lookupFieldTy tenv (TyField pos _ s _) = lookupTy pos tenv s

    lookupTy :: Show a => a -> TEnv -> Symbol -> m Types.Ty
    lookupTy pos tenv s = case lookupEnv s tenv of
      Just ty -> pure ty
      Nothing -> do
        compileError $ "Error (" ++ show pos
                    ++ "): Cannot find type with symbol " ++ show s
        pure Types.IntTy

    checkDup :: [(Symbol, Pos)] -> m ()
    checkDup = void . foldlM go IS.empty
     where
      go set (s, pos)
        | IS.member symId set = do
          compileError $ "Error (" ++ show pos ++ "): duplicate symbol " ++ show s
          pure set
        | otherwise = pure $ IS.insert symId set
       where symId = symbolId s

    transVar :: level -> VEnv level -> TEnv -> Var -> m ExpTy
    transVar level venv tenv = trVar
     where
      trVar (Var s) = case lookupEnv s venv of
        Just (Types.VarEntry access ty) ->
          pure (simpleVar access level, actualTy ty)
        _ -> do
          compileError $ "Var " ++ show s ++ " is not in the environment"
          pure (unit, Types.IntTy)
      trVar (RecField pos var sym) = do
        (exp, varTy) <- trVar var
        case actualTy varTy of
          Types.RecordTy fields _ | Just fieldTy <- lookup sym fields ->
            fieldVar exp sym (fmap fst fields) <&> (, fieldTy)
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
            subscriptVar exp1 exp2 <&> (, elemTy)
          _ -> do
            compileError $ "Error (" ++ show pos
                        ++ "): attempting to index a value of type " ++ show varTy
            pure (unit, Types.IntTy)

    transExp :: level -> VEnv level -> TEnv -> Exp -> m ExpTy
    transExp level venv tenv = trExp
     where
      trExp (VarExp var) = transVar level venv tenv var
      trExp (NilExp _) = pure (unit, Types.NilTy)
      trExp (SeqExp _ exps) = do
        results <- traverse trExp exps
        let lastTy = foldl' (const snd) Types.UnitTy results
        seqExp (fmap fst results) <&> (, lastTy)
      trExp (IntExp i) = intExp i <&> (, Types.IntTy)
      trExp (StringExp s) = stringExp s <&> (, Types.StringTy)
      trExp (OpExp pos op exp1 exp2) = do
        (exp1', ty1) <- trExp exp1
        (exp2', ty2) <- trExp exp2
        (, Types.IntTy) <$> case (opType op, ty1, ty2) of
          (Arithmetic, Types.IntTy, Types.IntTy) -> binOpExp op exp1' exp2'
          (Comparison, Types.IntTy, Types.IntTy) -> iRelOpExp op exp1' exp2'
          (Comparison, Types.StringTy, Types.StringTy) -> sRelOpExp op exp1' exp2'
          (Equality, Types.IntTy, Types.IntTy) -> iRelOpExp op exp1' exp2'
          (Equality, Types.StringTy, Types.StringTy) -> sRelOpExp op exp1' exp2'
          (Equality, Types.ArrayTy _ _, Types.ArrayTy _ _) ->
            iRelOpExp op exp1' exp2'
          (Equality, Types.RecordTy _ _, Types.RecordTy _ _) ->
            iRelOpExp op exp1' exp2'
          _ -> do
            compileError $ "Error (" ++ show pos ++ "): Invalid types: "
                        ++ show ty1 ++ ", " ++ show ty2
                        ++ " for operator " ++ show op
            pure unit
      trExp (FuncallExp pos name exps) = case lookupEnv name venv of
        Just (Types.FunEntry levelFun tys retTy) -> do
          results <- traverse trExp exps
          let tys' = fmap snd results
          unless (tys == tys') $
            compileError $ "Error (" ++ show pos ++ "): Function " ++ show name
                        ++ "'s parameter type list " ++ show tys
                        ++ " is different from " ++ show tys'
          funCallExp levelFun level name (fmap fst results) <&> (, retTy)
        _ -> do
          compileError $ "Error (" ++ show pos ++ "): Function " ++ show name
                      ++ " is not in the environment"
          pure (unit, Types.IntTy)
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
            recordExp exps <&> (, recTy)
          _ -> do
            compileError $ "Error (" ++ show pos ++ "): Record " ++ show name
                        ++ " is not in the environment"
            results <- trFields fields
            (unit ,) . Types.RecordTy (fmap (second snd) results) <$> unique
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
          arrayExp exp1 exp2 <&> (, arrTy)
        _ -> do
          compileError $ "Error (" ++ show pos ++ "): Array " ++ show name
                      ++ " is not in the environment"
          (unit ,) <$> (Types.ArrayTy <$> (snd <$> trExp valueExp) <*> unique)
      trExp (AssignExp pos var exp) = do
        (exp1, varTy) <- transVar level venv tenv var
        (exp2, expTy) <- trExp exp
        unless (varTy == expTy) $
          compileError $ "Error (" ++ show pos
                      ++ "): Attempting to assign a value of type " ++ show expTy
                      ++ " to variable of type " ++ show varTy
        assignExp exp1 exp2 <&> (, Types.UnitTy)
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
        ifElseExp exp1 exp2 (fmap fst elseResult) <&> (, thenTy)
      trExp (WhileExp pos test body) = do
        (exp1, testTy) <- trExp test
        unless (testTy == Types.IntTy) $
          compileError $ "Error (" ++ show pos
                      ++ "): While test expression must have integer type"
        s <- namedLabel breakId
        done <- newLabel
        let tenv' = insertEnv s (Types.NameTy done Nothing) tenv
        (exp2, bodyTy) <- transExp level venv tenv' body
        unless (bodyTy == Types.UnitTy) $
          compileError $ "Error (" ++ show pos
                      ++ "): While body expression must have unit type"
        whileExp exp1 exp2 done <&> (, Types.UnitTy)
      trExp (LetExp _ decs body) = do
        (venv', tenv', stms) <- foldlM
          (\(venv', tenv', exps) ->
            fmap (fmap (exps ++)) . transDec level venv' tenv')
          (venv, tenv, [])
          decs
        (exp, ty) <- transExp level venv' tenv' body
        letExp stms exp <&> (, ty)
      trExp (ForExp pos sym start end body esc) = trExp loopExp
       where
        loopExp = LetExp pos [startDec] (WhileExp pos testExp body')
        startDec = VarDec $ VarDec' pos sym Nothing start esc
        testExp = OpExp pos LtOp (VarExp (Var sym)) end
        body' = SeqExp pos [body, incrExp]
        incrExp =
          AssignExp pos (Var sym) (OpExp pos AddOp (VarExp (Var sym)) (IntExp 1))
      trExp (BreakExp pos) = do
        s <- namedLabel breakId
        case lookupEnv s tenv of
          Just (Types.NameTy done _) -> breakExp done <&> (, Types.UnitTy)
          _ -> do
            compileError $ "Error (" ++ show pos
                        ++ "): break must be within a while or for loop"
            pure (unit, Types.UnitTy)

    transDec
      :: level -> VEnv level -> TEnv -> Dec -> m (VEnv level, TEnv, [T.Exp])
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
      access <- allocLocal level esc
      for_ tyMaybe $ \s -> case lookupType s tenv of
        Just ty -> unless (initTy == ty) $
          compileError $ "Error (" ++ show pos ++ "): expected type " ++ show ty
                      ++ " got " ++ show initTy
        Nothing ->
          compileError $ "Error (" ++ show pos ++ "): type " ++ show s
                      ++ " not in environment"
      let venv' = insertEnv name (Types.VarEntry access initTy) venv
      exp' <- assignExp (simpleVar access level) exp
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
        functionDec level' exp
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

    transTy :: TEnv -> Ty -> m Types.Ty
    transTy tenv (IdTy s) = lookupTy "" tenv s
    transTy tenv (FieldsTy _ fields) = do
      checkDup $ fmap (\(TyField p n _ _) -> (n, p)) fields
      Types.RecordTy <$> traverse convertField fields <*> unique
     where
      convertField field@(TyField _ name _ _) =
        (name ,) <$> lookupFieldTy tenv field
    transTy tenv (ArrayOfTy pos s) =
      Types.ArrayTy <$> lookupTy pos tenv s <*> unique

    transProg :: VEnv level -> TEnv -> Exp -> m ExpTy
    transProg venv tenv e = do
      name <- namedLabel "main"
      level <- newLevel outermost name []
      (e', ty) <- transExp level venv tenv e
      functionDec level e'
      return (e', ty)
  in transProg

tcIO
  :: F.Frame frame => Temp_ IO -> F.Frame_ frame IO
  -> IO (Exp -> IO (Maybe (ExpTy, [T.Frag frame])))
tcIO t_ f_ = do
  fragList <- newIORef []
  var <- newIntVar 0
  let compileError err = writeIntVar var 1 >> hPutStrLn stderr err
      put frag = modifyIORef' fragList (frag :)
      translate_ = T.translate_ t_ newUnique put f_
  (venv, tenv) <- mkEnvs t_
  let semantIO = semant_ t_ newUnique compileError translate_ venv tenv
  pure $ \e -> do
    r <- semantIO e
    failed <- readIntVar var
    if failed == 0
      then Just . (r ,) . reverse <$> readIORef fragList
      else pure Nothing