{-# LANGUAGE RecordWildCards #-}
module Tiger.Types where

import Tiger.Symbol (Symbol, symbolId)
import Tiger.Temp (Temp_ (..), Unique)
import Tiger.Translate (Access, Exp, Translate (outermost))
import qualified Data.IntMap.Strict as IM

data Ty
  = IntTy
  | StringTy
  | RecordTy [(Symbol, Ty)] Unique
  | ArrayTy Ty Unique
  | NilTy
  | UnitTy
  | NameTy Symbol (Maybe Ty)
  deriving Show

instance Eq Ty where
  (==) IntTy IntTy = True
  (==) StringTy StringTy = True
  (==) (RecordTy _ u1) (RecordTy _ u2) = u1 == u2
  (==) (RecordTy _ _) NilTy = True
  (==) NilTy (RecordTy _ _) = True
  (==) (ArrayTy _ u1) (ArrayTy _ u2) = u1 == u2
  (==) NilTy NilTy = True
  (==) UnitTy UnitTy = True
  (==) (NameTy s1 _) (NameTy s2 _) = s1 == s2
  (==) _ _ = False

data EnvEntry l = VarEntry (Access l) Ty | FunEntry l [Ty] Ty

type TEnv = IM.IntMap Ty
type VEnv l = IM.IntMap (EnvEntry l)

type ExpTy = (Exp, Ty)

actualTy :: Ty -> Ty
actualTy (NameTy _ (Just ty)) = actualTy ty
actualTy ty                   = ty

tyMatches :: Ty -> Ty -> Bool
tyMatches a b = actualTy a == actualTy b

lookupEnv :: Symbol -> IM.IntMap a -> Maybe a
lookupEnv s = IM.lookup (symbolId s)

-- | Looks up type in type environment.
-- Unlike `lookupEnv`, `lookupType` looks into name references.
lookupType :: Symbol -> TEnv -> Maybe Ty
lookupType s = fmap actualTy . lookupEnv s

insertEnv :: Symbol -> a -> IM.IntMap a -> IM.IntMap a
insertEnv s = IM.insert (symbolId s)

adjustEnv :: (a -> a) -> Symbol -> IM.IntMap a -> IM.IntMap a
adjustEnv f s = IM.adjust f (symbolId s)

mkEnvs :: (Monad m, Translate level) => Temp_ m -> m (VEnv level, TEnv)
mkEnvs Temp_{..} = (,) <$> convertBase venvBase <*> convertBase tenvBase
 where
  tenvBase = [("int", IntTy), ("string", StringTy)]
  venvBase = fmap toFunTuple fns

  fns = [ ("print", [StringTy], UnitTy)
        , ("flush", [], UnitTy)
        , ("getchar", [], StringTy)
        , ("ord", [StringTy], IntTy)
        , ("chr", [IntTy], StringTy)
        , ("size", [StringTy], IntTy)
        , ("substring", [StringTy, IntTy, IntTy], StringTy)
        , ("not", [IntTy], IntTy)
        , ("exit", [IntTy], UnitTy) ]

  -- These functions are external assembly, so they don't use
  -- static links like Tiger functions do, so their level
  -- is Outermost. Note that the main function still uses static links,
  -- so main is in a child level of Outermost.
  toFunTuple (name, params, ret) = (name, FunEntry outermost params ret)

  convertBase = fmap IM.fromList
              . traverse (\(s, ty) -> (, ty) . symbolId <$> namedLabel s)