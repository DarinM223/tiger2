{-# LANGUAGE TupleSections #-}
module Tiger.Types where

import Tiger.Symbol (MonadSymbol (symbol), Symbol, symbolId)
import Tiger.Temp (MonadTemp (namedLabel), Unique)
import Tiger.Translate (Access, Exp, MonadTranslate (..), Translate (outermost))
import qualified Data.IntMap.Strict as IM

data Ty
  = IntTy
  | StringTy
  | RecordTy [(Symbol, Ty)] Unique
  | ArrayTy Ty Unique
  | NilTy
  | UnitTy
  | NameTy Symbol (Maybe Ty)
  deriving (Eq, Show)

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

mkEnvs :: (MonadTemp m, MonadTranslate level m) => m (VEnv level, TEnv)
mkEnvs = (,) <$> (venvBase >>= convertBase) <*> convertBase tenvBase
 where
  tenvBase = [("int", IntTy), ("string", StringTy)]
  venvBase = traverse toFunTuple fns

  fns = [ ("print", [StringTy], UnitTy)
        , ("flush", [], UnitTy)
        , ("getchar", [], StringTy)
        , ("ord", [StringTy], IntTy)
        , ("chr", [IntTy], StringTy)
        , ("size", [StringTy], IntTy)
        , ("substring", [StringTy, IntTy, IntTy], StringTy)
        , ("not", [IntTy], IntTy)
        , ("exit", [IntTy], UnitTy) ]

  toFunTuple t@(name, _, _) = (name ,) <$> toFunEntry t
  toFunEntry (name, params, ret) = do
    label <- namedLabel name
    level <- newLevel outermost label (fmap (const False) params)
    pure $ FunEntry level params ret

  convertBase = fmap IM.fromList
              . traverse (\(s, ty) -> (, ty) . symbolId <$> symbol s)