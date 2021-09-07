module Tiger.Types where

import Tiger.Symbol (Symbol)
import qualified Data.Unique as Unique
import qualified Data.IntMap.Strict as IM

newtype Unique = Unique Unique.Unique
  deriving Eq

instance Show Unique where
  show (Unique _) = "Unique"

newUnique :: IO Unique
newUnique = Unique <$> Unique.newUnique

data Ty
  = IntTy
  | StringTy
  | RecordTy [(Symbol, Ty)] Unique
  | ArrayTy Ty Unique
  | NilTy
  | UnitTy
  | NameTy Symbol (Maybe Ty)
  deriving (Eq, Show)

data EnvEntry = VarEntry Ty | FunEntry [Ty] Ty

type TEnv = IM.IntMap Ty
type VEnv = IM.IntMap EnvEntry

type ExpTy = ((), Ty)