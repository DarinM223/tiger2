module Tiger.FindEscape where

import Prelude hiding (exp)
import Control.Monad.ST.Strict
import Data.STRef.Strict
import Tiger.AST
import qualified Data.IntMap as IM

expToSt :: Exp -> ST s (Exp' (STRef s Bool))
expToSt = traverse newSTRef

stToExp :: Exp' (STRef s Bool) -> ST s Exp
stToExp = traverse readSTRef

type Depth = Int
type EscapeEnv s = IM.IntMap (Int, STRef s Bool)

findEscapes :: Exp -> Exp
findEscapes exp = runST $ do
  exp' <- expToSt exp
  traverseExp IM.empty 0 exp'
  stToExp exp'

traverseVar :: EscapeEnv s -> Depth -> Var' (STRef s Bool) -> ST s ()
traverseVar _ _ = trVar
 where
  trVar = undefined

traverseExp :: EscapeEnv s -> Depth -> Exp' (STRef s Bool) -> ST s ()
traverseExp = undefined

traverseDecs
  :: EscapeEnv s -> Depth -> [Dec' (STRef s Bool)] -> ST s (EscapeEnv s)
traverseDecs = undefined