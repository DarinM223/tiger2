module Tiger.FindEscape where

import Prelude hiding (exp, init)
import Control.Monad.ST.Strict (ST, runST)
import Data.Foldable (foldlM, traverse_)
import Data.Functor (($>))
import Data.STRef.Strict (STRef, newSTRef, readSTRef, writeSTRef)
import Tiger.AST
import Tiger.Types (insertEnv, lookupEnv)
import qualified Data.IntMap as IM

type Depth = Int
type EscapeEnv s = IM.IntMap (Int, STRef s Bool)

findEscapes :: Exp -> Exp
findEscapes exp = runST $ do
  exp' <- traverse newSTRef exp
  traverseExp IM.empty 0 exp'
  traverse readSTRef exp'

traverseVar :: EscapeEnv s -> Depth -> Var' (STRef s Bool) -> ST s ()
traverseVar env depth = trVar
 where
  trVar (Var s) = traverse_ updateEscRef (lookupEnv s env)
  trVar (RecField _ var _) = trVar var
  trVar (ArraySub _ var exp) = trVar var >> traverseExp env depth exp
  updateEscRef (depth', ref)
    | depth > depth' = writeSTRef ref True
    | otherwise      = pure ()

traverseExp :: EscapeEnv s -> Depth -> Exp' (STRef s Bool) -> ST s ()
traverseExp env depth = trExp
 where
  trExp (VarExp var) = traverseVar env depth var
  trExp (NilExp _) = pure ()
  trExp (SeqExp _ exps) = traverse_ trExp exps
  trExp (IntExp _) = pure ()
  trExp (StringExp _) = pure ()
  trExp (OpExp _ _ exp1 exp2) = trExp exp1 >> trExp exp2
  trExp (FuncallExp _ _ exps) = traverse_ trExp exps
  trExp (RecordExp _ _ fields) = traverse_ (\(_, _, e) -> trExp e) fields
  trExp (ArrayExp _ _ exp1 exp2) = trExp exp1 >> trExp exp2
  trExp (AssignExp _ var exp) = traverseVar env depth var >> trExp exp
  trExp (IfExp _ e1 e2 e3) = trExp e1 >> trExp e2 >> traverse_ trExp e3
  trExp (WhileExp _ exp1 exp2) = trExp exp1 >> trExp exp2
  trExp (ForExp _ name e1 e2 body esc) =
    writeSTRef esc False >> trExp e1 >> trExp e2 >> traverseExp env' depth body
   where env' = insertEnv name (depth, esc) env
  trExp (BreakExp _) = pure ()
  trExp (LetExp _ decs body) = do
    env' <- traverseDecs env depth decs
    traverseExp env' depth body

traverseDecs
  :: EscapeEnv s -> Depth -> [Dec' (STRef s Bool)] -> ST s (EscapeEnv s)
traverseDecs env0 depth = foldlM trDec env0
 where
  trDec env TyDecs{} = pure env
  trDec env (VarDec (VarDec' _ name _ init esc)) =
    writeSTRef esc False >> traverseExp env depth init
    $> insertEnv name (depth, esc) env
  trDec env (FunDecs decs) = traverse_ (trFunDec env) decs $> env

  trFunDec env (FunDec _ _ fields _ body) = do
    env' <- foldlM trField env fields
    traverseExp env' (depth + 1) body
  trField env (TyField _ name _ esc) =
    writeSTRef esc False $> insertEnv name (depth + 1, esc) env