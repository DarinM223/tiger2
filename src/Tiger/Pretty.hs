module Tiger.Pretty where

import Prelude hiding (exp)
import Control.Monad (replicateM_)
import Data.Foldable (traverse_)
import Tiger.Tree (BinOp (..), Exp (..), RelOp (..), Stm (..))
import qualified Tiger.Translate as T

indent :: Int -> IO ()
indent i = replicateM_ i (putStr " ")

printFrag :: Show frame => T.Frag frame -> IO ()
printFrag (T.ProcFrag stm frame) = do
  putStrLn $ "Frame: " ++ show frame
  printStm 0 stm
printFrag (T.StringFrag l s) = do
  putStrLn $ "Label " ++ show l ++ " " ++ show s

printStm :: Int -> Stm -> IO ()
printStm d stm = case stm of
  SeqStm a b -> do
    indent d >> putStrLn "SEQ("
    printStm (d + 1) a >> putStrLn ","
    printStm (d + 1) b >> putStr ")"
  LabelStm l -> indent d >> putStr ("LABEL " ++ show l)
  JumpStm e _ -> do
    indent d >> putStrLn "JUMP("
    printExp (d + 1) e >> putStr ")"
  CJumpStm r a b t f -> do
    indent d >> putStr "CJUMP("
    printRelop r >> putStrLn ","
    printExp (d + 1) a >> putStrLn ","
    printExp (d + 1) b >> putStrLn ","
    indent (d + 1)
    putStr $ show t ++ "," ++ show f ++ ")"
  MoveStm a b -> do
    indent d >> putStrLn "MOVE("
    printExp (d + 1) a >> putStrLn ","
    printExp (d + 1) b >> putStr ")"
  ExpStm e -> do
    indent d >> putStrLn "EXP("
    printExp (d + 1) e
    putStr ")"

printExp :: Int -> Exp -> IO ()
printExp d exp = case exp of
  BinOpExp p a b -> do
    indent d >> putStr "BINOP("
    printBinop p >> putStrLn ","
    printExp (d + 1) a >> putStrLn ","
    printExp (d + 1) b >> putStr ")"
  MemExp e -> do
    indent d >> putStrLn "MEM("
    printExp (d + 1) e >> putStr ")"
  TempExp t -> indent d >> putStr "TEMP t" >> putStr (show t)
  ESeqExp s e -> do
    indent d >> putStrLn "ESEQ("
    printStm (d + 1) s >> putStrLn ","
    printExp (d + 1) e >> putStr ")"
  NameExp l -> indent d >> putStr ("NAME " ++ show l)
  ConstExp i -> indent d >> putStr ("CONST " ++ show i)
  CallExp e el -> do
    indent d
    putStrLn "CALL("
    printExp (d + 1) e
    traverse_ (\a -> putStrLn "," >> printExp (d + 2) a) el
    putStr ")"

printBinop :: BinOp -> IO ()
printBinop = putStr . binop

printRelop :: RelOp -> IO ()
printRelop = putStr . relop

binop :: BinOp -> String
binop Plus = "PLUS"
binop Minus = "MINUS"
binop Mul = "MUL"
binop Div = "DIV"
binop And = "AND"
binop Or = "OR"
binop Lshift = "LSHIFT"
binop Rshift = "RSHIFT"
binop Arshift = "ARSHIFT"
binop Xor = "XOR"

relop :: RelOp -> String
relop Eq = "EQ"
relop Ne = "NE"
relop Lt = "LT"
relop Gt = "GT"
relop Le = "LE"
relop Ge = "GE"
relop Ult = "ULT"
relop Ule = "ULE"
relop Ugt = "UGT"
relop Uge = "UGE"