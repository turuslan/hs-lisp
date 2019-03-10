module Lisp.Std where

import Lisp.Ast
import Lisp.Eval



--
initState :: State
initState = State [] []
  [ ("nil", EmptyList)
  ]
  [ ("read-int", (parse_args "()", fun_read_int))
  , ("print", (parse_args "(x)", fun_print))
  , ("+", (parse_args "(&rest xs)", fun__plus))
  , ("/", (parse_args "(num &rest xs)", fun__div))
  ]



--
fun_read_int :: Fun
fun_read_int _ = do
  str <- eval_read
  case reads str :: [(Integer, String)] of
    [(v, _)] -> return $ IntegerLiteral v
    _ -> todo_runtime_error

fun_print :: Fun
fun_print [(_, arg)] = do
  eval_write $ show arg
  return arg
fun_print _ = impossible

fun__plus :: Fun
fun__plus [(_, args)] = reduce (_math_binary (+) (+)) (IntegerLiteral 0) args
fun__plus _ = impossible

fun__div :: Fun
fun__div [(_, num@(IntegerLiteral 1)), (_, EmptyList)] = return num
fun__div [(_, num), (_, EmptyList)] = math_div (FloatLiteral 1) num
fun__div [(_, num), (_, xs)] = reduce math_div num xs
fun__div _ = impossible

math_div :: SExpr -> SExpr -> Eval SExpr
math_div a b = do
  ab <- coerce a b
  case ab of
    (IntegerLiteral a', IntegerLiteral b') -> case divMod a' b' of
      (i, 0) -> return $ IntegerLiteral i
      _ -> return $ FloatLiteral (fromInteger a' / fromInteger b')
    (FloatLiteral a', FloatLiteral b') -> return $ FloatLiteral (a' / b')
    _ -> impossible



--
coerce :: SExpr -> SExpr -> Eval (SExpr, SExpr)
coerce a@(IntegerLiteral _) b@(IntegerLiteral _) = return (a, b)
coerce a@(FloatLiteral _) b@(FloatLiteral _) = return (a, b)
coerce (IntegerLiteral a) b@(FloatLiteral _) = return (FloatLiteral $ fromInteger a, b)
coerce a@(FloatLiteral _) (IntegerLiteral b) = return (a, FloatLiteral $ fromInteger b)
coerce a b = _is_number a >> _is_number b >> return (EmptyList, EmptyList)

reduce :: (SExpr -> SExpr -> Eval SExpr) -> SExpr -> SExpr -> Eval SExpr
reduce f acc (DottedPair car cdr) = do
  acc' <- f acc car
  reduce f acc' cdr
reduce _ acc _ = return acc

is_number :: SExpr -> Bool
is_number (FloatLiteral _) = True
is_number (IntegerLiteral _) = True
is_number _ = False


_is_number :: SExpr -> Eval ()
_is_number (IntegerLiteral _) = return ()
_is_number (FloatLiteral _) = return ()
_is_number a = eval_error (show a ++ " is not a number")

_math_binary :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> SExpr -> SExpr -> Eval SExpr
_math_binary fi ff a b = do
  ab <- coerce a b
  case ab of
    (IntegerLiteral a', IntegerLiteral b') -> return $ IntegerLiteral $ fi a' b'
    (FloatLiteral a', FloatLiteral b') -> return $ FloatLiteral $ ff a' b'
    _ -> impossible
