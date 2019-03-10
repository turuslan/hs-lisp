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
fun__plus [(_, args)] = sum' args where
  sum' EmptyList = return $ IntegerLiteral 0
  sum' (DottedPair car cdr) = do
    scdr <- sum' cdr
    coerced <- coerce car scdr
    return $ math_plus coerced
  sum' _ = impossible
fun__plus _ = impossible



--
coerce :: SExpr -> SExpr -> Eval (SExpr, SExpr)
coerce a@(IntegerLiteral _) b@(IntegerLiteral _) = return (a, b)
coerce a@(FloatLiteral _) b@(FloatLiteral _) = return (a, b)
coerce (IntegerLiteral a) b@(FloatLiteral _) = return (FloatLiteral $ fromInteger a, b)
coerce a@(FloatLiteral _) (IntegerLiteral b) = return (a, FloatLiteral $ fromInteger b)
coerce a b = eval_error (show (if is_number a then b else a) ++ " is not a number")

is_number :: SExpr -> Bool
is_number (FloatLiteral _) = True
is_number (IntegerLiteral _) = True
is_number _ = False

math_plus :: (SExpr, SExpr) -> SExpr
math_plus (IntegerLiteral a, IntegerLiteral b) = IntegerLiteral (a + b)
math_plus (FloatLiteral a, FloatLiteral b) = FloatLiteral (a + b)
math_plus _ = impossible
