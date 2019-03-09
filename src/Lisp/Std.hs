module Lisp.Std where

import Lisp.Ast
import Lisp.Eval
import Lisp.Parser (parseString)



--
initState :: State
initState = State [] []
  [ ("nil", EmptyList)
  ]
  [ ("read-int", (parse_args "()", fun_read_int))
  , ("print", (parse_args "(x)", fun_print))
  ]

parse_args :: String -> SExpr
parse_args = head . parseString

impossible :: a
impossible = error "impossible"



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
