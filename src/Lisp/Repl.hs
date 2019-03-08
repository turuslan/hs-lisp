module Lisp.Repl where

import Lisp.Eval
import Lisp.Parser

exitCommand = "(quit)"

runRepl :: IO ()
runRepl = repl

repl :: IO ()
repl = do
  putStr "> "
  input <- getLine
  case input of
    "(quit)" -> putStrLn "Bye!"
    _ ->
      case parseString input of
        expr -> do
          putStr $ show expr
          repl