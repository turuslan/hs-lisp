module Lisp.Repl where

import Lisp.Eval
import Lisp.Parser
import Lisp.Ast
import Lisp.Std

runRepl :: IO ()
runRepl = repl initState

repl :: State -> IO ()
repl state = do
  putStr "> "
  input <- getLine
  case input of
    "(quit)" -> putStrLn "Bye!"
    _ ->
      case parseString input of
        exprs -> do
          (state', result) <- evalIO state $ foldl (>>) (return EmptyList) $ map (eval []) exprs
          case result of 
            Left v -> putStrLn $ show v
            Right (LispError err) -> putStrLn ("error: " ++ err)
          repl state'

