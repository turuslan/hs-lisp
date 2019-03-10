module Lisp.Repl where

import Lisp.Eval
import Lisp.Parser
import Lisp.Std

import System.IO

runRepl :: IO ()
runRepl = repl initState

repl :: State -> IO ()
repl state = do
  putStr "> "
  hFlush stdout
  input <- getLine
  case input of
    ":q" -> return ()
    ":r" -> repl initState
    ':':'f':' ':path -> do
      s <- readFile path
      evalStr s
    _ -> evalStr input
  where
    evalStr s = case parseString s of
      exprs -> do
        (state', result) <- evalIO state $ foldl1 (>>) $ map (eval []) exprs
        case result of 
          Left v -> putStrLn $ show v
          Right (LispError err) -> putStrLn ("error: " ++ err)
        repl state'
