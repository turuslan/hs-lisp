module Lisp.Repl where

import Lisp.Eval
import Lisp.Parser
import Lisp.Std
import Lisp.Errors

import System.IO
import Control.Exception

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
      se <- try $ readFile path
      case se of
        Left e -> do
          print (e :: IOError)
          repl state
        Right s -> evalStr s
    _ -> evalStr input
  where
    evalStr s = case parseStringS s of
      Left e -> do
        putStrLn (parseError e s)
        repl state
      Right exprs -> do
        (state', result) <- evalIO state $ foldl1 (>>) $ map (eval []) exprs
        case result of 
          Left v -> putStrLn $ show v
          Right e -> putStrLn (lispError e s)
        repl state'
