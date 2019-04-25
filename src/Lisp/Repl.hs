module Lisp.Repl where

import Lisp.Eval
import Lisp.Parser
import Lisp.Std
import Lisp.JsCompiler
import Lisp.Errors

import System.IO
import Control.Exception



-- | Invoke repl loop with initial state.
runRepl :: IO ()
runRepl = do
  hSetBuffering stdin LineBuffering
  repl initState

-- | Repl loop iteration.
-- Commands:
-- * ":q" - quit repl.
-- * ":r" - reset state to initial.
-- * ":f <path>" - run "<path>" lisp file.
-- * ":js <path>" - compile "<path>" lisp file to "<path>.js" js file.
repl :: State -> IO ()
repl state = do
  putStr "> "
  hFlush stdout
  input <- getLine
  case input of
    ":q" -> return ()
    ":r" -> repl initState
    ':':'j':'s':' ':path -> do
      jsBuild path
      repl state
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

-- | Compile lisp to js and save resulting file.
jsBuild :: String -> IO ()
jsBuild path = do
  base <- try $ readFile "nodejs.base.js"
  case base of
    Left e -> do
      putStrLn "nodejs build target is missing"
      print (e :: IOError)
    Right jsBase -> do
      src <- try $ readFile path
      case src of
        Left e -> print (e :: IOError)
        Right s -> case jsCompileString s of
          Left e -> putStrLn ("parse error: " ++ show e)
          Right js -> do
            writeFile out (jsBase ++ "\n" ++ js)
            putStrLn ("successfully built " ++ out)
  where
    out = path ++ ".js"
