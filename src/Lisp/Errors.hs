module Lisp.Errors where


import System.Console.Pretty (Color (..), Style (..), bgColor, color, style)
import Text.Parsec.Error


data LispError = LispError String deriving Show

customError :: String -> a
customError s = error $ color Red s

impossible :: a
impossible = error $ 
    bgColor Red . color White . style Bold $ 
    "Something went wrong. Impossible program state."

parseError :: ParseError -> String
parseError e = (headerErr "Parse error:\n") ++ (colored $ show e)

lispError :: LispError -> String
lispError (LispError e) = (headerErr "Error:\n") ++ (colored e)

headerErr :: String -> String
headerErr s = bgColor Red . color White . style Bold $ s

colored :: String -> String
colored "" = "" 
colored s = (red err) ++ (yellow bold) ++ colored rest
        where
            err = takeWhile (/= '\'') s
            next = drop (1 + length err) s
            bold = takeWhile (/= '\'') next
            rest = drop (1 + length bold) next
            red str = color Red $ str
            yellow str = color Yellow . style Bold $ str