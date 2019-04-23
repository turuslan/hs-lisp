module Lisp.Errors where


import System.Console.Pretty (Color (..), Style (..), bgColor, color, style)
import Text.Parsec.Error
import Lisp.Ast
import Text.Parsec.Pos

data LispError = LispError Pos String deriving Show

customError :: String -> a
customError s = error $ color Red s

parseError :: ParseError -> String -> String
parseError e source = (headerErr "Parse error:\n") ++ (colored $ show e)
    ++ "\n" ++ (ppSourceCode source pos)
    where
        pos = Pos (errorPos e) (errorPos e)

lispError :: LispError -> String -> String
lispError (LispError pos e) source = (headerErr "Error:\n") ++ "'" ++ (show pos) ++ "'" ++ (colored e) 
    ++ "\n" ++ (ppSourceCode source pos)

ppSourceCode :: String -> Pos -> String
ppSourceCode source (Pos start end) = unlines $ drop (startLineN - 1) $ take endLineN $ lines source
    where 
        startLineN = sourceLine start
        endLineN = sourceLine end

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