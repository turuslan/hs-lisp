module Lisp.Errors where


import System.Console.Pretty (Color (..), Style (..), bgColor, color, style)
import Text.Parsec.Error
import Lisp.Ast
import Text.Parsec.Pos

data LispError = LispError Pos String deriving Show

customError :: String -> a
customError s = error $ color Red s

parseError :: ParseError -> String -> String
parseError e source = errorString (show e) pos source
    where
        pos = Pos (errorPos e) (errorPos e)

lispError :: LispError -> String -> String
lispError (LispError pos e) source = errorString e pos source

errorString :: String -> Pos -> String -> String
errorString msg pos@(Pos p1 _) source = unlines
    [ headerErr "Error:"
    , (show p1) 
    , "'" ++ (colored msg) ++ "'" 
    , ppSourceCode source pos
    ]

ppSourceCode :: String -> Pos -> String
ppSourceCode source (Pos start end) = unlines $ zipWith (++) lineNums linesForShow
    where 
        lineNums = map lineNoStr [startLineN..endLineN]
        linesForShow = drop (startLineN - 1) $ take endLineN $ lines source
        startLineN = sourceLine start
        endLineN = sourceLine end
        maxLineNLen = length $ show endLineN
        lineNoStr n = let 
            diff = maxLineNLen - length (show n) in
            unwords (replicate diff " ") ++ show n ++ " | "

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