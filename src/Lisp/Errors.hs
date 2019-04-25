module Lisp.Errors where

import System.Console.Pretty (Color (..), Style (..), bgColor, color, style)
import Text.Parsec.Error
import Lisp.Ast
import Text.Parsec.Pos

-- | Error for monad.
data LispError = LispError Pos String deriving Show

-- | Any error message
customError :: String -> a
customError s = error $ color Red s

-- | ParseError to readable message
parseError :: ParseError -> String -> String
parseError e source = errorString (show e) pos source
    where
        pos = Pos (errorPos e) (errorPos e)

-- | LispError to readable message
lispError :: LispError -> String -> String
lispError (LispError pos e) source = errorString e pos source

-- | Readable error message with pointing to sorce code location
errorString :: String -> Pos -> String -> String
errorString msg pos@(Pos p1 _) source = unlines
    [ headerErr "Error:"
    , (show p1) 
    , "'" ++ (colored msg) ++ "'" 
    , ppSourceCode source pos
    ]
    where
        headerErr :: String -> String
        headerErr s = bgColor Red . color White . style Bold $ s        

-- | Pretty print source code with lines' numbers
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

-- | Hightlight with yellow color parts of error messages
--   in single quotes
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