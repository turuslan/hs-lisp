module Lisp.JsCompiler where

import Lisp.Ast
import Lisp.Parser (parseStringS)

import Text.Parsec.Error (ParseError)
import Data.List (intercalate)

type Locals = [String]

jsCompileString :: String -> Either ParseError String
jsCompileString s = case parseStringS s of
  Left e -> Left e
  Right exprs -> Right $ jscProgram exprs

flatList :: SExpr -> [SExpr]
flatList EmptyList = []
flatList (DottedPair x xs) = x : flatList xs
flatList _ = error "?"

jscGlobal :: String
jscGlobal = "global"

jscProgram :: [SExpr] -> String
jscProgram exprs = unlines (
  ["function program(" ++ jscGlobal ++ ") {"]
  ++ map (jscExpr []) exprs
  ++ ["}"]
  )

jscExpr :: Locals -> SExpr -> String
jscExpr locals exprs@(DottedPair _ _) = case flatList exprs of
  [Atom "defun", Atom name, args, body] ->
    jscFunc name (argNames args) body locals
    where
      argNames EmptyList = []
      argNames (DottedPair (Atom aname) cdr) =  aname : argNames cdr
      argNames _ = error "?"
  [Atom "let", pairs, body] -> case flatList pairs of
    [pair] -> case flatList pair of
      [Atom name, value] -> unlines [
        "((" ++ name ++ ") =>",
        jscExpr (name:locals) body,
        ")(" ++ jscExpr locals value ++ ")"
        ]
      _ -> error "?"
    _ -> error "?"
  [Atom "if", test, left, right] -> unlines [
    "(" ++ jscExpr locals test ++ ") ? (",
    jscExpr locals left,
    ") : (",
    jscExpr locals right,
    ")"
    ]
  [Atom "+", left, right] -> "(" ++ jscExpr locals left ++ ") + (" ++ jscExpr locals right ++ ")"
  [Atom "-", left, right] -> "(" ++ jscExpr locals left ++ ") - (" ++ jscExpr locals right ++ ")"
  [Atom "/", left, right] -> "(" ++ jscExpr locals left ++ ") / (" ++ jscExpr locals right ++ ")"
  [Atom "=", left, right] -> "(" ++ jscExpr locals left ++ ") === (" ++ jscExpr locals right ++ ")"
  [Atom "<", left, right] -> "(" ++ jscExpr locals left ++ ") < (" ++ jscExpr locals right ++ ")"
  [Atom ">", left, right] -> "(" ++ jscExpr locals left ++ ") > (" ++ jscExpr locals right ++ ")"
  (Atom "seq":xs) -> "(" ++ intercalate ",\n" (map (jscExpr locals) xs) ++ ")"
  (Atom name:args) -> jscGlobal ++ "._get(\"" ++ name ++ "\")(" ++ intercalate ", " (map (jscExpr locals) args) ++ ")"
  exprs2 -> "/* TODO: {" ++ show exprs2 ++ " in " ++ show locals ++ "} */"
jscExpr locals (Atom name)
  | elem name locals = name
  | otherwise = jscGlobal ++ "._get(\"" ++ name ++ "\")"
jscExpr _ (IntegerLiteral value) = show value
jscExpr _ (StringLiteral value) = show value
jscExpr locals expr = "/* TODO: {" ++ show expr ++ " with " ++ show locals ++ "} */"

jscFunc :: String -> [String] -> SExpr -> Locals -> String
jscFunc name args body locals = unlines [
  jscGlobal ++ "._def(\"" ++ name ++ "\",",
  "(" ++ intercalate ", " args ++ ") => " ++ jscExpr (locals ++ args) body,
  ")"
  ]
