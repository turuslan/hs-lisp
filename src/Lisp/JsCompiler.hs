module Lisp.JsCompiler where

import Lisp.Ast
import Lisp.Parser (parseStringS)

import Text.Parsec.Error (ParseError)
import Data.List (intercalate)



-- | Type to represent set of available names.
-- Requires operations: add element, check if element exists.
type Locals = [String]

-- | Compile lisp code to js code.
jsCompileString :: String -> Either ParseError String
jsCompileString s = case parseStringS s of
  Left e -> Left e
  Right exprs -> Right $ jscProgram exprs

-- | Extract elements from Lisp.Ast form list to haskell list.
flatList :: SExpr -> [SExpr]
flatList (EmptyList _) = []
flatList (DottedPair x xs _) = x : flatList xs
flatList _ = error "?"

-- | Name of global parameter.
jscGlobal :: String
jscGlobal = "_"

-- | Generate js top-level function representing program for lisp top-level expressions.
jscProgram :: [SExpr] -> String
jscProgram exprs = unlines (
  ["function program(" ++ jscGlobal ++ ") {"]
  ++ map (jscExpr []) exprs
  ++ ["}"]
  )

-- | Generate js expression for lisp expression.
-- "locals" represent available local variables, includes parent scopes.
-- For "let" form "locals" passed down are extended with declared variables.
jscExpr :: Locals -> SExpr -> String
jscExpr locals exprs@(DottedPair _ _ _) = case flatList exprs of
  [Atom "defun" _, Atom name _, args, body] ->
    jscFunc name (argNames args) body locals
    where
      argNames (EmptyList _) = []
      argNames (DottedPair (Atom aname _) cdr _) =  aname : argNames cdr
      argNames _ = error "?"
  [Atom "let" _, pairs, body] -> case flatList pairs of
    [pair] -> case flatList pair of
      [Atom name _, value] -> unlines [
        "((" ++ name ++ ") =>",
        jscExpr (name:locals) body,
        ")(" ++ jscExpr locals value ++ ")"
        ]
      _ -> error "?"
    _ -> error "?"
  [Atom "if" _, test, left, right] -> unlines [
    "(" ++ jscExpr locals test ++ ") ? (",
    jscExpr locals left,
    ") : (",
    jscExpr locals right,
    ")"
    ]
  [Atom "+" _, left, right] -> "(" ++ jscExpr locals left ++ ") + (" ++ jscExpr locals right ++ ")"
  [Atom "-" _, left, right] -> "(" ++ jscExpr locals left ++ ") - (" ++ jscExpr locals right ++ ")"
  [Atom "/" _, left, right] -> "(" ++ jscExpr locals left ++ ") / (" ++ jscExpr locals right ++ ")"
  [Atom "=" _, left, right] -> "(" ++ jscExpr locals left ++ ") === (" ++ jscExpr locals right ++ ")"
  [Atom "<" _, left, right] -> "(" ++ jscExpr locals left ++ ") < (" ++ jscExpr locals right ++ ")"
  [Atom ">" _, left, right] -> "(" ++ jscExpr locals left ++ ") > (" ++ jscExpr locals right ++ ")"
  (Atom "seq" _:xs) -> "(" ++ intercalate ",\n" (map (jscExpr locals) xs) ++ ")"
  (name@(Atom _ _):args) -> jscExpr locals name ++ "(" ++ intercalate ", " (map (jscExpr locals) args) ++ ")"
  exprs2 -> "/* TODO: {" ++ show exprs2 ++ " in " ++ show locals ++ "} */"
jscExpr locals (Atom name _)
  | elem name locals = name
  | otherwise = jscGlobal ++ "`" ++ name ++ "`"
jscExpr _ (IntegerLiteral value _) = show value
jscExpr _ (StringLiteral value _) = show value
jscExpr locals expr = "/* TODO: {" ++ show expr ++ " with " ++ show locals ++ "} */"

-- | Generate js expression for lisp "defun" special form.
-- "locals" passed down are extended with function arguments.
jscFunc :: String -> [String] -> SExpr -> Locals -> String
jscFunc name args body locals = unlines [
  jscGlobal ++ ".set(`" ++ name ++ "`, (" ++ intercalate ", " args ++ ") => ",
  jscExpr (locals ++ args) body,
  ")"
  ]
