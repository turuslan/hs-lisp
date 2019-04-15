module Lisp.Ast where

import Lisp.Errors

data SExpr
  = Atom String
  | IntegerLiteral Integer
  | FloatLiteral Double
  | StringLiteral String
  | EmptyList
  | DottedPair SExpr SExpr
  deriving Eq

instance Show SExpr where
  show (Atom name) = name
  show (IntegerLiteral value) = show value
  show (FloatLiteral value) = show value
  show (StringLiteral value) = show value
  show EmptyList = "()"
  show xs'@(DottedPair _ _) = "(" ++ f xs' ++ ")"
    where
      f (DottedPair x xs) = show x ++ g xs
      f _ = impossible
      g EmptyList = ""
      g xs@(DottedPair _ _) = " " ++ f xs
      g x = " . " ++ show x
