module Lisp.Ast where

import Text.Parsec.Pos

import Lisp.Utils

data Pos = Pos SourcePos SourcePos deriving (Eq, Show)

data SExpr
  = Atom String Pos
  | IntegerLiteral Integer Pos
  | FloatLiteral Double Pos
  | StringLiteral String Pos
  | EmptyList Pos
  | DottedPair SExpr SExpr Pos
  deriving Eq

instance Show SExpr where
  show (Atom name _) = name
  show (IntegerLiteral value _) = show value
  show (FloatLiteral value _) = show value
  show (StringLiteral value _) = show value
  show (EmptyList _) = "()"
  show xs'@(DottedPair _ _ _) = "(" ++ f xs' ++ ")"
    where
      f (DottedPair x xs _) = show x ++ g xs
      f _ = impossible
      g (EmptyList _) = ""
      g xs@(DottedPair _ _ _) = " " ++ f xs
      g x = " . " ++ show x

getPos :: SExpr -> Pos
getPos (Atom _ pos) = pos
getPos (IntegerLiteral _ pos) = pos
getPos (FloatLiteral _ pos) = pos
getPos (StringLiteral _ pos) = pos
getPos (EmptyList pos) = pos
getPos (DottedPair _ _ pos) = pos

setPos :: SExpr -> Pos -> SExpr
setPos (Atom a _) pos = Atom a pos
setPos (IntegerLiteral i _) pos = IntegerLiteral i pos
setPos (FloatLiteral f _) pos = FloatLiteral f pos
setPos (StringLiteral s _) pos = StringLiteral s pos
setPos (EmptyList _) pos = EmptyList pos
setPos (DottedPair car cdr _) pos = DottedPair car cdr pos

startPos :: Pos -> SourcePos
startPos (Pos sp _) = sp

endPos :: Pos -> SourcePos
endPos (Pos _ sp) = sp

mergePos :: Pos -> Pos -> Pos
mergePos (Pos p1 _) (Pos _ p2) = Pos p1 p2