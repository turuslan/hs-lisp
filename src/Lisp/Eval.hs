module Lisp.Eval where

import Lisp.Ast



-- TODO: context type
data Context

eval :: Context -> SExpr -> SExpr
eval c (DottedPair (Atom "+") args) = eval__plus $ eval_args c args
eval _ s = s

eval__plus :: [SExpr] -> SExpr
eval__plus args
  | any is_float args = FloatLiteral $ sum $ map to_float args
  | otherwise = IntegerLiteral $ sum $ map to_integer args

to_float :: SExpr -> Double
to_float (FloatLiteral value) = value
to_float (IntegerLiteral value) = fromInteger value
to_float _ = todo_runtime_error

to_integer :: SExpr -> Integer
to_integer (IntegerLiteral value) = value
to_integer _ = todo_runtime_error

is_float :: SExpr -> Bool
is_float (FloatLiteral _) = True
is_float _ = False

eval_args :: Context -> SExpr -> [SExpr]
eval_args c (DottedPair car cdr) = eval c car : eval_args c cdr
eval_args _ _ = []

-- TODO: runtime error
todo_runtime_error :: a
todo_runtime_error = error "TODO"



-- _lisp_*

_lisp_bool :: Bool -> SExpr
_lisp_bool True = _lisp_true
_lisp_bool False = _lisp_false

_lisp_false :: SExpr
_lisp_false = EmptyList

_lisp_true :: SExpr
_lisp_true = lisp_T



-- lisp_*

lisp_T :: SExpr
lisp_T = Atom "T"

lisp_atomp :: SExpr -> SExpr
lisp_atomp (DottedPair _ _) = _lisp_false
lisp_atomp _ = _lisp_true

lisp_numberp :: SExpr -> SExpr
lisp_numberp (IntegerLiteral _) = _lisp_true
lisp_numberp (FloatLiteral _) = _lisp_true
lisp_numberp _ = _lisp_false

lisp_listp :: SExpr -> SExpr
lisp_listp EmptyList = _lisp_true
lisp_listp (DottedPair _ _) = _lisp_true
lisp_listp _ = _lisp_false

lisp__eq :: SExpr -> SExpr -> SExpr
lisp__eq (IntegerLiteral x) (IntegerLiteral y) = _lisp_bool (x == y)
lisp__eq (FloatLiteral x) (FloatLiteral y) = _lisp_bool (x == y)
lisp__eq (IntegerLiteral x) (FloatLiteral y) = _lisp_bool (fromIntegral x == y)
lisp__eq (FloatLiteral x) (IntegerLiteral y) = _lisp_bool (x == fromIntegral y)
lisp__eq _ _ = todo_runtime_error

lisp_cons :: SExpr -> SExpr -> SExpr
lisp_cons car cdr = DottedPair car cdr

lisp_car :: SExpr -> SExpr
lisp_car (DottedPair car _) = car
lisp_car _ = todo_runtime_error

lisp_cdr :: SExpr -> SExpr
lisp_cdr (DottedPair _ cdr) = cdr
lisp_cdr _ = todo_runtime_error
