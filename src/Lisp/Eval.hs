module Lisp.Eval where

import Lisp.Ast

import Control.Monad (liftM, ap)



-- monad
type Lookup a = [(String, a)]

type Fun = Vars -> Eval SExpr

type Vars = Lookup SExpr

data State = State
  { sPendingOutput :: [String]
  , sPendingInput :: [String]
  , sVars :: Vars
  , sFuns :: Lookup (SExpr, Fun)
  }

instance Show State where
  show s = "State {sVars = " ++ show (sVars s) ++ "}"

data LispError = LispError String deriving Show

newtype Eval a = Eval (State -> (State, Either (Either a LispError) (Eval a)))

instance Functor Eval where
  fmap = liftM

instance Applicative Eval where
  pure  = return
  (<*>) = ap

instance Monad Eval where
  Eval c1 >>= fc2 = Eval (\s -> case c1 s of
    (s', Left (Right e)) -> (s', Left (Right e))
    (s', Left (Left v)) -> let Eval c2 = fc2 v in c2 s'
    (s', Right pc1) -> (s', Right (pc1 >>= fc2)))

  return v = Eval (\s -> (s, Left (Left v)))

eval_read :: Eval String
eval_read = e where
  e = Eval (\s -> case sPendingInput s of
    [] -> (s, Right e)
    x:xs -> (s {sPendingInput = xs}, Left $ Left x))

eval_write :: String -> Eval ()
eval_write str = Eval (\s -> (s {sPendingOutput = str : sPendingOutput s}, Left $ Left ()))

eval_var :: String -> Eval (Maybe SExpr)
eval_var name = Eval (\s -> (s, Left $ Left $ lookup name $ sVars s))

eval_fun :: String -> Eval (Maybe (SExpr, Fun))
eval_fun name = Eval (\s -> (s, Left $ Left $ lookup name $ sFuns s))

eval_error :: String -> Eval a
eval_error str = Eval (\s -> (s, Left $ Right $ LispError str))



--
eval :: Vars -> SExpr -> Eval SExpr
eval locals e = case e of
  Atom name -> do
    case lookup name locals of
      Just var -> return var
      _ -> do
        mvar <- eval_var name
        case mvar of
          Just var -> return var
          _ -> todo_runtime_error
  DottedPair (Atom name) aargs -> do
    mfun <- eval_fun name
    case mfun of
      Just (fargs, fun) -> do
        args <- get_args name fargs locals aargs
        fun args
      _ -> todo_runtime_error
  DottedPair _ _ -> todo_runtime_error
  _ -> return e

evalIO :: State -> Eval SExpr -> IO (State, Either SExpr LispError)
evalIO s (Eval c) = case c s of
  (s', Left r) -> do
    s'' <- flush s'
    return (s'', r)
  (s', Right pc) -> do
    line <- getLine
    evalIO s' {sPendingInput = [line]} pc
  where
    flush s'' = (putStr $ unlines $ sPendingOutput s'') >> return s'' {sPendingOutput = []}

get_args :: String -> SExpr -> Vars -> SExpr -> Eval Vars
get_args _ EmptyList _ EmptyList = return []
get_args fname EmptyList _ _ = eval_error ("too many arguments given to " ++ fname)
get_args fname (DottedPair (Atom aname) fcdr) locals (DottedPair acar acdr) = do
  args <- get_args fname fcdr locals acdr
  arg <- eval locals acar
  return ((aname, arg) : args)
get_args fname (DottedPair _ _) _ _ = eval_error ("too few arguments given to " ++ fname)
get_args fname _ _ _ = eval_error ("get_args " ++ fname ++ " not implemented")



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
