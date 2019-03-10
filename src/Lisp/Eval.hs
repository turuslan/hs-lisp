module Lisp.Eval where

import Lisp.Ast
import Lisp.Parser (parseString)

import Control.Monad (liftM, ap)



-- monad
type Lookup a = [(String, a)]

type Fun = Vars -> Eval SExpr

type Special = Vars -> Fun

type Vars = Lookup SExpr

data State = State
  { sPendingOutput :: [String]
  , sPendingInput :: [String]
  , sVars :: Vars
  , sFuns :: Lookup (SExpr, Fun)
  }

instance Show State where
  show s = "State {sVars = " ++ show (sVars s) ++ ", sFuns = " ++ (show $ map fst $ sFuns s) ++ "}"

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
    case lookup name specials of
      Just (fargs, special) -> do
        args <- get_args name fargs aargs
        special locals args
      _ -> do
        mfun <- eval_fun name
        case mfun of
          Just (fargs, fun) -> do
            aargs' <- eval_args locals aargs
            args <- get_args name fargs aargs'
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
    s'' <- flush s'
    line <- getLine
    evalIO s'' {sPendingInput = [line]} pc
  where
    flush s'' = (putStr $ unlines $ reverse $ sPendingOutput s'') >> return s'' {sPendingOutput = []}

eval_args :: Vars -> SExpr -> Eval SExpr
eval_args locals (DottedPair car cdr) = do
  car' <- eval locals car
  cdr' <- eval_args locals cdr
  return $ DottedPair car' cdr'
eval_args _ _ = return EmptyList

get_args :: String -> SExpr -> SExpr -> Eval Vars
get_args _ EmptyList EmptyList = return []
get_args fname EmptyList _ = eval_error ("too many arguments given to " ++ fname)

get_args _ (DottedPair (Atom "&optional") fargs) aargs = return $ get_opt fargs aargs where
  get_opt (DottedPair (Atom aname) fcdr) EmptyList = (aname, EmptyList) : get_opt fcdr EmptyList
  get_opt (DottedPair (Atom aname) fcdr) (DottedPair acar acdr) = (aname, acar) : get_opt fcdr acdr
  get_opt _ _ = []

get_args _ (DottedPair (Atom "&rest") (DottedPair (Atom aname) EmptyList)) aargs = return [(aname, aargs)]
get_args fname (DottedPair (Atom "&rest") _) _ = eval_error ("TODO: MSG: bad &rest in " ++ fname)

get_args fname (DottedPair (Atom aname) fcdr) (DottedPair acar acdr) = do
  args <- get_args fname fcdr acdr
  return ((aname, acar) : args)
get_args fname (DottedPair _ _) _ = eval_error ("too few arguments given to " ++ fname)

get_args fname _ _ = eval_error ("get_args " ++ fname ++ " not implemented")



--
specials :: Lookup (SExpr, Special)
specials =
  [ ("if", (parse_args "(cnd then &optional else)", special_if))
  , ("defun", (parse_args "(name args body)", special_defun))
  , ("setq", (parse_args "(nane value)", special_setq))
  ]

special_if :: Special
special_if locals [(_, cnd), (_, then'), (_, else')] = do
  cnd' <- eval locals cnd
  case cnd' of
    EmptyList -> eval locals else'
    _ -> eval locals then'
special_if _ _ = impossible

special_defun :: Special
special_defun locals [(_, Atom name), (_, fargs), (_, body)] =
  Eval (\s -> (s {sFuns = (name, (fargs, f)) : sFuns s}, Left $ Left $ Atom name))
    where f args = eval (locals ++ args) body
special_defun _ _ = eval_error "TODO: MSG: function name must be symbol"

special_setq :: Special
special_setq locals [(_, Atom name), (_, value)] = do
  value' <- eval locals value
  Eval (\s -> (s {sVars = (name, value') : sVars s}, Left $ Left value'))
special_setq _ _ = eval_error "TODO: MSG: var name must be symbol"



-- TODO: runtime error
todo_runtime_error :: a
todo_runtime_error = error "TODO"

impossible :: a
impossible = error "impossible"

parse_args :: String -> SExpr
parse_args = head . parseString
