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

evalRead :: Eval String
evalRead = e where
  e = Eval (\s -> case sPendingInput s of
    [] -> (s, Right e)
    x:xs -> (s {sPendingInput = xs}, Left $ Left x))

evalWrite :: String -> Eval ()
evalWrite str = Eval (\s -> (s {sPendingOutput = str : sPendingOutput s}, Left $ Left ()))

evalWritec :: String -> Eval ()
evalWritec str = Eval (\s -> (
  s {sPendingOutput = case sPendingOutput s of
    [] -> [str]
    prefix:lines' -> (prefix ++ str) : lines'},
  Left $ Left ()))

evalVar :: String -> Eval (Maybe SExpr)
evalVar name = Eval (\s -> (s, Left $ Left $ lookup name $ sVars s))

evalFun :: String -> Eval (Maybe (SExpr, Fun))
evalFun name = Eval (\s -> (s, Left $ Left $ lookup name $ sFuns s))

evalError :: String -> Eval a
evalError str = Eval (\s -> (s, Left $ Right $ LispError str))



--
eval :: Vars -> SExpr -> Eval SExpr
eval locals e = case e of
  Atom name -> do
    case lookup name locals of
      Just var -> return var
      _ -> do
        mvar <- evalVar name
        case mvar of
          Just var -> return var
          _ -> evalError ("variable " ++ name ++ " has no value")
  DottedPair (Atom name) aargs -> do
    case lookup name specials of
      Just (fargs, special) -> do
        args <- getArgs name fargs aargs
        special locals args
      _ -> do
        mfun <- evalFun name
        case mfun of
          Just (fargs, fun) -> do
            aargs' <- evalArgs locals aargs
            args <- getArgs name fargs aargs'
            fun args
          _ -> evalError ("undefined function " ++ name)
  DottedPair car _ -> evalError (show car ++ "is not a function name; try using a symbol instead")
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

evalArgs :: Vars -> SExpr -> Eval SExpr
evalArgs locals (DottedPair car cdr) = do
  car' <- eval locals car
  cdr' <- evalArgs locals cdr
  return $ DottedPair car' cdr'
evalArgs _ _ = return EmptyList

getArgs :: String -> SExpr -> SExpr -> Eval Vars
getArgs _ EmptyList EmptyList = return []
getArgs fname EmptyList _ = evalError ("too many arguments given to " ++ fname)

getArgs _ (DottedPair (Atom "&optional") fargs) aargs = return $ getOpt fargs aargs where
  getOpt (DottedPair (Atom aname) fcdr) EmptyList = (aname, EmptyList) : getOpt fcdr EmptyList
  getOpt (DottedPair (Atom aname) fcdr) (DottedPair acar acdr) = (aname, acar) : getOpt fcdr acdr
  getOpt _ _ = []

getArgs _ (DottedPair (Atom "&rest") (DottedPair (Atom aname) EmptyList)) aargs = return [(aname, aargs)]
getArgs fname (DottedPair (Atom "&rest") _) _ = evalError ("TODO: MSG: bad &rest in " ++ fname)

getArgs fname (DottedPair (Atom aname) fcdr) (DottedPair acar acdr) = do
  args <- getArgs fname fcdr acdr
  return ((aname, acar) : args)
getArgs fname (DottedPair _ _) _ = evalError ("too few arguments given to " ++ fname)

getArgs fname _ _ = evalError ("getArgs " ++ fname ++ " not implemented")



--
specials :: Lookup (SExpr, Special)
specials =
  [ ("if", (parseArgs "(cnd then &optional else)", lIf))
  , ("defun", (parseArgs "(name args body)", lDefun))
  , ("setq", (parseArgs "(nane value)", lSetq))
  , ("let", (parseArgs "(vars body)", lLet))
  ]

lIf :: Special
lIf locals [(_, cnd), (_, then'), (_, else')] = do
  cnd' <- eval locals cnd
  case cnd' of
    EmptyList -> eval locals else'
    _ -> eval locals then'
lIf _ _ = impossible

lDefun :: Special
lDefun locals [(_, Atom name), (_, fargs), (_, body)] =
  Eval (\s -> (s {sFuns = (name, (fargs, f)) : sFuns s}, Left $ Left $ Atom name))
    where f args = eval (locals ++ args) body
lDefun _ _ = evalError "TODO: MSG: function name must be symbol"

lSetq :: Special
lSetq locals [(_, Atom name), (_, value)] = do
  value' <- eval locals value
  Eval (\s -> (s {sVars = (name, value') : sVars s}, Left $ Left value'))
lSetq _ _ = evalError "TODO: MSG: var name must be symbol"

lLet :: Special
lLet locals [(_, DottedPair (DottedPair (Atom name) (DottedPair value EmptyList)) EmptyList), (_, body)] = do
  value' <- eval locals value
  eval ((name, value'):locals) body
lLet _ _ = evalError "TODO: lLet"



impossible :: a
impossible = error "impossible"

parseArgs :: String -> SExpr
parseArgs = head . parseString
