module Lisp.Eval where

import Lisp.Ast
import Lisp.Parser (parseStringS)
import Lisp.Errors
import Lisp.Utils

import Control.Monad (liftM, ap)


import Text.Parsec.Pos

initPos :: Pos
initPos = Pos (initialPos "") (initialPos "") -- TODO: right position

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

evalError :: Pos -> String -> Eval a
evalError p str = Eval (\s -> (s, Left $ Right $ LispError p str))




get :: Eval State
get = Eval (\s -> (s, Left (Left s)))

levDistance :: String -> String -> Int
levDistance "" "" = 0
levDistance "" ys = length ys
levDistance xs "" = length xs
levDistance (x:xs) (y:ys)
  | x == y    = levDistance xs ys
  | otherwise = 1 + minimum [levDistance xs (y:ys)
                            , levDistance (x:xs) ys
                            , levDistance xs ys]

findSuggestions :: String -> [String] -> [String]
findSuggestions name fnames = filter (\n -> 2 > levDistance name n) fnames

errorFunctionNotFound :: String -> Eval String
errorFunctionNotFound name = errUnknownIdentifier name "function" sFuns

errorVariableNotFound :: String -> Eval String
errorVariableNotFound name = errUnknownIdentifier name "variable" sVars

errUnknownIdentifier :: String -> String -> (State -> Lookup a) -> Eval String
errUnknownIdentifier name t locals = do
  s <- get
  let names = map fst (locals s)
      suggestions = findSuggestions name names
      sMsg = case (length suggestions) of
        0 -> "No suggestions."
        _ -> "'Perhaps you meant one of these: " ++ unwords (map ("\n- "++) suggestions) ++ "'."
  return $ unlines
    [ "Unkown " ++ t ++ " '" ++ name ++ "'."
    , sMsg
    ]


--
eval :: Vars -> SExpr -> Eval SExpr
eval locals e = case e of
  Atom name pos -> do
    case lookup name locals of
      Just var -> return var
      _ -> do
        mvar <- evalVar name
        case mvar of
          Just var -> return var
          _ -> do
            msg <- errorVariableNotFound name
            evalError pos msg
  DottedPair (Atom name pos) aargs _ -> do
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
            expr <- fun args
            return $ setPos expr pos--(mergePos pos (getPos expr))--fun args
          _ -> do
            msg <- errorFunctionNotFound name
            evalError pos msg
  DottedPair car _ _ -> evalError (getPos car) ("'" ++ show car ++ "' is not a function name; try using a symbol instead")
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
evalArgs locals (DottedPair car cdr pos) = do
  car' <- eval locals car
  cdr' <- evalArgs locals cdr
  return $ DottedPair car' cdr' pos
evalArgs _ e = return (EmptyList $ getPos e)

getArgs :: String -> SExpr -> SExpr -> Eval Vars
getArgs _ (EmptyList _) (EmptyList _) = return []
getArgs fname (EmptyList pos) _ = evalError pos ("Too many arguments given to '" ++ fname ++ "'")

getArgs _ (DottedPair (Atom "&optional" _) fargs _) aargs = return $ getOpt fargs aargs where
  getOpt (DottedPair (Atom aname pos1) fcdr pos2) (EmptyList _) = (aname, (EmptyList pos1)) : getOpt fcdr (EmptyList pos2)
  getOpt (DottedPair (Atom aname _) fcdr _) (DottedPair acar acdr _) = (aname, acar) : getOpt fcdr acdr
  getOpt _ _ = []

getArgs _ (DottedPair (Atom "&rest" _) (DottedPair (Atom aname _) (EmptyList _) _) _) aargs = return [(aname, aargs)]
getArgs fname (DottedPair (Atom "&rest" _) _ pos) _ = evalError pos ("Wrong '&rest' arguments provided to '" ++ fname ++ "'")

getArgs fname (DottedPair (Atom aname _) fcdr _) (DottedPair acar acdr _) = do
  args <- getArgs fname fcdr acdr
  return ((aname, acar) : args)
getArgs fname (DottedPair _ _ pos) _ = evalError pos ("Too few arguments given to function '" ++ fname ++ "'")

getArgs fname a _ = evalError (getPos a) ("getArgs '" ++ fname ++ "' not implemented") -- TODO: think



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
    (EmptyList _) -> eval locals else'
    _ -> eval locals then'
lIf _ _ = impossible

lDefun :: Special
lDefun locals [(_, Atom name pos), (_, fargs), (_, body)] =
  Eval (\s -> (s {sFuns = (name, (fargs, f)) : sFuns s}, Left $ Left $ Atom name pos))
    where f args = eval (locals ++ args) body
lDefun _ _ = evalError initPos ("Function name provided to 'defun' must be identifier") -- TODO: think

lSetq :: Special
lSetq locals [(_, Atom name _), (_, value)] = do
  value' <- eval locals value
  Eval (\s -> (s {sVars = (name, value') : sVars s}, Left $ Left value'))
lSetq _ _ = evalError initPos ("Variable name provided to 'set' must be identifier") -- TODO: think

lLet :: Special
lLet locals [(_, DottedPair (DottedPair (Atom name _) (DottedPair value (EmptyList _) _) _) (EmptyList _) _), (_, body)] = do
  value' <- eval locals value
  eval ((name, value'):locals) body
lLet _ _ = evalError initPos ("No 'body' arguments provided to function 'let'") -- TODO: think


parseArgs :: String -> SExpr
parseArgs s = case parseStringS s of
    Left e -> customError (show e)
    Right r -> head r
