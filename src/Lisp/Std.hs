module Lisp.Std where

import Text.Read

import Lisp.Ast
import Lisp.Eval
import Lisp.Utils

--
initState :: State
initState = State [] []
  [ ("nil", (EmptyList initPos))
  ]
  [ ("read-int", (parseArgs "()", lReadInt))
  , ("print", (parseArgs "(x)", lPrint))
  , ("princ", (parseArgs "(x)", lPrinc))
  , ("+", (parseArgs "(&rest xs)", lPlus))
  , ("-", (parseArgs "(num &rest xs)", lMinus))
  , ("/", (parseArgs "(num &rest xs)", lDiv))
  , ("=", (parseArgs "(a b)", lEq))
  , ("<", (parseArgs "(a b)", lLt))
  , (">", (parseArgs "(a b)", lGt))
  , (">=", (parseArgs "(a b)", lGe))
  , ("atomp", (parseArgs "(x)", lAtomp))
  , ("numberp", (parseArgs "(x)", lNumberp))
  , ("listp", (parseArgs "(x)", lListp))
  , ("cons", (parseArgs "(a b)", lCons))
  , ("car", (parseArgs "(x)", lCar))
  , ("cdr", (parseArgs "(x)", lCdr))
  , ("list", (parseArgs "(&rest xs)", lList))
  , ("floor", (parseArgs "(x)", lFloor))
  , ("append", (parseArgs "(&rest xs)", lAppend))
  , ("null", (parseArgs "(x)", lNull))
  , ("first", (parseArgs "(x)", lCar))
  , ("rest", (parseArgs "(x)", lCdr))
  , ("seq", (parseArgs "(&rest xs)", lSeq))
  ]



--
lReadInt :: Fun
lReadInt _ = do
  str <- evalRead
  case reads str :: [(Integer, String)] of
    [(v, _)] -> return $ IntegerLiteral v initPos
    _ -> evalError initPos ("Substring '" ++ show str ++ "' does not have integer syntax at position 0")

lPrint :: Fun
lPrint [(_, arg)] = do
  evalWrite $ showE arg
  return arg
lPrint _ = impossible

lPrinc :: Fun
lPrinc [(_, arg)] = do
  evalWritec $ showE arg
  return arg
lPrinc _ = impossible

showE :: SExpr -> String
showE (StringLiteral s _) = s
showE e = show e

lPlus :: Fun
lPlus [(_, args)] = reduce (numBinaryOp (+) (+)) (IntegerLiteral 0 initPos) args
lPlus _ = impossible

lDiv :: Fun
lDiv [(_, num@(IntegerLiteral 1 _)), (_, EmptyList _)] = return num
lDiv [(_, num), (_, EmptyList _)] = divOp (FloatLiteral 1 initPos) num
lDiv [(_, num), (_, xs)] = reduce divOp num xs
lDiv _ = impossible

lMinus :: Fun
lMinus [(_, num), (_, EmptyList _)] = numBinaryOp (-) (-) (IntegerLiteral 0 initPos) num
lMinus [(_, num), (_, xs)] = reduce (numBinaryOp (-) (-)) num xs
lMinus _ = impossible

lEq :: Fun
lEq [(_, a), (_, b)] = do
  ab <- coerce a b
  case ab of
    (IntegerLiteral a' _, IntegerLiteral b' _) -> return $ lBool (a' == b')
    (FloatLiteral a' _, FloatLiteral b' _) -> return $ lBool (a' == b')
    _ -> impossible
lEq _ = impossible

lLt :: Fun
lLt [(_, a), (_, b)] = compareOp (<) (<) a b
lLt _ = impossible

lGt :: Fun
lGt [(_, a), (_, b)] = compareOp (>) (>) a b
lGt _ = impossible

lGe :: Fun
lGe [(_, a), (_, b)] = compareOp (>=) (>=) a b
lGe _ = impossible

divOp :: SExpr -> SExpr -> Eval SExpr
divOp a b = do
  ab <- coerce a b
  case ab of
    (IntegerLiteral a' _, IntegerLiteral b' _) -> case divMod a' b' of
      (i, 0) -> return $ IntegerLiteral i initPos
      _ -> return $ FloatLiteral (fromInteger a' / fromInteger b') initPos
    (FloatLiteral a' _, FloatLiteral b' _) -> return $ FloatLiteral (a' / b') initPos
    _ -> impossible

lAtomp :: Fun
lAtomp [(_, arg)] = return $ case arg of
  DottedPair _ _ _ -> lFalse
  _ -> lTrue
lAtomp _ = impossible

lNumberp :: Fun
lNumberp [(_, arg)] = return $ case arg of
  IntegerLiteral _ _ -> lTrue
  FloatLiteral _ _ -> lTrue
  _ -> lFalse
lNumberp _ = impossible

lListp :: Fun
lListp [(_, arg)] = return $ case arg of
  EmptyList _ -> lTrue
  DottedPair _ _ _ -> lTrue
  _ -> lFalse
lListp _ = impossible

lCons :: Fun
lCons [(_, car), (_, cdr)] = return $ DottedPair car cdr initPos
lCons _ = impossible

lCar :: Fun
lCar [(_, arg)] = do
  isList arg
  return $ case arg of
    DottedPair car _ _ -> car
    _ -> EmptyList initPos
lCar _ = impossible

lCdr :: Fun
lCdr [(_, arg)] = do
  isList arg
  return $ case arg of
    DottedPair _ cdr _ -> cdr
    _ -> EmptyList initPos
lCdr _ = impossible

lList :: Fun
lList [(_, args)] = return args
lList _ = impossible

lFloor :: Fun
lFloor [(_, arg)] = do
  isNumber arg
  return $ case arg of
    IntegerLiteral _ _ -> arg
    FloatLiteral f pos -> IntegerLiteral (floor f) pos
    _ -> impossible
lFloor _ = impossible

lSeq :: Fun
lSeq [(_, xs)] = return $ last' xs where
  last' (DottedPair x (EmptyList _) _) = x
  last' (DottedPair _ xs' _) = last' xs'
  last' _ = EmptyList initPos
lSeq _ = impossible

lAppend :: Fun
lAppend [(_, args)] = concat' (EmptyList initPos) (argsToArray args)
    where
      argsToArray :: SExpr -> [SExpr]
      argsToArray (EmptyList pos) = [EmptyList pos]
      argsToArray (DottedPair car (EmptyList _) _) = [car]
      argsToArray (DottedPair car cdr _) = (car : argsToArray cdr)
      argsToArray e = [e]

      concat' :: SExpr -> [SExpr] -> Eval SExpr
      concat' (EmptyList pos) [] = do return $ EmptyList pos
      concat' (EmptyList _) (e:es) = concat' e es
      concat' e [] = do return $ e
      concat' (DottedPair car (EmptyList _) pos) (e:es) = do 
        e' <- concat' e es
        return $ DottedPair car e' pos
      concat' (DottedPair car cdr pos) l = do 
        e' <- concat' cdr l
        return $ DottedPair car e' pos
      concat' e _ = evalError (getPos e) ("'" ++ show e ++ "' is not a list")
lAppend _ = impossible

lNull :: Fun
lNull [(_, arg)] = do
  case arg of
    EmptyList _ -> return $ lBool True
    _ -> return $ lBool False
lNull _ = impossible

lParseInteger :: Fun
lParseInteger [(_, arg)] = do
  case arg of
    StringLiteral s pos -> do
      let n = readMaybe s :: Maybe Double
      case n of
        Nothing -> return $ EmptyList pos
        Just n' -> return $ IntegerLiteral (floor n') pos
    e -> evalError (getPos e) ("'" ++ show e ++ "' is not a string")
lParseInteger _ = impossible

--
coerce :: SExpr -> SExpr -> Eval (SExpr, SExpr)
coerce a@(IntegerLiteral _ _) b@(IntegerLiteral _ _) = return (a, b)
coerce a@(FloatLiteral _ _) b@(FloatLiteral _ _) = return (a, b)
coerce (IntegerLiteral a pos1) b@(FloatLiteral _ _) = return (FloatLiteral (fromInteger a) pos1, b)
coerce a@(FloatLiteral _ _) (IntegerLiteral b pos2) = return (a, FloatLiteral (fromInteger b) pos2)
coerce a b = isNumber a >> isNumber b >> return (EmptyList initPos, EmptyList initPos)

reduce :: (SExpr -> SExpr -> Eval SExpr) -> SExpr -> SExpr -> Eval SExpr
reduce f acc (DottedPair car cdr _) = do
  acc' <- f acc car
  reduce f acc' cdr
reduce _ acc _ = return acc

isNumber :: SExpr -> Eval ()
isNumber (IntegerLiteral _ _) = return ()
isNumber (FloatLiteral _ _) = return ()
isNumber a = evalError (getPos a) ("'" ++ show a ++ "' is not a number")

isList :: SExpr -> Eval ()
isList (EmptyList _) = return ()
isList (DottedPair _ _ _) = return ()
isList a = evalError (getPos a) ("'" ++ show a ++ "' is not a list")

numBinaryOp :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> SExpr -> SExpr -> Eval SExpr
numBinaryOp fi ff a b = do
  ab <- coerce a b
  case ab of
    (IntegerLiteral a' pos1, IntegerLiteral b' pos2) -> return $ IntegerLiteral (fi a' b') (mergePos pos1 pos2)
    (FloatLiteral a' pos1, FloatLiteral b' pos2) -> return $ FloatLiteral (ff a' b') (mergePos pos1 pos2)
    _ -> impossible

compareOp :: (Integer -> Integer -> Bool) -> (Double -> Double -> Bool) -> SExpr -> SExpr -> Eval SExpr
compareOp fi ff a b = do
  ab <- coerce a b
  return $ lBool $ case ab of
    (IntegerLiteral a' _, IntegerLiteral b' _) -> fi a' b'
    (FloatLiteral a' _, FloatLiteral b' _) -> ff a' b'
    _ -> impossible

lBool :: Bool -> SExpr
lBool True = lTrue
lBool False = lFalse

lTrue :: SExpr
lTrue = IntegerLiteral 1 initPos

lFalse :: SExpr
lFalse = EmptyList initPos
