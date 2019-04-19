module Lisp.Std where

import Text.Read

import Lisp.Ast
import Lisp.Eval



-- | Initial state.
-- Constains standard declared functions and variables.
initState :: State
initState = State [] []
  [ ("nil", EmptyList)
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
  , ("map", (parseArgs "(&rest xs)", lMap))
  , ("lookup", (parseArgs "(y x)", lLookup))
  ]



-- | Standard functions implementation.

lReadInt :: Fun
lReadInt _ = do
  str <- evalRead
  case reads str :: [(Integer, String)] of
    [(v, _)] -> return $ IntegerLiteral v
    _ -> evalError ("substring " ++ show str ++ " does not have integer syntax at position 0")

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
showE (StringLiteral s) = s
showE e = show e

lPlus :: Fun
lPlus [(_, args)] = reduce (numBinaryOp (+) (+)) (IntegerLiteral 0) args
lPlus _ = impossible

lDiv :: Fun
lDiv [(_, num@(IntegerLiteral 1)), (_, EmptyList)] = return num
lDiv [(_, num), (_, EmptyList)] = divOp (FloatLiteral 1) num
lDiv [(_, num), (_, xs)] = reduce divOp num xs
lDiv _ = impossible

lMinus :: Fun
lMinus [(_, num), (_, EmptyList)] = numBinaryOp (-) (-) (IntegerLiteral 0) num
lMinus [(_, num), (_, xs)] = reduce (numBinaryOp (-) (-)) num xs
lMinus _ = impossible

lEq :: Fun
lEq [(_, a), (_, b)] = do
  ab <- coerce a b
  case ab of
    (IntegerLiteral a', IntegerLiteral b') -> return $ lBool (a' == b')
    (FloatLiteral a', FloatLiteral b') -> return $ lBool (a' == b')
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
    (IntegerLiteral a', IntegerLiteral b') -> case divMod a' b' of
      (i, 0) -> return $ IntegerLiteral i
      _ -> return $ FloatLiteral (fromInteger a' / fromInteger b')
    (FloatLiteral a', FloatLiteral b') -> return $ FloatLiteral (a' / b')
    _ -> impossible

lAtomp :: Fun
lAtomp [(_, arg)] = return $ case arg of
  DottedPair _ _ -> lFalse
  _ -> lTrue
lAtomp _ = impossible

lNumberp :: Fun
lNumberp [(_, arg)] = return $ case arg of
  IntegerLiteral _ -> lTrue
  FloatLiteral _ -> lTrue
  _ -> lFalse
lNumberp _ = impossible

lListp :: Fun
lListp [(_, arg)] = return $ case arg of
  EmptyList -> lTrue
  DottedPair _ _ -> lTrue
  _ -> lFalse
lListp _ = impossible

lCons :: Fun
lCons [(_, car), (_, cdr)] = return $ DottedPair car cdr
lCons _ = impossible

lCar :: Fun
lCar [(_, arg)] = do
  isList arg
  return $ case arg of
    DottedPair car _ -> car
    _ -> EmptyList
lCar _ = impossible

lCdr :: Fun
lCdr [(_, arg)] = do
  isList arg
  return $ case arg of
    DottedPair _ cdr -> cdr
    _ -> EmptyList
lCdr _ = impossible

lList :: Fun
lList [(_, args)] = return args
lList _ = impossible

lFloor :: Fun
lFloor [(_, arg)] = do
  isNumber arg
  return $ case arg of
    IntegerLiteral _ -> arg
    FloatLiteral f -> IntegerLiteral $ floor f
    _ -> impossible
lFloor _ = impossible

lSeq :: Fun
lSeq [(_, xs)] = return $ last' xs where
  last' (DottedPair x EmptyList) = x
  last' (DottedPair _ xs') = last' xs'
  last' _ = EmptyList
lSeq _ = impossible

lAppend :: Fun
lAppend [(_, args)] = concat' EmptyList (argsToArray args)
    where
      argsToArray :: SExpr -> [SExpr]
      argsToArray EmptyList = [EmptyList]
      argsToArray (DottedPair car EmptyList) = [car]
      argsToArray (DottedPair car cdr) = (car : argsToArray cdr)
      argsToArray e = [e]

      concat' :: SExpr -> [SExpr] -> Eval SExpr
      concat' EmptyList [] = do return $ EmptyList
      concat' EmptyList (e:es) = concat' e es
      concat' e [] = do return $ e
      concat' (DottedPair car EmptyList) (e:es) = do 
        e' <- concat' e es
        return $ DottedPair car e'
      concat' (DottedPair car cdr) l = do 
        e' <- concat' cdr l
        return $ DottedPair car e'
      concat' e _ = evalError (show e ++ " is not a list")
lAppend _ = impossible

lNull :: Fun
lNull [(_, arg)] = do
  case arg of
    EmptyList -> return $ lBool True
    _ -> return $ lBool False
lNull _ = impossible

lParseInteger :: Fun
lParseInteger [(_, arg)] = do
  case arg of
    StringLiteral s -> do
      let n = readMaybe s :: Maybe Double
      case n of
        Nothing -> return $ EmptyList
        Just n' -> return $ IntegerLiteral (floor n')
    e -> evalError (show e ++ " is not a string")
lParseInteger _ = impossible



-- | Utility functions.

-- | Convert types of Lisp.Ast numbers to common type.
-- Result is float if one of numbers if float.
-- Required for arithmetic operations, but division is handled differently.
coerce :: SExpr -> SExpr -> Eval (SExpr, SExpr)
coerce a@(IntegerLiteral _) b@(IntegerLiteral _) = return (a, b)
coerce a@(FloatLiteral _) b@(FloatLiteral _) = return (a, b)
coerce (IntegerLiteral a) b@(FloatLiteral _) = return (FloatLiteral $ fromInteger a, b)
coerce a@(FloatLiteral _) (IntegerLiteral b) = return (a, FloatLiteral $ fromInteger b)
coerce a b = isNumber a >> isNumber b >> return (EmptyList, EmptyList)

-- | Folds Lisp.Ast list.
reduce :: (SExpr -> SExpr -> Eval SExpr) -> SExpr -> SExpr -> Eval SExpr
reduce f acc (DottedPair car cdr) = do
  acc' <- f acc car
  reduce f acc' cdr
reduce _ acc _ = return acc

-- | Lisp.Ast number check assertion.
-- Uses error of "Eval" monad.
isNumber :: SExpr -> Eval ()
isNumber (IntegerLiteral _) = return ()
isNumber (FloatLiteral _) = return ()
isNumber a = evalError (show a ++ " is not a number")

-- | Lisp.Ast list check assertion.
-- Uses error of "Eval" monad.
isList :: SExpr -> Eval ()
isList EmptyList = return ()
isList (DottedPair _ _) = return ()
isList a = evalError (show a ++ " is not a list")

-- | Apply binary arithmetic operation to pair of Lisp.Ast numbers.
numBinaryOp :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> SExpr -> SExpr -> Eval SExpr
numBinaryOp fi ff a b = do
  ab <- coerce a b
  case ab of
    (IntegerLiteral a', IntegerLiteral b') -> return $ IntegerLiteral $ fi a' b'
    (FloatLiteral a', FloatLiteral b') -> return $ FloatLiteral $ ff a' b'
    _ -> impossible

-- | Apply binary comparison operation to pair of Lisp.Ast numbers.
compareOp :: (Integer -> Integer -> Bool) -> (Double -> Double -> Bool) -> SExpr -> SExpr -> Eval SExpr
compareOp fi ff a b = do
  ab <- coerce a b
  return $ lBool $ case ab of
    (IntegerLiteral a', IntegerLiteral b') -> fi a' b'
    (FloatLiteral a', FloatLiteral b') -> ff a' b'
    _ -> impossible

lBool :: Bool -> SExpr
lBool True = lTrue
lBool False = lFalse

lTrue :: SExpr
lTrue = IntegerLiteral 1

lFalse :: SExpr
lFalse = EmptyList

-- | Internal method for converting DottedList to Haskel list or lisp map
toMap :: SExpr -> [SExpr]
toMap EmptyList = []
toMap (DottedPair x xs) = x : toMap xs
toMap x = [x]

-- | Lisp.Ast Map check assertion.
-- Uses error of "Eval" monad.
isMap :: SExpr -> Eval ()
isMap xs = do
  isList xs
  if length (toMap xs) `mod` 2 /= 0 
    then evalError (show xs ++ " is not a map")
    else return ()

lMap :: Fun
lMap [(_, args)] = check
  where
    ar = toMap args
    check
      | length ar `mod` 2 /= 0 = impossible
      | otherwise = return args
lMap _ = impossible

findInMap :: SExpr -> [SExpr] -> SExpr
findInMap key map = findRecursively map
  where
    findRecursively [] = EmptyList
    findRecursively (k:v:xs)
      | k == key = v
      | otherwise = findRecursively xs
    findRecursively xs = impossible

lLookup :: Fun
lLookup [(_, k), (_, args)] = look k args
  where
    look _ EmptyList = return EmptyList
    look key lst = do
      isMap lst
      return $ findInMap key $ toMap lst

-- Algorithms using map
-- use hashmap