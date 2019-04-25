module Lisp.Std where

import Text.Read
import Data.Maybe
import Data.List.Split

import Lisp.Ast
import Lisp.Eval
import Lisp.Utils

-- | Initial state.
-- Constains standard declared functions and variables.
initState :: State
initState = State [] []
  [ ("nil", (EmptyList initPos))
  ]
  [ ("read-int", (parseArgs "()", lReadInt))
  , ("read", (parseArgs "()", lReadStr))
  , ("concat", (parseArgs "(x y)", lConcat))
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
  , ("put", (parseArgs "(k v x)", lPutToMap))
  , ("removeKey", (parseArgs "(k x)", lRemoveKey))
  , ("split", (parseArgs "(d s)", lSplit))
  ]



-- | Standard functions implementation.

lReadInt :: Fun
lReadInt _ = do
  str <- evalRead
  case reads str :: [(Integer, String)] of
    [(v, _)] -> return $ IntegerLiteral v initPos
    _ -> evalError initPos ("Substring '" ++ show str ++ "' does not have integer syntax at position 0")


lReadStr :: Fun
lReadStr _ = do
  str <- evalRead
  return $ StringLiteral str initPos

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

lConcat :: Fun
lConcat [(_, a), (_, b)] = return $ concatS a b
  where
    concatS (StringLiteral s1 pos1) (StringLiteral s2 pos2) = StringLiteral (s1 ++ s2) (mergePos pos1 pos2)
    concatS _ _ = EmptyList $ mergePos (getPos a) (getPos b)
lConcat _ = impossible

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
  case (a, b) of
    (StringLiteral a' _, StringLiteral b' _) -> return $ lBool (a' == b')
    _ -> do
      ab <- coerce a b
      case ab of
        (IntegerLiteral a' _, IntegerLiteral b' _) -> return $ lBool (a' == b')
        (FloatLiteral a' _, FloatLiteral b' _) -> return $ lBool (a' == b')
        (StringLiteral a' _, StringLiteral b' _) -> return $ lBool (a' == b')
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



-- | Utility functions.

-- | Convert types of Lisp.Ast numbers to common type.
-- Result is float if one of numbers if float.
-- Required for arithmetic operations, but division is handled differently.
coerce :: SExpr -> SExpr -> Eval (SExpr, SExpr)
coerce a@(IntegerLiteral _ _) b@(IntegerLiteral _ _) = return (a, b)
coerce a@(FloatLiteral _ _) b@(FloatLiteral _ _) = return (a, b)
coerce (IntegerLiteral a pos1) b@(FloatLiteral _ _) = return (FloatLiteral (fromInteger a) pos1, b)
coerce a@(FloatLiteral _ _) (IntegerLiteral b pos2) = return (a, FloatLiteral (fromInteger b) pos2)
coerce a b = isNumber a >> isNumber b >> return (EmptyList initPos, EmptyList initPos)

-- | Folds Lisp.Ast list.
reduce :: (SExpr -> SExpr -> Eval SExpr) -> SExpr -> SExpr -> Eval SExpr
reduce f acc (DottedPair car cdr _) = do
  acc' <- f acc car
  reduce f acc' cdr
reduce _ acc _ = return acc

-- | Lisp.Ast number check assertion.
-- Uses error of "Eval" monad.
isNumber :: SExpr -> Eval ()
isNumber (IntegerLiteral _ _) = return ()
isNumber (FloatLiteral _ _) = return ()
isNumber a = evalError (getPos a) ("'" ++ show a ++ "' is not a number")

-- | Lisp.Ast list check assertion.
-- Uses error of "Eval" monad.
isList :: SExpr -> Eval ()
isList (EmptyList _) = return ()
isList (DottedPair _ _ _) = return ()
isList a = evalError (getPos a) ("'" ++ show a ++ "' is not a list")

-- | Apply binary arithmetic operation to pair of Lisp.Ast numbers.
numBinaryOp 
  :: (Integer -> Integer -> Integer)
  -> (Double -> Double -> Double)
  -> SExpr -> SExpr -> Eval SExpr
numBinaryOp fi ff a b = do
  ab <- coerce a b
  case ab of
    (IntegerLiteral a' pos1, IntegerLiteral b' pos2) -> return $ IntegerLiteral (fi a' b') (mergePos pos1 pos2)
    (FloatLiteral a' pos1, FloatLiteral b' pos2) -> return $ FloatLiteral (ff a' b') (mergePos pos1 pos2)
    _ -> impossible

-- | Apply binary comparison operation to pair of Lisp.Ast numbers.
compareOp 
  :: (Integer -> Integer -> Bool) 
  -> (Double -> Double -> Bool)
  -> SExpr -> SExpr -> Eval SExpr
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

-- | Implementation of split command
lSplit :: Fun
lSplit [(_, d), (_, s)] = splitS d s
  where
    splitS (StringLiteral ds _) (StringLiteral ss _) = 
      return
        $ toLispList 
        $ map (\str -> StringLiteral str initPos)
        $ splitOn ds ss
    splitS a1 a2 = evalError (mergePos (getPos a1) (getPos a2)) "'split' can be applied only to strings"
lSplit _ = impossible

-- | Internal method for converting DottedList to Haskel list or lisp map
toHsList :: SExpr -> [SExpr]
toHsList (EmptyList _) = []
toHsList (DottedPair x xs _) = x : toHsList xs
toHsList x = [x]

-- | Lisp.Ast Map check assertion.
-- Uses error of "Eval" monad.
isMap :: SExpr -> Eval ()
isMap xs = do
  isList xs
  if length (toHsList xs) `mod` 2 /= 0 
    then evalError (getPos xs) ("'" ++ show xs ++ "' is not a map")
    else return ()

-- | Lisp Map constructor
lMap :: Fun
lMap [(_, args)] = do
  isMap args
  return args
lMap _ = impossible

-- | Internal function for searching within lisp map
findInMap :: SExpr -> [SExpr] -> Maybe SExpr
findInMap key = findRecursively
  where
    findRecursively [] = Nothing
    findRecursively (k:v:xs)
      | k == key = Just v
      | otherwise = findRecursively xs
    findRecursively _ = impossible

-- | Internal function for inserting to lisp map
insertToMap :: SExpr -> SExpr -> [SExpr] -> SExpr
insertToMap key newVal mp = toLispList $ insertRec mp
  where
    insertRec [] = [key, newVal]
    insertRec (k:v:xs)
      | k == key = k:newVal:xs
      | otherwise = k:v:insertRec xs
    insertRec _ = impossible

removeFromMap :: SExpr -> [SExpr] -> SExpr
removeFromMap key mp = toLispList $ removeRec mp
  where
    removeRec [] = []
    removeRec (k:v:xs)
      | k == key = xs
      | otherwise = k:v:removeRec xs
    removeRec _ = impossible

-- | Internal function to converting haskell list to lisp list
toLispList :: [SExpr] -> SExpr
toLispList = foldr (\car cdr -> DottedPair car cdr initPos) (EmptyList initPos)

-- | Lisp 'lookup' function implementation for lisp map.
-- Finds value by 'key' or returns EmptyList
lLookup :: Fun
lLookup [(_, k), (_, args)] = lookA args
  where
    lookA l@(EmptyList _) = return l
    lookA lst = do
      isMap lst
      return $ fromMaybe (EmptyList initPos) (findInMap k $ toHsList lst)
lLookup _ = impossible

-- | Lisp 'put' function implementation.
lPutToMap :: Fun
lPutToMap [(_, k), (_, v), (_, mp)] = do
  isMap mp
  return $ insertToMap k v $ toHsList mp
lPutToMap _ = impossible

-- | Lisp 'removeKey' function implementation
lRemoveKey :: Fun
lRemoveKey [(_, k), (_, mp)] = do
    isMap mp
    return $ removeFromMap k $ toHsList mp
lRemoveKey _ = impossible
