module Lisp.Parser.Number where

import Lisp.Ast

import Text.Parsec
import Text.Parsec.String



-- did not use `Text.ParsecCombinators.Parsec.Token.naturalOrFloat` because:
--   - does not handle sign;
--   - consumes whitespace after number;
--   - does not accept numbers like "1." and ".1";
number :: Parser SExpr
number = do
  n <- intOrFloat
  lookAhead ((oneOf " ()" >> return ()) <|> eof)
  return n
  where
    sign = (char '-' >> return "-")
      <|> (char '+' >> return "")
      <|> return ""

    intOrFloat :: Parser SExpr
    intOrFloat = do
      sign' <- sign
      (char '.' >> lookAhead digit >> frEx (sign' ++ "0."))
        <|> intFrEx sign'

    intFrEx :: String -> Parser SExpr
    intFrEx prefix = do
      int' <- many1 digit
      (char '.' >> (
        (oneOf "eE" >> ex (prefix ++ int' ++ ".0e"))
        <|> (lookAhead digit >> frEx (prefix ++ int' ++ "."))
        <|> double (prefix ++ int')
        ))
        <|> (oneOf "eE" >> ex (prefix ++ int' ++ "e"))
        <|> int (prefix ++ int')

    int :: String -> Parser SExpr
    int prefix = case reads prefix :: [(Integer, String)] of
      [(value, "")] -> return $ IntegerLiteral value
      _ -> parserZero

    frEx :: String -> Parser SExpr
    frEx prefix = do
      fr <- many1 digit
      (oneOf "eE" >> ex (prefix ++ fr ++ "e"))
        <|> double (prefix ++ fr)

    ex :: String -> Parser SExpr
    ex prefix = do
      sign' <- sign
      ex' <- many1 digit
      double (prefix ++ sign' ++ ex')

    double :: String -> Parser SExpr
    double prefix = case reads prefix :: [(Double, String)] of
      [(value, "")] -> return $ FloatLiteral value
      _ -> parserZero
