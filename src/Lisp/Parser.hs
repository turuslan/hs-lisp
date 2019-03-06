module Lisp.Parser where

import Lisp.Ast

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import qualified Data.Functor.Identity



languageDef :: GenLanguageDef String u Data.Functor.Identity.Identity
languageDef = emptyDef
  { Token.identStart = letter
  , Token.identLetter = alphaNum
  , Token.reservedOpNames = ["."]
  }
lexer :: Token.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Token.makeTokenParser languageDef

sexprs :: Parser [SExpr]
sexprs = sepBy sexpr (Token.whiteSpace lexer)

sexpr :: Parser SExpr
sexpr =
  liftM Atom (Token.identifier lexer)
  <|> number
  <|> (Token.parens lexer) list
  <|> liftM StringLiteral (Token.stringLiteral lexer)
  where
    number = do
      neg <- (Token.lexeme lexer) ((char '-' >> return True) <|> (char '+' >> return False) <|> return False)
      n <- Token.naturalOrFloat lexer
      case n of
        Left v -> return $ IntegerLiteral (if neg then (-v) else v)
        Right v -> return $ FloatLiteral (if neg then (-v) else v)
    list = do
      before_dot <- sexprs
      after_dot <- (Token.reservedOp lexer "." >> sexpr) <|> return EmptyList
      return $ foldr DottedPair after_dot before_dot

whileParser :: Parser [SExpr]
whileParser = (Token.whiteSpace lexer) >> sexprs

parseString :: String -> [SExpr]
parseString s = case parse whileParser "" s of
  Left e -> error $ show e
  Right r -> r
