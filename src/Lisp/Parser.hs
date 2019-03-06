module Lisp.Parser where

import Lisp.Ast
import Lisp.Parser.Number

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import qualified Data.Functor.Identity



languageDef :: GenLanguageDef String u Data.Functor.Identity.Identity
languageDef = emptyDef
  { Token.identStart = ident
  , Token.identLetter = ident
  , Token.reservedOpNames = ["."]
  }
  where ident = alphaNum <|> oneOf "_+-*/=<>!?[]&~@#$%^."
lexer :: Token.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Token.makeTokenParser languageDef

sexprs :: Parser [SExpr]
sexprs = sepBy sexpr (Token.whiteSpace lexer)

sexpr :: Parser SExpr
sexpr =
  (Token.parens lexer) list
  <|> liftM StringLiteral (Token.stringLiteral lexer)
  <|> try number
  <|> liftM Atom (Token.identifier lexer)
  where
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
