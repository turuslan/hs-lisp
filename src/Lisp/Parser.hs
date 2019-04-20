module Lisp.Parser where

import Lisp.Ast
import Lisp.Parser.Number

import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

import qualified Data.Functor.Identity



languageDef :: GenLanguageDef String u Data.Functor.Identity.Identity
languageDef = emptyDef
  { Token.identStart = ident
  , Token.identLetter = ident
  , Token.reservedOpNames = ["."]
  , Token.commentLine     = ";"
  }
  where ident = alphaNum <|> oneOf "_+-*/=<>!?[]&~@#$%^."
lexer :: Token.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Token.makeTokenParser languageDef

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

sexprs :: Parser [SExpr]
sexprs = sepBy sexpr whiteSpace

parens :: Parser a -> Parser a
parens = Token.parens lexer

sexpr :: Parser SExpr
sexpr =
  try ((parens $ return ()) >> return EmptyList)
  <|> parens list
  <|> liftM StringLiteral (Token.stringLiteral lexer)
  <|> try number
  <|> liftM Atom (Token.identifier lexer)
  where
    list = do
      car <- sexpr
      whiteSpace
      cdr <- try (char '.' >> lookAhead (oneOf " \r\n\t(\"") >> whiteSpace >> sexpr) <|> (lookAhead sexpr >> list) <|> return EmptyList
      return $ DottedPair car cdr

whileParser :: Parser [SExpr]
whileParser = do 
  whiteSpace
  es <- sexprs
  eof
  return es

parseStringS :: String -> Either ParseError [SExpr]
parseStringS = parse whileParser ""