module Lisp.Parser where

import Lisp.Ast
import Lisp.Parser.Number

import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

import qualified Data.Functor.Identity



-- | Boilerplate
languageDef :: GenLanguageDef String u Data.Functor.Identity.Identity
languageDef = emptyDef
  { Token.identStart = ident
  , Token.identLetter = ident
  , Token.reservedOpNames = ["."]
  , Token.commentLine     = ";"
  }
  where ident = alphaNum <|> oneOf "_+-*/=<>!?[]&~@#$%^."

-- | Boilerplate
lexer :: Token.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Token.makeTokenParser languageDef

-- | Parse whitespace.
whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

-- | Parse sequence of expressions.
sexprs :: Parser [SExpr]
sexprs = sepBy sexpr whiteSpace

-- | Parse parentheses.
parens :: Parser a -> Parser a
parens = Token.parens lexer

-- | Parse single expression.
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

-- | Boilerplate
whileParser :: Parser [SExpr]
whileParser = whiteSpace >> sexprs

-- | Safe farse string.
parseStringS :: String -> Either ParseError [SExpr]
parseStringS = parse whileParser ""

-- | Unsafe parse string.
parseString :: String -> [SExpr]
parseString s = case parseStringS s of
  Left e -> error $ show e
  Right r -> r
