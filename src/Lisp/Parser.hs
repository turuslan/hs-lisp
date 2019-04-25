module Lisp.Parser where

import Lisp.Ast
import Lisp.Parser.Number

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
  try ( do 
    pos1 <- getPosition
    (parens $ return ())
    pos2 <- getPosition
    return $ EmptyList (Pos pos1 pos2)
  )
  <|> parens list
  <|> ( do
    pos1 <- getPosition
    s <- Token.stringLiteral lexer
    pos2 <- getPosition
    return $ StringLiteral s (Pos pos1 pos2)
  )
  <|> try number
  <|> ( do
    pos1 <- getPosition
    i <- Token.identifier lexer
    pos2 <- getPosition
    return $ Atom i (Pos pos1 pos2)
  )
  where
    list = do
      pos1 <- getPosition
      car <- sexpr
      whiteSpace
      cdr <- try (char '.' 
          >> lookAhead (oneOf " \r\n\t(\"") 
          >> whiteSpace 
          >> sexpr
        )
        <|> (lookAhead sexpr >> list) 
        <|> return (EmptyList (Pos pos1 pos1))
      pos2 <- getPosition
      return $ DottedPair car cdr (Pos pos1 pos2)

-- | Boilerplate
whileParser :: Parser [SExpr]
whileParser = do 
  whiteSpace
  es <- sexprs
  eof
  return es

-- | Safe parse string.
parseStringS :: String -> Either ParseError [SExpr]
parseStringS = parse whileParser ""
