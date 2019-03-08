module Lisp where

import Lisp.Ast
import Lisp.Parser
import Lisp.Eval



run :: IO ()
run = do
  putStrLn $ show sampleAst
  putStrLn $ show parsedAst
  -- putStrLn $ show $ eval undefined parsedAst

parsedAst :: SExpr
parsedAst = head $ parseString $ show sampleAst

sampleAst :: SExpr
sampleAst =
  (lisp_cons (IntegerLiteral 1)
    (lisp_cons (FloatLiteral 2.3)
      (lisp_cons (StringLiteral "456")
        (lisp_cons _lisp_false
          (lisp_cons
            (lisp_cons (IntegerLiteral 7)
              (lisp_cons (FloatLiteral 8.9)
                EmptyList))
            _lisp_true)))))

test :: IO ()
test = do
  match (parse1 "(+ 1. .2)") (lisp_cons plus (lisp_cons (float 1) (lisp_cons (float 0.2) EmptyList)))
  match (parse1 "(+ 1 2)") (lisp_cons plus (lisp_cons (int 1) (lisp_cons (int 2) EmptyList)))
  match (parse1 "(+ 1 . 2)") (lisp_cons plus (lisp_cons (int 1) (int 2)))
  
  -- match (eval undefined $ parse1 "(+)") (int 0)
  -- match (eval undefined $ parse1 "(+ 1 2)") (int 3)
  -- match (eval undefined $ parse1 "(+ 1 . .2)") (int 1)
  -- match (eval undefined $ parse1 "(+ 1 .2)") (float 1.2)
  -- match (eval undefined $ parse1 "(+ 1. .2)") (float 1.2)

  where
    plus = Atom "+"
    int = IntegerLiteral
    float = FloatLiteral

    parse1 = head . parseString
    match a e = putStrLn (if a /= e then "expected " ++ show e ++ ", got " ++ show a else "pass")
