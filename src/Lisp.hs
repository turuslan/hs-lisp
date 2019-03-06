module Lisp where

import Lisp.Ast
import Lisp.Parser
import Lisp.Eval



run :: IO ()
run = do
  putStrLn $ show sampleAst
  putStrLn $ show parsedAst
  putStrLn $ show $ eval undefined parsedAst

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
