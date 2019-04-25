module Lisp.Utils where
    
import System.Console.Pretty (Color (..), Style (..), bgColor, color, style)


-- | Stub.
-- Never executes.
-- Used to avoid non-exhaustive pattern matching warnings.
impossible :: a
impossible = error $ 
    bgColor Red . color White . style Bold $ 
    "Something went wrong. Impossible program state."