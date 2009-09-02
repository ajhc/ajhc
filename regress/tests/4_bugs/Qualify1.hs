module Main where

import qualified Prelude as P

data T = T

-- GHC doesn't allow: T.show T = "T". What does the haskell98 doc say?
{-
  idecls  ->  { idecl_1 ; ... ; idecl_n }             (n>=0)
  idecl   ->  (funlhs | var) rhs
          |                                           (empty) 
-}
-- var, of course, does not permit qnames.

instance P.Show T where
  show T = "T"

main :: P.IO ()
main = P.return ()

