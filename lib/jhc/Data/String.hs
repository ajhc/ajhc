{-# LANGUAGE NoImplicitPrelude#-}
module Data.String (
   String
   ,IsString(..)
   ) where

import Jhc.Type.Basic

-- | Class for string-like datastructures; used by the overloaded string
--   extension (-XOverloadedStrings in GHC).
class IsString a where
    fromString :: String -> a

--instance IsString [Char] where
--    fromString xs = xs
