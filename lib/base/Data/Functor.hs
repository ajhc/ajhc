{-# OPTIONS_JHC -N #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functors: uniform action over a parameterized type, generalizing the
-- 'map' function on lists.

module Data.Functor
    (
      Functor(fmap, (<$)),
      (<$>),
    ) where

import Jhc.Monad

infixl 4 <$>

-- | An infix synonym for 'fmap'.
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap
