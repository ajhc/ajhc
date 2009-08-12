{-# OPTIONS_JHC -N -fffi #-}

module System.Mem where

import Jhc.Basics

foreign import ccall "hs_perform_gc" performGC :: IO ()
