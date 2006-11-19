{-# OPTIONS_JHC -N -fffi #-}

module Jhc.Options(target,Target(..)) where

data Target = Grin | GhcHs | DotNet | Java


{-# NOINLINE target #-}
target :: Target
target = unknown_target

foreign import primitive unknown_target :: Target

