{-# OPTIONS_JHC -N #-}

module Jhc.Options(target,Target(..)) where

data Target = Grin | GhcHs | DotNet | Java


{-# NOINLINE target #-}
target :: Target
target = error_target

foreign import primitive "error.Jhc.Options.Target" error_target :: Target
