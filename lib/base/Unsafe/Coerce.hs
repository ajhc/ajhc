{-# OPTIONS_JHC -fno-prelude -fffi #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Unsafe.Coerce(unsafeCoerce) where

foreign import primitive unsafeCoerce :: a -> b
