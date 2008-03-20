{-# OPTIONS_JHC -N -fffi #-}
module Unsafe.Coerce(unsafeCoerce) where


foreign import primitive unsafeCoerce :: a -> b
