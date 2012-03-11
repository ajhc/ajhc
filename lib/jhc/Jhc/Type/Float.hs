module Jhc.Type.Float where

import Jhc.Prim.Bits

data {-# CTYPE "float" #-} Float = Float Float32_
data {-# CTYPE "double" #-} Double = Double Float64_

infixl 7 :%
data Ratio a  = !a :% !a
