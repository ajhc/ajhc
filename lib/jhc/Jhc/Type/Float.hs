module Jhc.Type.Float where

import Jhc.Prim.Bits

data Float = Float Float32_
data Double = Double Float64_

infixl 7 :%
data Ratio a  = !a :% !a
