module Jhc.Type.Basic(module Jhc.Type.Basic, module Jhc.Type.Word)  where

-- CI import Jhc.Prim.Prim
import Jhc.Type.Word

type String = [Char]

data Maybe a  =  Nothing | Just a

data Either a b = Left a | Right b

data Char = Char Char_
data Integer = Integer BitsMax_

type Bool__ = Bool_
type Int__  = Bits32_
type Char__ = Bits32_
type Enum__ = Bits16_
type Addr__ = BitsPtr_
