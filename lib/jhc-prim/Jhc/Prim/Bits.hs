module Jhc.Prim.Bits where

-- this declares the built in unboxed types.
-- no code is brought in by this module, it just
-- brings the names into scope, so it is okay to
-- have platform specific definitions here.

data Bits1_ :: #

data Bits8_   :: #
data Bits16_  :: #
data Bits32_  :: #
data Bits64_  :: #
data Bits128_ :: #
data BitsPtr_ :: #
data BitsMax_ :: #

data Float16_  :: #
data Float32_  :: #
data Float64_  :: #
data Float80_  :: #
data Float128_ :: #
data FloatMax_ :: #

-- data Complex_ :: # -> #

-- these newtypes exist to modify the
-- calling convention and provide hints as
-- to the use of the types.
newtype Addr_    = Addr_ BitsPtr_
newtype FunAddr_ = FunAddr_ BitsPtr_
newtype Bool_    = Bool_ Bits16_
newtype Char_    = Char_ Bits32_

-- type aliases to help document whether signed or unsigned
-- uses are intended, they have no effect other than helping
-- convey intent to someone reading the code.
type Word_ = Bits32_
type Int_  = Bits32_
