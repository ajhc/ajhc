-- This module is always included behind the scenes when compiling.
-- It will not bring any code into the system, but brings several
-- names and type definitions into scope that the compiler expects
-- to exist.
module Jhc.Prim.Prim where

import Jhc.Prim.Bits
import Jhc.Prim.IO

data (->) :: ?? -> ? -> *

data () = ()
data (,) a b = (,) a b
data (,,) a b c = (,,) a b c
data (,,,) a b c d = (,,,) a b c d
data (,,,,) a b c d e = (,,,,) a b c d e
data (,,,,,) a b c d e f = (,,,,,) a b c d e f
data (,,,,,,) a b c d e f g = (,,,,,,) a b c d e f g
data (,,,,,,,) a b c d e f g h = (,,,,,,,) a b c d e f g h
data (,,,,,,,,) a b c d e f g h i = (,,,,,,,,) a b c d e f g h i
