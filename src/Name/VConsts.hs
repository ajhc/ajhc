module Name.VConsts where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Traversable

-- This is much more verbose/complicated than it needs be.

class TypeNames a where
    tInt :: a
    tRational :: a
    tChar :: a
    tIntzh :: a
    tEnumzh :: a
    tCharzh :: a
    tBool :: a
    tUnit :: a
    tString :: a
    tInteger :: a
    tWorld__ :: a

    tInt = error "tInt"
    tRational = error "tRational"
    tChar = error "tChar"
    tIntzh = error "tIntzh"
    tEnumzh = error "tEnumzh"
    tCharzh = error "tCharzh"
    tBool = error "tBool"
    tUnit = error "tUnit"
    tString = error "tString"
    tInteger = error "tInteger"
    tWorld__ = error "tWorld"

class ConNames a where
    vTrue :: a
    vFalse :: a
    vCons :: a
    vUnit :: a

    vTrue = error "vTrue"
    vFalse = error "vFalse"
    vCons = error "vCons"
    vUnit = error "vUnit"

class FromTupname a where
    fromTupname :: Monad m => a -> m Int

instance FromTupname String where
    fromTupname ('(':s) | (cs,")") <- span (== ',') s, lc <- length cs, lc > 0 = return $! (lc + 1)
    fromTupname xs = fail $ "fromTupname: not tuple " ++ xs

instance FromTupname (String,String) where
    fromTupname ("Jhc.Prim.Prim",n) = fromTupname n
    fromTupname xs =  fail $ "fromTupname: not tuple " ++ show xs

class ToTuple a where
    toTuple :: Int -> a

instance ToTuple String where
    toTuple n = '(': replicate (n - 1) ',' ++ ")"

instance ToTuple (String,String) where
    toTuple n = ("Jhc.Prim.Prim",toTuple n)

-- | various functions needed for desugaring.
data FuncNames a = FuncNames {
    func_equals :: a,
    func_fromInt :: a,
    func_fromInteger :: a,
    func_fromRational :: a,
    func_negate :: a,
    func_runExpr :: a,
    func_runMain :: a,
    func_runNoWrapper :: a,
    func_runRaw :: a
    }
    {-! derive: Functor, Traversable, Foldable !-}
