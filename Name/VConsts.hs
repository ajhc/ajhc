module Name.VConsts where

import Data.Traversable
import Data.Foldable
import Control.Applicative
import Data.Monoid

-- This is much more verbose/complicated than it needs be.

class TypeNames a where
    tInt :: a
    tRational :: a
    tChar :: a
    tIntzh :: a
    tIntegerzh :: a
    tCharzh :: a
    tStar :: a
    tHash :: a
    tBool :: a
    tUnit :: a
    tString :: a
    tInteger :: a
    tWorld__ :: a

    tInt = error "tInt"
    tRational = error "tRational"
    tChar = error "tChar"
    tIntzh = error "tIntzh"
    tIntegerzh = error "tIntegerzh"
    tCharzh = error "tCharzh"
    tStar = error "VConsts: tStar"
    tBool = error "tBool"
    tUnit = error "tUnit"
    tString = error "tString"
    tInteger = error "tInteger"
    tHash = error "tHash"
    tWorld__ = error "tWorld"


class ConNames a where
    vTrue :: a
    vFalse :: a
    vEmptyList :: a
    vCons :: a
    vUnit :: a
    vOrdering :: Ordering -> a

    vTrue = error "vTrue"
    vFalse = error "vFalse"
    vEmptyList = error "vEmptyList"
    vCons = error "vCons"
    vUnit = error "vUnit"
    vOrdering x = error $ "v" ++ show x

class FromTupname a where
    fromTupname :: Monad m => a -> m Int

instance FromTupname String where
    fromTupname ('(':s) | (cs,")") <- span (== ',') s, lc <- length cs, lc > 0 = return $! (lc + 1)
    fromTupname xs = fail $ "fromTupname: not tuple " ++ xs

instance FromTupname (String,String) where
    fromTupname ("Jhc.Basics",n) = fromTupname n
    fromTupname xs =  fail $ "fromTupname: not tuple " ++ show xs


class ToTuple a where
    toTuple :: Int -> a

instance ToTuple String where
    toTuple n = '(': replicate (n - 1) ',' ++ ")"

instance ToTuple (String,String) where
    toTuple n = ("Jhc.Basics",toTuple n)




-- | various functions needed for desugaring.
data FuncNames a = FuncNames {
    func_bind :: a,
    func_bind_ :: a,
    func_return :: a,
    func_concatMap :: a,
    func_equals :: a,
    func_fromInt :: a,
    func_fromInteger :: a,
    func_fromRational :: a,
    func_negate :: a,
    func_runExpr :: a,
    func_runRaw :: a,
    func_runMain :: a,
    func_leq :: a,
    func_geq :: a,
    func_lt :: a,
    func_gt :: a,
    func_compare :: a,
    func_neq :: a,
    func_fromEnum :: a,
    func_toEnum :: a,
    func_minBound :: a,
    func_maxBound :: a,
    func_enumFrom :: a,
    func_enumFromThen :: a,
    func_range :: a,
    func_index :: a,
    func_inRange :: a,
    func_runNoWrapper :: a
    }
    {-! derive: Functor, Traversable, Foldable !-}



