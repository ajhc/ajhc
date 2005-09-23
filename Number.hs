module Number(Number(..),TypeInfo,toIntegral) where

import Ratio
import Binary
import Data.Generics

data NumType = Signed | Unsigned | Floating | Pointer
data Value a = Known a | Unknown | AtLeast a | GreatestOfAll

data TypeInfo = TypeInfo {
    typeType :: NumType,
    typeBytes :: Value Int
    }

{-
signed v n = (toName RawType v,TypeInfo Signed (Known n))
unsigned v n = (toName RawType v,TypeInfo Unsigned (Known n))
typeTable = [
    signed "int" 0 { typeBytes = AtLeast 4 },
    unsigned "unsigned int" 0 { typeBytes = AtLeast 4 },
    signed "int8_t" 1,
    signed "int16_t" 2,
    signed "int32_t" 4,
    signed "int64_t" 8,
    signed "intmax_t" 0 { typeBytes = GreatestOfAll },
    signed "intptr_t" 0 { typeBytes = AtLeast 4 },
    unsigned "uint8_t" 1,
    unsigned "uint16_t" 2,
    unsigned "uint32_t" 4,
    unsigned "uint64_t" 8,
    unsigned "uintmax_t" 0 { typeBytes = GreatestOfAll },
    unsigned "uintptr_t" 0 { typeBytes = AtLeast 4 },
    unsigned "wchar_t" 0   { typeBytes = AtLeast 4 },
    signed "wint_t" 0   { typeBytes = AtLeast 4 },
    unsigned "size_t" 0   { typeBytes = AtLeast 4 },
    signed "ssize_t" 0   { typeBytes = AtLeast 4 },
    (toName RawType "HsPtr", TypeInfo Pointer (AtLeast 4)),
    (toName RawType "HsFunPtr", TypeInfo Pointer (AtLeast 4)),
    unsigned "char" 1,
    unsigned "short" 2 { typeBytes = AtLeast 2 }
    ]

 -}

newtype Number = Number Rational
    deriving(Num,Eq,Ord,Binary,Real,Fractional,RealFrac,Enum,Typeable,Data)

instance Integral Number where
    toInteger (Number x) = case denominator x of
        1 -> numerator x
        _ -> error $ "toInteger: Number not integer " ++ show x
    quotRem x y = case toInteger x `quotRem` toInteger y  of
        (x,y) -> (fromInteger x,fromInteger y)

instance Show Number where
    showsPrec n (Number r) = case denominator r of
        1 -> showsPrec n (numerator r)
        _ -> showsPrec n (realToFrac r :: Double)

toIntegral :: (Integral i,Monad m) => Number -> m i
toIntegral (Number r) = case denominator r of
    1 -> return $ fromInteger (numerator r)
    _ -> fail $ "toInteger: Number not integer " ++ show r

--instance Show Number where

--data Number = Number {
--    numberValue :: Ratio,
--    numberType :: Atom
--    }


