{-# OPTIONS_JHC -fno-prelude -fffi -fm4  #-}

module Prelude.Float(readDouble,doubleToDigits,doubleToRational) where

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Jhc.Basics
import Jhc.Float
import Jhc.IO(error)
import Jhc.List(length,notElem,take,elem)
import Jhc.Monad
import Jhc.Num
import Jhc.Order
-- CI import Jhc.Prim.Bits
import Jhc.Type.C
import Numeric
import Jhc.Numeric((^),(^^))
import Prelude.Text
import System.IO.Unsafe

m4_define(INST,{{

foreign import primitive "FDiv" divide$2 ::  $2 -> $2 -> $2
foreign import primitive "FPwr" exponent$2 ::  $2 -> $2 -> $2
foreign import primitive "FAtan2" atan2$1 ::  $1 -> $1 -> $1
foreign import primitive "F2I"  toInteger$1 :: $1 -> Integer
foreign import primitive "const.M_PI" c_pi$1 :: $1

instance Fractional $1 where
    $1 x / $1 y = $1 (divide$2 x y)
    fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

m4_define(FI,foreign import primitive "{{$}}1"  {{$}}2$2 :: $2 -> $2)

FI(Sqrt,sqrt)
FI(Exp,exp)
FI(Log,log)
FI(Sin,sin)
FI(Cos,cos)
FI(Tan,tan)
FI(Sinh,sinh)
FI(Cosh,cosh)
FI(Tanh,tanh)
FI(Asin,asin)
FI(Acos,acos)
FI(Atan,atan)

m4_undefine({{FI}})

instance Floating $1 where
    pi = c_pi$1
    sqrt ($1 x) = $1 (sqrt$2 x)
    exp ($1 x) = $1 (exp$2 x)
    log ($1 x) = $1 (log$2 x)
    sin ($1 x) = $1 (sin$2 x)
    cos ($1 x) = $1 (cos$2 x)
    tan ($1 x) = $1 (tan$2 x)
    asin ($1 x) = $1 (asin$2 x)
    acos ($1 x) = $1 (acos$2 x)
    atan ($1 x) = $1 (atan$2 x)
    sinh ($1 x) = $1 (sinh$2 x)
    cosh ($1 x) = $1 (cosh$2 x)
    tanh ($1 x) = $1 (tanh$2 x)
    $1 x ** $1 y = $1 (exponent$2 x y)

    asinh = c_asinh$1
    acosh = c_acosh$1
    atanh = c_atanh$1

instance RealFrac $1 where
    properFraction x
      = case (decodeFloat x)      of { (m,n) ->
    	let  b = floatRadix x     in
    	if n >= 0 then
	    (fromInteger m * fromInteger b ^ n, 0.0)
    	else
	    case (quotRem m (b^(negate n))) of { (w,r) ->
	    (fromInteger w, encodeFloat r n)
	    }
        }

    truncate x = fromInteger (toInteger$1 x)
    round x = fromInteger (toInteger$1 (roundf x))
    ceiling x = fromInteger (toInteger$1 (ceilingf x))
    floor x = fromInteger (toInteger$1 (floorf x))

    properFractionf x = (c_trunc$1 x,x - c_trunc$1 x)
    truncatef x = c_trunc$1 x
    roundf x = c_nearbyint$1 x
    ceilingf x = c_ceil$1 x
    floorf x = c_floor$1 x

foreign import ccall "-lm math.h asinh$3" c_asinh$1 :: $1 -> $1
foreign import ccall "-lm math.h acosh$3" c_acosh$1 :: $1 -> $1
foreign import ccall "-lm math.h atanh$3" c_atanh$1 :: $1 -> $1
foreign import ccall "-lm math.h trunc$3" c_trunc$1 :: $1 -> $1
foreign import ccall "-lm math.h ceil$3" c_ceil$1 :: $1 -> $1
foreign import ccall "-lm math.h floor$3" c_floor$1 :: $1 -> $1
foreign import ccall "-lm math.h nearbyint$3" c_nearbyint$1 :: $1 -> $1

foreign import ccall "math.h isnan" c_isnan$3 :: $1 -> CInt
foreign import ccall "math.h isinf" c_isinfinite$3 :: $1 -> CInt
foreign import ccall "math.h signbit" c_signbit$3 :: $1 -> CInt

foreign import ccall "math.h ldexp$3"  c_ldexp$3 :: $1 -> CInt -> $1
foreign import ccall "math.h frexp$3"  c_frexp$3 :: $1 -> Ptr CInt -> IO $1

}})

INST(Float,Float32_,f)
INST(Double,Float64_)

instance Real Float where
    toRational x	=  (m:%1)*(b:%1)^^n
			   where (m,n) = decodeFloat x
				 b     = floatRadix  x
    toDouble x = floatToDouble x

instance Real Double where
    toRational x = doubleToRational x
    toDouble x = x

instance RealFloat Float where
    floatRadix _ = 2
    floatDigits _ = 24
    floatRange _ = (-125,128)

    exponent x		= case decodeFloatf x of (_,n) -> n
    significand x	= case decodeFloatf x of (m,_) -> m

    isNaN x = c_isnanf x /= 0
    isInfinite x = c_isinfinitef x /= 0
    isDenormalized _ = False
    isNegativeZero x = x == 0 && c_signbitf x /= 0
    isIEEE _ = True

    scaleFloat k x = c_ldexpf x (fromInt k)
    decodeFloatf x = unsafePerformIO $ alloca $ \ptr -> do
        x' <- c_frexpf x ptr
        exp <- peek ptr
        return (x', fromIntegral exp)

    encodeFloat i e = c_ldexpf (fromInteger i) (fromInt e)
    decodeFloat x = unsafePerformIO $ alloca $ \ptr -> do
        x' <- c_frexp (floatToDouble x) ptr
        exp <- peek ptr
        let x'' =  c_ldexp x' (fromInt $ floatDigits x)
        return (double2integer x'', fromIntegral exp  - floatDigits x)

    atan2 = atan2Float

instance RealFloat Double where
    floatRadix _ = 2
    floatDigits _ = 53
    floatRange _ = (-1021,1024)

    exponent x		= case decodeFloatf x of (_,n) -> n
    significand x	= case decodeFloatf x of (m,_) -> m

    isNaN x = c_isnan x /= 0
    isInfinite x = c_isinfinite x /= 0
    isDenormalized _ = False
    isNegativeZero x = x == 0 && c_signbit x /= 0
    isIEEE _ = True
    scaleFloat k x = c_ldexp x (fromInt k)
    decodeFloatf x = unsafePerformIO $ alloca $ \ptr -> do
        x' <- c_frexp x ptr
        exp <- peek ptr
        return (x', fromIntegral exp)

    encodeFloat i e =  c_ldexp (integer2double i) (fromInt e)
    decodeFloat x = unsafePerformIO $ alloca $ \ptr -> do
        x' <- c_frexp x ptr
        exp <- peek ptr
        let x'' = c_ldexp x' (fromInt $ floatDigits x)
        return (double2integer x'', fromIntegral exp  - floatDigits x)

    atan2 = atan2Double

foreign import primitive "I2F" integer2float :: Integer -> Float
foreign import primitive "I2F" integer2double :: Integer -> Double
foreign import primitive "F2I" double2integer :: Double -> Integer

readDouble :: ReadS Double
readDouble r    = [((fromInteger n * (10^^(k-d))),t) | (n,d,s) <- readFix r,(k,t)   <- readExp s] ++
                 [ (0/0, t) | ("NaN",t)      <- lex r] ++
                 [ (1/0, t) | ("Infinity",t) <- lex r]
               where
                 readFix r = [(read (take 15 $ ds++ds'), length ds', t)
                             | (ds,d) <- lexDigits r,
                               (ds',t) <- lexFrac d ]

                 lexFrac ('.':ds) = lexDigits ds
                 lexFrac s        = [("",s)]

                 readExp (e:s) | e `elem` "eE" = readExp' s
                 readExp s                     = [(0,s)]

                 readExp' ('-':s) = [(-k,t) | (k,t) <- readDec s]
                 readExp' ('+':s) = readDec s
                 readExp' s       = readDec s

doubleToDigits :: Integer -> Double -> ([Int], Int)
doubleToDigits n d | n `seq` d `seq` d == 0 = ([], 0)
doubleToDigits base' x =
    let (f0', e0) = decodeFloat x
        base, f0 :: WordMax
        base = fromInteger base'
        f0 = fromIntegral f0'
        (minExp0, _) = floatRange x
        p = floatDigits x
        b :: WordMax
        b = fromInteger $ floatRadix x
        minExp = minExp0 - p            -- the real minimum exponent

        -- Haskell requires that f be adjusted so denormalized numbers
        -- will have an impossibly low exponent.  Adjust for this.
        f :: WordMax
        e :: Int
        (f, e) = let n = minExp - e0
                 in  if n > 0 then (f0 `div` (b^n), e0+n) else (f0, e0)

        (r, s, mUp, mDn) =
           if e >= 0 then
               let be = b^e in
               if f == b^(p-1) then
                   (f*be*b*2, 2*b, be*b, b)
               else
                   (f*be*2, 2, be, be)
           else
               if e > minExp && f == b^(p-1) then
                   (f*b*2, b^(-e+1)*2, b, 1)
               else
                   (f*2, b^(-e)*2, 1, 1)
        k =
            let k0 =
                    if b==2 && base==10 then
                        -- logBase 10 2 is slightly bigger than 3/10 so
                        -- the following will err on the low side.  Ignoring
                        -- the fraction will make it err even more.
                        -- Haskell promises that p-1 <= logBase b f < p.
                        (p - 1 + e0) * 3 `div` 10
                    else
                        ceiling ((log ((fromIntegral (f+1))::Double) +
                                 fromIntegral e * log (fromIntegral b)) /
                                  log (fromIntegral base))
                fixup n =
                    if n >= 0 then
                        if r + mUp <= expt base n * s then n else fixup (n+1)
                    else
                        if expt base (-n) * (r + mUp) <= s then n
                                                           else fixup (n+1)
            in  fixup (k0::Int)

        gen ds rn sN mUpN mDnN | rn `seq` sN `seq` mUpN `seq` mDnN `seq` True =
            let (dn, rn') = (rn * base) `divMod` sN
                mUpN' = mUpN * base
                mDnN' = mDnN * base
            in  case (rn' < mDnN', rn' + mUpN' > sN) of
                (True,  False) -> toInt dn : ds
                (False, True)  -> toInt (dn+1) : ds
                (True,  True)  -> if rn' * 2 < sN then toInt dn : ds else toInt (dn+1) : ds
                (False, False) -> gen (toInt dn:ds) rn' sN mUpN' mDnN'
        rds,rrds :: [Int]
        rrds = reverse rds
        rds =
            if k >= 0 then
                gen [] r (s * expt base k) mUp mDn
            else
                let bk = expt base (-k)
                in  gen [] (r * bk) s (mUp * bk) (mDn * bk)
        expt :: WordMax -> Int -> WordMax
        expt base n = base^n
    in  k `seq` f `seq` e `seq` b `seq` rrds `seq` (rrds, k)

doubleToRational :: Double -> Rational
doubleToRational x  =  (m:%1)*(b:%1)^^n where
    (m,n) = decodeFloat x
    b     = floatRadix  x
