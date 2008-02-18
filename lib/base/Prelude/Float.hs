{-# OPTIONS_JHC -N -fffi  #-}

module Prelude.Float(readDouble,doubleToDigits,doubleToRational) where

import Jhc.Order
import Jhc.Basics
import Jhc.Monad
import Jhc.IO
import Jhc.Float
import Jhc.Num
import Data.Word
import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Numeric
import Prelude.Text
import Jhc.List
import Prelude((^),(^^),elem,take)


{-  template


--instance Fractional @type@ where
--    (/) = divide@type@
--    fromRational x = numerator x / denominator x

instance Floating @type@ where
    pi = atan 1 * 4
    sqrt = c_sqrt@x@
    exp = c_exp@x@
    log = c_log@x@
    sin = c_sin@x@
    cos = c_cos@x@
    tan = c_tan@x@
    asin = c_asin@x@
    acos = c_acos@x@
    atan = c_atan@x@
    (**) = exponent@type@
    asinh = c_asinh@x@
    acosh = c_acosh@x@
    atanh = c_atanh@x@
    sinh = c_sinh@x@
    cosh = c_cosh@x@
    tanh = c_tanh@x@


instance RealFrac Float where
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

    truncatef x = c_trunc@x@ x
    roundf x = c_nearbyint@x@ x
    ceilingf x = c_ceil@x@ x
    floorf x = c_floor@x@ x


foreign import ccall "-lm math.h sqrt@x@" c_sqrt@x@ :: @type@ -> @type@
foreign import ccall "-lm math.h exp@x@" c_exp@x@ :: @type@ -> @type@
foreign import ccall "-lm math.h log@x@" c_log@x@ :: @type@ -> @type@
foreign import ccall "-lm math.h sin@x@" c_sin@x@ :: @type@ -> @type@
foreign import ccall "-lm math.h cos@x@" c_cos@x@ :: @type@ -> @type@
foreign import ccall "-lm math.h tan@x@" c_tan@x@ :: @type@ -> @type@
foreign import ccall "-lm math.h asin@x@" c_asin@x@ :: @type@ -> @type@
foreign import ccall "-lm math.h acos@x@" c_acos@x@ :: @type@ -> @type@
foreign import ccall "-lm math.h atan@x@" c_atan@x@ :: @type@ -> @type@
foreign import ccall "-lm math.h asinh@x@" c_asinh@x@ :: @type@ -> @type@
foreign import ccall "-lm math.h acosh@x@" c_acosh@x@ :: @type@ -> @type@
foreign import ccall "-lm math.h atanh@x@" c_atanh@x@ :: @type@ -> @type@
foreign import ccall "-lm math.h sinh@x@" c_sinh@x@ :: @type@ -> @type@
foreign import ccall "-lm math.h cosh@x@" c_cosh@x@ :: @type@ -> @type@
foreign import ccall "-lm math.h tanh@x@" c_tanh@x@ :: @type@ -> @type@
foreign import ccall "-lm math.h pow@x@" exponent@type@ ::  @type@ -> @type@ -> @type@
foreign import ccall "-lm math.h trunc@x@" c_trunc@x@ :: @type@ -> @type@
foreign import ccall "-lm math.h ceil@x@" c_ceil@x@ :: @type@ -> @type@
foreign import ccall "-lm math.h floor@x@" c_floor@x@ :: @type@ -> @type@
foreign import ccall "-lm math.h nearbyint@x@" c_nearbyint@x@ :: @type@ -> @type@

foreign import primitive "divide" divide@type@ ::  @type@ -> @type@ -> @type@

-}




instance Fractional Float where
    a / b = divideFloat a b
    fromRational x = fromInteger (numerator x) / fromInteger (denominator x)
    fromDouble x = doubleToFloat x

instance Floating Float where
    pi = c_pif
    sqrt = c_sqrtf
    exp = c_expf
    log = c_logf
    sin = c_sinf
    cos = c_cosf
    tan = c_tanf
    asin = c_asinf
    acos = c_acosf
    atan = c_atanf
    (**) = exponentFloat
    asinh = c_asinhf
    acosh = c_acoshf
    atanh = c_atanhf
    sinh = c_sinhf
    cosh = c_coshf
    tanh = c_tanhf


foreign import ccall "-lm math.h sqrtf" c_sqrtf :: Float -> Float
foreign import ccall "-lm math.h expf" c_expf :: Float -> Float
foreign import ccall "-lm math.h logf" c_logf :: Float -> Float
foreign import ccall "-lm math.h sinf" c_sinf :: Float -> Float
foreign import ccall "-lm math.h cosf" c_cosf :: Float -> Float
foreign import ccall "-lm math.h tanf" c_tanf :: Float -> Float
foreign import ccall "-lm math.h asinf" c_asinf :: Float -> Float
foreign import ccall "-lm math.h acosf" c_acosf :: Float -> Float
foreign import ccall "-lm math.h atanf" c_atanf :: Float -> Float
foreign import ccall "-lm math.h asinhf" c_asinhf :: Float -> Float
foreign import ccall "-lm math.h acoshf" c_acoshf :: Float -> Float
foreign import ccall "-lm math.h atanhf" c_atanhf :: Float -> Float
foreign import ccall "-lm math.h sinhf" c_sinhf :: Float -> Float
foreign import ccall "-lm math.h coshf" c_coshf :: Float -> Float
foreign import ccall "-lm math.h tanhf" c_tanhf :: Float -> Float
foreign import ccall "-lm math.h powf" exponentFloat ::  Float -> Float -> Float
foreign import ccall "-lm math.h truncf" c_truncf :: Float -> Float
foreign import ccall "-lm math.h ceilf" c_ceilf :: Float -> Float
foreign import ccall "-lm math.h floorf" c_floorf :: Float -> Float
foreign import ccall "-lm math.h nearbyintf" c_nearbyintf :: Float -> Float



foreign import primitive "FDiv" divideFloat ::  Float -> Float -> Float





instance Fractional Double where
    (/) = divideDouble
    fromRational x = fromInteger (numerator x) / fromInteger (denominator x)
    fromDouble x = x

instance Floating Double where
    pi = c_pi
    sqrt = c_sqrt
    exp = c_exp
    log = c_log
    sin = c_sin
    cos = c_cos
    tan = c_tan
    asin = c_asin
    acos = c_acos
    atan = c_atan
    (**) = exponentDouble
    asinh = c_asinh
    acosh = c_acosh
    atanh = c_atanh
    sinh = c_sinh
    cosh = c_cosh
    tanh = c_tanh


foreign import ccall "-lm math.h sqrt" c_sqrt :: Double -> Double
foreign import ccall "-lm math.h exp" c_exp :: Double -> Double
foreign import ccall "-lm math.h log" c_log :: Double -> Double
foreign import ccall "-lm math.h sin" c_sin :: Double -> Double
foreign import ccall "-lm math.h cos" c_cos :: Double -> Double
foreign import ccall "-lm math.h tan" c_tan :: Double -> Double
foreign import ccall "-lm math.h asin" c_asin :: Double -> Double
foreign import ccall "-lm math.h acos" c_acos :: Double -> Double
foreign import ccall "-lm math.h atan" c_atan :: Double -> Double
foreign import ccall "-lm math.h asinh" c_asinh :: Double -> Double
foreign import ccall "-lm math.h acosh" c_acosh :: Double -> Double
foreign import ccall "-lm math.h atanh" c_atanh :: Double -> Double
foreign import ccall "-lm math.h sinh" c_sinh :: Double -> Double
foreign import ccall "-lm math.h cosh" c_cosh :: Double -> Double
foreign import ccall "-lm math.h tanh" c_tanh :: Double -> Double
foreign import ccall "-lm math.h pow" exponentDouble ::  Double -> Double -> Double
foreign import ccall "-lm math.h trunc" c_trunc :: Double -> Double
foreign import ccall "-lm math.h ceil" c_ceil :: Double -> Double
foreign import ccall "-lm math.h floor" c_floor :: Double -> Double
foreign import ccall "-lm math.h nearbyint" c_nearbyint :: Double -> Double



foreign import primitive "FDiv" divideDouble ::  Double -> Double -> Double


instance Real Float where
    toRational x	=  (m:%1)*(b:%1)^^n
			   where (m,n) = decodeFloat x
				 b     = floatRadix  x
    toDouble x = floatToDouble x

instance Real Double where
    toRational x = doubleToRational x
    toDouble x = x


instance RealFrac Float where
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
    truncatef x = c_truncf x
    roundf x = c_nearbyintf x
    ceilingf x = c_ceilf x
    floorf x = c_floorf x


instance RealFrac Double where
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
    truncatef x = c_trunc x
    roundf x = c_nearbyint x
    ceilingf x = c_ceil x
    floorf x = c_floor x


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



foreign import ccall "math.h isnan" c_isnanf :: Float -> CInt
foreign import ccall "math.h isinf" c_isinfinitef :: Float -> CInt
foreign import ccall "math.h signbit" c_signbit :: Double -> CInt
foreign import ccall "math.h signbit" c_signbitf :: Float -> CInt

foreign import ccall "math.h ldexp"  c_ldexp :: Double -> CInt -> Double
foreign import ccall "math.h ldexpf" c_ldexpf :: Float -> CInt -> Float
foreign import ccall "math.h frexp"  c_frexp :: Double -> Ptr CInt -> IO Double
foreign import ccall "math.h frexpf" c_frexpf :: Float -> Ptr CInt -> IO Float

foreign import primitive "const.M_PI" c_pif :: Float
foreign import primitive "const.M_PI" c_pi :: Double

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



foreign import ccall "math.h isnan" c_isnan :: Double -> CInt
foreign import ccall "math.h isinf" c_isinfinite :: Double -> CInt


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


