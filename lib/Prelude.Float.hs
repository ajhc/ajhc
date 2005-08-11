module Prelude.Float() where

import Ratio
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Jhc.IO
import Foreign.Storable


{-  template


instance Fractional @type@ where
    (/) = divide@type@
    fromRational x = numerator x / denominator x

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

foreign import primitive "divide" divide@type@ ::  @type@ -> @type@ -> @type@

-}




instance Fractional Float where
    a / b = divideFloat a b
    fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

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

foreign import primitive "divide" divideFloat ::  Float -> Float -> Float





instance Fractional Double where
    (/) = divideDouble
    fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

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

foreign import primitive "divide" divideDouble ::  Double -> Double -> Double


instance Real Float where
    toRational x	=  (m%1)*(b%1)^^n
			   where (m,n) = decodeFloat x
				 b     = floatRadix  x
instance Real Double where
    toRational x	=  (m%1)*(b%1)^^n
			   where (m,n) = decodeFloat x
				 b     = floatRadix  x


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

instance RealFloat Float where
    floatRadix _ = c_flt_radix
    floatDigits _ = c_flt_mant_dig
    floatRange _ = (c_flt_min_exp,c_flt_max_exp)

    exponent x		= case decodeFloat x of
			    (m,n) -> if m == 0 then 0 else n + floatDigits x

    significand x	= case decodeFloat x of
			    (m,_) -> encodeFloat m (negate (floatDigits x))

    scaleFloat k x	= case decodeFloat x of
			    (m,n) -> encodeFloat m (n+k)
    isNaN x = c_isnanf x /= 0
    isInfinite x = c_isinfinitef x /= 0
    isDenormalized _ = False
    isNegativeZero x = x == 0 && c_signbitf x /= 0
    isIEEE _ = True
    encodeFloat i e = double2float $ c_ldexp (integer2double i) (fromInt e)
    decodeFloat x = unsafePerformIO $ alloca $ \ptr -> do
        x' <- c_frexp (float2double x) ptr
        exp <- peek ptr
        let x'' =  c_ldexp x' (fromInt $ floatDigits x)
        return (double2integer x'', fromIntegral exp  - floatDigits x)




foreign import ccall "math.h isnan" c_isnanf :: Float -> CInt
foreign import ccall "math.h isinf" c_isinfinitef :: Float -> CInt
foreign import ccall "math.h ldexp" c_ldexp :: Double -> CInt -> Double
foreign import ccall "math.h signbit" c_signbit :: Double -> CInt
foreign import ccall "math.h signbit" c_signbitf :: Float -> CInt

foreign import ccall "math.h frexp" c_frexp :: Double -> Ptr CInt -> IO Double

foreign import primitive "const.FLT_RADIX" c_flt_radix :: Integer
foreign import primitive "const.FLT_MANT_DIG" c_flt_mant_dig :: Int
foreign import primitive "const.FLT_MIN_EXP" c_flt_min_exp :: Int
foreign import primitive "const.FLT_MAX_EXP" c_flt_max_exp :: Int
foreign import primitive "const.M_PI" c_pif :: Float
foreign import primitive "const.M_PI" c_pi :: Double

instance RealFloat Double where
    floatRadix _ = c_flt_radix
    floatDigits _ = c_dbl_mant_dig
    floatRange _ = (c_dbl_min_exp,c_dbl_max_exp)

    exponent x		= case decodeFloat x of
			    (m,n) -> if m == 0 then 0 else n + floatDigits x

    significand x	= case decodeFloat x of
			    (m,_) -> encodeFloat m (negate (floatDigits x))

    --scaleFloat k x	= case decodeFloat x of
    --    		    (m,n) -> encodeFloat m (n+k)
    isNaN x = c_isnan x /= 0
    isInfinite x = c_isinfinite x /= 0
    isDenormalized _ = False
    isNegativeZero x = x == 0 && c_signbit x /= 0
    isIEEE _ = True
    encodeFloat i e =  c_ldexp (integer2double i) (fromInt e)
    scaleFloat k x = c_ldexp x (fromInt k)
    decodeFloat x = unsafePerformIO $ alloca $ \ptr -> do
        x' <- c_frexp x ptr
        exp <- peek ptr
        let x'' = c_ldexp x' (fromInt $ floatDigits x)
        return (double2integer x'', fromIntegral exp  - floatDigits x)




foreign import ccall "math.h isnan" c_isnan :: Double -> CInt
foreign import ccall "math.h isinf" c_isinfinite :: Double -> CInt

foreign import primitive "const.DBL_MANT_DIG" c_dbl_mant_dig :: Int
foreign import primitive "const.DBL_MIN_EXP" c_dbl_min_exp :: Int
foreign import primitive "const.DBL_MAX_EXP" c_dbl_max_exp :: Int

foreign import primitive "integralCast" integer2float :: Integer -> Float
foreign import primitive "integralCast" integer2double :: Integer -> Double
foreign import primitive "integralCast" double2float :: Double -> Float
foreign import primitive "integralCast" double2integer :: Double -> Integer
foreign import primitive "integralCast" float2double :: Float -> Double

