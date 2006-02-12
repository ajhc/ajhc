-- | All hardcoded names in the compiler should go in here
-- the convention is
-- v_foo for values
-- tc_foo for type constructors
-- dc_foo for data constructors
-- s_foo for sort names
-- rt_foo for raw names
-- class_foo for classes

module Name.Names where

import Char(isDigit)

import Name.VConsts
import Name.Name

instance TypeNames Name where
    tInt = tc_Int
    tBool = tc_Bool
    tInteger = tc_Integer
    tChar = tc_Char
    tStar = s_Star
    tHash = s_Hash
    tUnit = tc_Unit
    tIntzh = rt_int
    tCharzh = rt_hschar
    tIntegerzh = rt_intmax_t
    tWorld__ = tc_World__

instance ConNames Name where
    vTrue = dc_True
    vFalse = dc_False
    vEmptyList = dc_EmptyList
    vUnit = dc_Unit
    vCons = dc_Cons


-- Tuple handling

--No tuple instance because it is easy to get the namespace wrong. use 'nameTuple'
--instance ToTuple Name where
--    toTuple n = toName DataConstructor (toTuple n :: (String,String))

nameTuple _ n | n < 2 = error "attempt to create tuple of length < 2"
nameTuple t n = toName t  $ (toTuple n:: (String,String)) -- Qual (HsIdent ("(" ++ replicate (n - 1) ',' ++ ")"))

unboxedNameTuple t n = toName t $ "(#" ++ show n ++ "#)"
fromUnboxedNameTuple n = case show n of
    '(':'#':xs | (ns@(_:_),"#)") <- span isDigit xs -> return (read ns::Int)
    _ -> fail $ "Not unboxed tuple: " ++ show n

instance FromTupname Name where
    fromTupname name | m == "Prelude" = fromTupname (nn::String) where
        (_,(m,nn)) = fromName name
    fromTupname _ = fail "not a tuple"



-- The constructors

dc_Addr = toName DataConstructor ("Jhc.Addr","Addr")
dc_Char = toName DataConstructor ("Prelude","Char")
dc_Cons = toName DataConstructor ("Prelude",":")
dc_EmptyList = toName DataConstructor ("Prelude","[]")
dc_False = toName DataConstructor ("Prelude","False")
dc_Integer = toName DataConstructor ("Prelude","Integer")
dc_Int = toName DataConstructor ("Prelude","Int")
dc_JustIO = toName DataConstructor ("Jhc.IO", "JustIO")
dc_Rational = toName DataConstructor ("Ratio",":%")
dc_True = toName DataConstructor ("Prelude","True")
dc_Unit = toName DataConstructor ("Prelude","()")
dc_World__ = toName DataConstructor ("Jhc.IO","World__")

tc_Absurd = toName TypeConstructor ("Jhc@","Absurd#")
tc_Arrow = toName TypeConstructor ("Jhc@","->")

tc_IOResult = toName TypeConstructor ("Jhc.IO","IOResult")
tc_IO = toName TypeConstructor ("Jhc.IO", "IO")
tc_World__ = toName TypeConstructor ("Jhc.IO","World__")

tc_Bool = toName TypeConstructor ("Prelude","Bool")
tc_Char = toName TypeConstructor ("Prelude","Char")
tc_Double = toName TypeConstructor ("Prelude","Double")
tc_Integer = toName TypeConstructor ("Prelude","Integer")
tc_Int = toName TypeConstructor ("Prelude","Int")
tc_List = toName TypeConstructor ("Prelude","[]")
tc_Ptr = toName TypeConstructor ("Foreign.Ptr","Ptr")
tc_Ratio = toName TypeConstructor ("Ratio","Ratio")
tc_Unit = toName TypeConstructor  ("Prelude","()")

rt_int = toName RawType "int"
rt_uint32_t = toName RawType "uint32_t"
rt_intmax_t = toName RawType "intmax_t"
rt_hschar   = toName RawType "HsChar"

s_Star = toName SortName ("Jhc@","*")
s_Hash = toName SortName ("Jhc@","#")

v_error = toName Val ("Prelude","error")
v_toEnum = toName Val ("Prelude","toEnum")
v_fromEnum = toName Val ("Prelude","fromEnum")
v_minBound = toName Val ("Prelude","minBound")
v_maxBound = toName Val ("Prelude","maxBound")
v_fail = toName Val ("Prelude","fail")
v_concatMap = toName Val ("Prelude","concatMap")
v_map = toName Val ("Prelude","map")
v_and = toName Val ("Prelude","&&")
v_filter = toName Val ("Prelude","filter")
v_foldr = toName Val ("Prelude","foldr")

sFuncNames = FuncNames {
    func_bind = toName Val ("Prelude",">>="),
    func_bind_ = toName Val ("Prelude",">>"),
    func_negate = toName Val ("Prelude","negate"),
    func_runMain = toName Val ("Prelude.IO","runMain"),
    func_fromInt = toName Val ("Prelude","fromInt"),
    func_fromInteger = toName Val ("Prelude","fromInteger"),
    func_fromRational = toName Val ("Prelude","fromRational"),
    func_runExpr = toName Val ("Prelude.IO","runExpr"),
    func_equals = toName Val ("Prelude","=="),
    func_concatMap = toName Val ("Prelude","concatMap")
    }



class_Eq = toName ClassName ("Prelude","Eq")
class_Ord = toName ClassName ("Prelude","Ord")
class_Enum = toName ClassName ("Prelude","Enum")
class_Bounded = toName ClassName ("Prelude","Bounded")
class_Show = toName ClassName ("Prelude.Text","Show")
class_Read = toName ClassName ("Prelude.Text","Read")
class_Ix = toName ClassName ("Ix","Ix")
class_Functor = toName ClassName ("Prelude","Functor")
class_Monad = toName ClassName ("Prelude","Monad")
class_Num = toName ClassName ("Prelude","Num")
class_Real = toName ClassName ("Prelude","Real")
class_Integral = toName ClassName ("Prelude","Integral")
class_Fractional = toName ClassName ("Prelude","Fractional")
class_Floating = toName ClassName ("Prelude","Floating")
class_RealFrac = toName ClassName ("Prelude","RealFrac")
class_RealFloat = toName ClassName ("Prelude","RealFloat")

