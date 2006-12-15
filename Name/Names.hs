-- | All hardcoded names in the compiler should go in here
-- the convention is
-- v_foo for values
-- tc_foo for type constructors
-- dc_foo for data constructors
-- s_foo for sort names
-- rt_foo for raw names
-- class_foo for classes

module Name.Names(module Name.Names,module Name.Prim) where

import Char(isDigit)

import Name.VConsts
import Name.Name
import Name.Prim

instance TypeNames Name where
    tInt = tc_Int
    tBool = tc_Bool
    tInteger = tc_Integer
    tChar = tc_Char
    tStar = s_Star
    tHash = s_Hash
    tUnit = tc_Unit
    tIntzh = rt_int
    tCharzh = rt_HsChar
    tIntegerzh = rt_intmax_t
    tWorld__ = tc_World__

instance ConNames Name where
--    vTrue = dc_True
--    vFalse = dc_False
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

dc_Cons = toName DataConstructor ("Jhc.Prim",":")
dc_EmptyList = toName DataConstructor ("Jhc.Prim","[]")
dc_Rational = toName DataConstructor ("Ratio",":%")
dc_Unit = toName DataConstructor ("Jhc.Basics","()")
dc_Boolzh = toName DataConstructor ("Jhc.Order","Bool#")
dc_Target = toName DataConstructor  ("Jhc.Options","Target#")

tc_Absurd = toName TypeConstructor ("Jhc@","Absurd#")
tc_Box = toName TypeConstructor    ("Jhc@","Box")
tc_Arrow = toName TypeConstructor  ("Jhc@","->")
tc_JumpPoint = toName TypeConstructor   ("Jhc.JumpPoint","JumpPoint")

tc_IO = toName TypeConstructor       ("Jhc.Prim", "IO")
tc_World__ = toName TypeConstructor  ("Jhc.Prim","World__")
tc_Int__ = toName TypeConstructor  ("Jhc.Prim","Int__")
tc_Addr__ = toName TypeConstructor  ("Jhc.Prim","Addr__")
tc_Word8__ = toName TypeConstructor  ("Jhc.Prim","Word8__")
tc_Char__ = toName TypeConstructor  ("Jhc.Prim","Char__")
tc_Bool__ = toName TypeConstructor  ("Jhc.Prim","Bool__")
tc_Array__ = toName TypeConstructor  ("Jhc.Array","Array__")
tc_MutArray__ = toName TypeConstructor  ("Jhc.Array","MutArray__")
tc_Ref__ = toName TypeConstructor ("Data.IORef","Ref__")


tc_Bool = toName TypeConstructor   ("Jhc.Order","Bool")
tc_Boolzh = toName TypeConstructor ("Jhc.Order","Bool#")
tc_Target = toName TypeConstructor ("Jhc.Options","Target")
tc_List = toName TypeConstructor  ("Jhc.Prim","[]")
tc_Ptr = toName TypeConstructor   ("Jhc.Addr","Ptr")
tc_Ratio = toName TypeConstructor ("Ratio","Ratio")
tc_Unit = toName TypeConstructor  ("Jhc.Basics","()")


rt_tag = toName RawType "tag#"

s_Star = toName SortName ("Jhc@","*")
s_Hash = toName SortName ("Jhc@","#")

v_eqString = toName Val  ("Jhc.String","eqString")
v_eqUnpackedString = toName Val  ("Jhc.String","eqUnpackedString")
v_unpackString = toName Val  ("Jhc.String","unpackString")
v_target = toName Val  ("Jhc.Options","target")
v_error = toName Val ("Jhc.IO","error")
v_toEnum = toName Val ("Jhc.Enum","toEnum")
v_fromEnum = toName Val ("Jhc.Enum","fromEnum")
v_minBound = toName Val ("Jhc.Enum","minBound")
v_maxBound = toName Val ("Jhc.Enum","maxBound")
v_fail = toName Val ("Prelude","fail")
v_concatMap = toName Val ("Jhc.Basics","concatMap")
v_map = toName Val ("Jhc.Basics","map")
v_and = toName Val ("Jhc.Order","&&")
v_filter = toName Val ("Jhc.List","filter")
v_foldr = toName Val ("Jhc.Basics","foldr")
v_undefined = toName Val ("Jhc.Basics","undefined")
v_silly = toName Val ("Jhc@","silly")

sFuncNames = FuncNames {
    func_bind = toName Val ("Jhc.Monad",">>="),
    func_bind_ = toName Val ("Jhc.Monad",">>"),
    func_concatMap = toName Val ("Jhc.Basics","concatMap"),
    func_fromInteger = toName Val ("Prelude","fromInteger"),
    func_fromInt = toName Val ("Prelude","fromInt"),
    func_fromRational = toName Val ("Prelude","fromRational"),
    func_negate = toName Val ("Prelude","negate"),
    func_leq = toName Val ("Jhc.Order","<="),
    func_geq = toName Val ("Jhc.Order",">="),
    func_lt = toName Val ("Jhc.Order","<"),
    func_gt = toName Val ("Jhc.Order",">"),
    func_compare = toName Val ("Jhc.Order","compare"),
    func_equals = toName Val ("Jhc.Order","=="),
    func_neq = toName Val ("Jhc.Order","/="),
    func_fromEnum = toName Val ("Jhc.Enum","fromEnum"),
    func_toEnum = toName Val ("Jhc.Enum","toEnum"),
    func_minBound = toName Val ("Jhc.Enum","minBound"),
    func_maxBound = toName Val ("Jhc.Enum","maxBound"),
    func_enumFrom = toName Val ("Jhc.Enum","enumFrom"),
    func_enumFromThen = toName Val ("Jhc.Enum","enumFromThen"),
    func_range = toName Val ("Data.Ix","range"),
    func_index = toName Val ("Data.Ix","index"),
    func_inRange = toName Val ("Data.Ix","inRange"),
    func_runExpr = toName Val ("Prelude.IO","runExpr"),
    func_runRaw = toName Val ("Jhc.Prim","runRaw"),
    func_runMain = toName Val ("Jhc.IO","runMain"),
    func_runNoWrapper = toName Val ("Jhc.Prim","runNoWrapper")
    }



class_Eq = toName ClassName ("Jhc.Order","Eq")
class_Ord = toName ClassName ("Jhc.Order","Ord")
class_Enum = toName ClassName ("Jhc.Enum","Enum")
class_Bounded = toName ClassName ("Jhc.Enum","Bounded")
class_Show = toName ClassName ("Prelude.Text","Show")
class_Read = toName ClassName ("Prelude.Text","Read")
class_Ix = toName ClassName ("Ix","Ix")
class_Functor = toName ClassName ("Jhc.Monad","Functor")
class_Monad = toName ClassName ("Jhc.Monad","Monad")
class_Num = toName ClassName ("Prelude","Num")
class_Real = toName ClassName ("Prelude","Real")
class_Integral = toName ClassName ("Prelude","Integral")
class_Fractional = toName ClassName ("Prelude","Fractional")
class_Floating = toName ClassName ("Prelude","Floating")
class_RealFrac = toName ClassName ("Prelude","RealFrac")
class_RealFloat = toName ClassName ("Prelude","RealFloat")

