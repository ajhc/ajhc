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
    tIntzh = rt_bits32
    tCharzh = rt_bits32
    tIntegerzh = rt_bits_max_
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
    fromTupname name | m == "Jhc.Basics" = fromTupname (nn::String) where
        (_,(m,nn)) = fromName name
    fromTupname _ = fail "not a tuple"



-- The constructors


tc_Box = toName TypeConstructor    ("Jhc@","Box")
tc_Arrow = toName TypeConstructor  ("Jhc@","->")

tc_Int__ = toName TypeConstructor  ("Jhc.Prim","Int__")
tc_Addr__ = toName TypeConstructor  ("Jhc.Prim","Addr__")
tc_Array__ = toName TypeConstructor  ("Jhc.Array","Array__")
tc_MutArray__ = toName TypeConstructor  ("Jhc.Array","MutArray__")
tc_Ref__ = toName TypeConstructor ("Data.IORef","Ref__")


tc_Boolzh = toName TypeConstructor ("Jhc.Order","Bool#")
tc_List = toName TypeConstructor  ("Jhc.Prim","[]")


s_Star = toName SortName ("Jhc@","*")
s_Hash = toName SortName ("Jhc@","#")


sFuncNames = FuncNames {
    func_bind = v_bind,
    func_bind_ = v_bind_,
    func_concatMap = v_concatMap,
    func_fromInteger = v_fromInteger,
    func_fromInt = v_fromInt,
    func_fromRational = v_fromRational,
    func_negate = v_negate,
    func_leq = v_leq,
    func_geq = v_geq,
    func_lt = v_lt,
    func_gt = v_gt,
    func_compare = v_compare,
    func_equals = v_equals,
    func_neq = v_neq,
    func_fromEnum = v_fromEnum,
    func_toEnum = v_toEnum,
    func_minBound = v_minBound,
    func_maxBound = v_maxBound,
    func_enumFrom = v_enumFrom,
    func_enumFromThen = v_enumFromThen,
    func_range = v_range,
    func_index = v_index,
    func_inRange = v_inRange,
    func_runExpr = v_runExpr,
    func_runRaw = v_runRaw,
    func_runMain = v_runMain,
    func_runNoWrapper = v_runNoWrapper
    }




