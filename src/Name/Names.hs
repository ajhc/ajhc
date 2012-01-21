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

import Name.Name
import Name.Prim
import Name.VConsts

instance TypeNames Name where
    tInt = tc_Int
    tBool = tc_Bool
    tInteger = tc_Integer
    tChar = tc_Char
    tStar = s_Star
    tHash = s_Hash
    tUnit = tc_Unit
    tIntzh = rt_bits32
    tEnumzh = rt_bits16
    tCharzh = rt_bits32
    tIntegerzh = rt_bits_max_
    tWorld__ = tc_World__

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

tc_Arrow = toName TypeConstructor  ("Jhc.Basics","->")

s_Star = toName SortName ("Jhc@","*")
s_Hash = toName SortName ("Jhc@","#")
s_Bang = toName SortName ("Jhc@","!")
s_Quest = toName SortName ("Jhc@","?")
s_QuestQuest = toName SortName ("Jhc@","??")
s_StarBang = toName SortName ("Jhc@","*!")

u_instance = toName UnknownType ("Jhc@","instance")

sFuncNames = FuncNames {
    func_equals = v_equals,
    func_fromInteger = v_fromInteger,
    func_fromInt = v_fromInt,
    func_fromRational = v_fromRational,
    func_negate = v_negate,
    func_runExpr = v_runExpr,
    func_runMain = v_runMain,
    func_runNoWrapper = v_runNoWrapper,
    func_runRaw = v_runRaw
    }
