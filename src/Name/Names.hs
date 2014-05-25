{-# LANGUAGE OverloadedStrings #-}
-- | All hardcoded names in the compiler should go in here
-- the convention is
-- v_foo for values
-- tc_foo for type constructors
-- dc_foo for data constructors
-- s_foo for sort names
-- rt_foo for raw names
-- class_foo for classes

module Name.Names(module Name.Name,module Name.Names,module Name.Prim) where

import Char(isDigit)

import Name.Name
import Name.Prim
import Name.VConsts
import Ty.Level

instance TypeNames Name where
    tInt = tc_Int
    tBool = tc_Bool
    tInteger = tc_Integer
    tChar = tc_Char
    tUnit = tc_Unit

    tIntzh = rt_bits32
    tEnumzh = rt_bits16
    tCharzh = tc_Char_
--    tWorld__ = tc_World__

--No tuple instance because it is easy to get the namespace wrong. use 'nameTuple'
--instance ToTuple Name where
--    toTuple n = toName DataConstructor (toTuple n :: (String,String))

nameTuple t 0 = toName t dc_Unit -- $ -- (toTuple n:: (String,String)) -- Qual (HsIdent ("(" ++ replicate (n - 1) ',' ++ ")"))
nameTuple _ n | n < 2 = error "attempt to create tuple of length < 2"
nameTuple t n = toName t  $ (toTuple n:: (String,String)) -- Qual (HsIdent ("(" ++ replicate (n - 1) ',' ++ ")"))

unboxedNameTuple t n = toName t $ "(#" ++ show n ++ "#)"
fromUnboxedNameTuple n = case show n of
    '(':'#':xs | (ns@(_:_),"#)") <- span isDigit xs -> return (read ns::Int)
    _ -> fail $ "Not unboxed tuple: " ++ show n

instance FromTupname Name where
    fromTupname name | m == mod_JhcPrimPrim = fromTupname (nn::String) where
        (_,(m,nn)) = fromName name
    fromTupname _ = fail "not a tuple"

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

--------------
-- tuple names
--------------

name_TupleConstructor :: TyLevel -> Int -> Name
name_TupleConstructor l 1 = error $ "name_TupleConstructor called for unary tuple at " ++ show l
name_TupleConstructor l 0 = nameTyLevel_u (const l) dc_Unit
name_TupleConstructor l n = mkName l True (Just mod_JhcPrimPrim) ("(" ++ replicate (n - 1) ',' ++ ")")

name_UnboxedTupleConstructor :: TyLevel -> Int -> Name
name_UnboxedTupleConstructor l n = mkName l True Nothing ("(#" ++ show n ++ "#)")

-- checks whether it is either a normal or unboxed tuple. Bool return is whether
-- it is boxed.
fromName_Tuple :: Name -> Maybe (Bool,TyLevel,Int)
fromName_Tuple n | n == dc_Unit = Just (True,termLevel,0)
fromName_Tuple n | n == tc_Unit = Just (True,typeLevel,0)
fromName_Tuple n = f (getTyLevel n, deconstructName n) where
    f (Just tl,(False,Just m,Nothing,uq,Nothing)) |
        m == mod_JhcPrimPrim,
        Just n <- fromTupname (show uq)= Just (True,tl,n)
    f (Just tl,(False,Nothing,Nothing,uq,Nothing)) |
        Just n <- fromUnboxedNameTuple uq = Just (False,tl,n)
    f _ = Nothing

fromName_UnboxedTupleConstructor :: Name -> Maybe (TyLevel,Int)
fromName_UnboxedTupleConstructor (fromName_Tuple -> Just (False,tl,n)) = Just (tl,n)
fromName_UnboxedTupleConstructor _ = Nothing

fromName_TupleConstructor :: Name -> Maybe (TyLevel,Int)
fromName_TupleConstructor (fromName_Tuple -> Just (True,tl,n)) = Just (tl,n)
fromName_TupleConstructor _ = Nothing
