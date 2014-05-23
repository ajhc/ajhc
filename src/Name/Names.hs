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

import Ty.Level
import Name.Name
import Name.Prim
import Name.VConsts

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

nameTuple _ n | n < 2 = error "attempt to create tuple of length < 2"
nameTuple t n = toName t  $ (toTuple n:: (String,String)) -- Qual (HsIdent ("(" ++ replicate (n - 1) ',' ++ ")"))

unboxedNameTuple t n = toName t $ "(#" ++ show n ++ "#)"
fromUnboxedNameTuple n = case show n of
    '(':'#':xs | (ns@(_:_),"#)") <- span isDigit xs -> return (read ns::Int)
    _ -> fail $ "Not unboxed tuple: " ++ show n

instance FromTupname Name where
    fromTupname name | m == Module "Jhc.Prim.Prim" = fromTupname (nn::String) where
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

----------------
-- Parameterized
----------------

-- Goal, get rid of hardcoded NameType, move pertinent info into cache byte

instance HasTyLevel Name where
    getTyLevel n = f (nameType n) where
        f DataConstructor = Just termLevel
        f Val             = Just termLevel
        f RawType         = Just typeLevel
        f TypeConstructor = Just typeLevel
        f TypeVal         = Just typeLevel
        f SortName
            | n == s_HashHash = Just $ succ kindLevel
            | n == s_StarStar = Just $ succ kindLevel
            | otherwise = Just kindLevel
        f _ = Nothing

isConstructor :: Name -> Bool
isConstructor n = f (nameType n) where
    f TypeConstructor = True
    f DataConstructor = True
    f SortName = True
    f _ = False

nameTyLevel_u f n = case getTyLevel n of
    Nothing -> n
    Just cl | cl == cl' -> n
            | otherwise -> toName (mkNameType cl' (isConstructor n)) n
        where cl' = f cl
