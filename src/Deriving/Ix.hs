module Deriving.Ix(deriveIx,deriveEnum) where

import Deriving.Type
import Deriving.Util
import FrontEnd.Warning
import FrontEnd.HsSyn
import FrontEnd.Syn.Q
import Name.Names

deriveIx :: SrcLoc -> Module -> Data -> Q HsDecl
deriveIx sloc mod d@D { .. }
    | null body = err "Cannot derive Ix for nullary data type"
    | length body /= 1 = err "Cannot derive Ix for non enumerations with multiple constructors"
    | otherwise = doix body
    where
    err s = warn sloc InvalidDecl s >> mkInst sloc mod d class_Ix []
    doix [b@Body { types = [_],.. }] = do
        (prl,[prle]) <- mkPat mod b
        (prh,[prhe]) <- mkPat mod b
        (pri,[prie]) <- mkPat mod b
--        (euwn,euw) <- newVar (Just mod)
        let unsafeIndex = funBind sloc v_unsafeIndex [HsPTuple [prl,prh], pri] $ app2 (HsVar v_unsafeIndex) (pair prle prhe) prie
            inRange = funBind sloc v_inRange [HsPTuple [prl,prh], pri] $ app2 (HsVar v_inRange) (pair prle prhe) prie
            range = funBind sloc v_range [HsPTuple [prl,prh]] $ app2 (HsVar v_map) (HsCon constructor) (HsApp (HsVar v_range) (HsTuple [prle,prhe]))
        mkInst sloc mod d class_Ix [range,unsafeIndex,inRange]
    doix [b@Body { .. }] = do
        --(pa,[]) <- mkPat mod b
        err "Data.Ix deriving for complex types not yet implemented."
        mkInst sloc mod d class_Ix []

pair a b = HsTuple [a,b]

deriveEnum :: SrcLoc -> Module -> Data -> Q HsDecl
deriveEnum sloc mod d@D { .. }
    | null body = err "Cannot derive Enum for nullary data type"
    | any (not . null) (map types body) = err "Cannot derive Enum for types with non nullary constructors"
    | otherwise = unit_enum body
    where
    err s = warn sloc InvalidDecl s >> mkInst sloc mod d class_Enum []
    unit_enum  ~[b@Body { .. }] = do
        (pa,[]) <- mkPat mod b
        let toEnum = funBind sloc v_toEnum [HsPLit (HsInt 0)] (HsCon constructor)
            fromEnum = funBind sloc v_fromEnum [pa] (HsLit (HsInt 0))
        mkInst sloc mod d class_Enum [toEnum,fromEnum]

