{-# LANGUAGE OverloadedStrings #-}
-- translate foreign Prim statements to jhc core
module E.PrimDecode(processPrim) where

import Text.Printf

import C.Prims
import Cmm.Op(readTy,Ty)
import DataConstructors
import E.E
import E.Values
import FrontEnd.SrcLoc
import FrontEnd.Tc.Kind
import FrontEnd.Warning
import StringTable.Atom
import Support.CanType
import qualified Cmm.Op as Op
import qualified Data.Map as Map

{-
-- type to describe type patterns, used for error checking types.
data Var t = Var !Int t
data Quant = Forall | Exists
data Ty t
    = Quant Quant [Var t] t
    | Kind KBase
    | V (Var t)
    | t :-> t
    | Or [t]
    | t :*> t
    | C Name
    | A (Ty t) [Ty t]

match :: Fix Ty ->

--    a :*> b = a :*> (a :-> b) | b

-- describes a pattern the type of a prim must match
data TSpec
    = K KBase
    | TSpec :-> TSpec
    | TSpec :*> TSpec  -- repeated
    | UIO_
    | UIO TSpec
    | TA [TSpec]
-}

data Typ = [KBase] :-> KBase
    deriving (Show)

--data BType
--    = BVar !Int KBase
--    | BCon Name (Maybe [BType])

star = [] :-> Star
hash = [] :-> KHash
starHash = [] :-> KQuestQuest
world = hash

infixr 3 +>
[] :-> k +> ks :-> rt = (k:ks) :-> rt

infix 1 ==>
a ==> b = (a,b)
--plainPrimMap, rawArgPrimMap, prefixPrimMap :: Map.Map Atom Typ
plainPrimMap, fullPlainPrimMap :: Map.Map Atom Typ
plainPrimMap = Map.fromList
    [ "seq" ==> star +> starHash +> starHash
    , "dependingOn" ==> star +> starHash +> star
    , "newWorld__" ==> star +> world
    , "zero" ==> star
    , "one" ==> star
    , "box" ==> hash +> star
    , "unbox" ==> star +> hash
    , "increment" ==> starHash +> starHash
    , "decrement" ==> starHash +> starHash
    , "fincrement" ==> starHash +> starHash
    , "fdecrement" ==> starHash +> starHash
    , "constPeekByte" ==> hash +> hash
    , "exitFailure__" ==> hash +> hash
    ]
ashow op = toAtom (show op)
fullPlainPrimMap = Map.union plainPrimMap . Map.fromList
    $ [ (ashow op ==> starHash +> starHash +> starHash)
        | op :: Op.BinOp <- [minBound .. maxBound] ]
    ++ [ (ashow op ==> starHash +> starHash)
        | op :: Op.UnOp <- [minBound .. maxBound] ]
    ++ [ (ashow op ==> starHash +> starHash)
        | op :: Op.ConvOp <- [minBound .. maxBound] ]
--rawArgPrimMap =
--    [ "peek." ==> starHash :-> UIO starHash
--    , "poke." ==> starHash :-> UIO starHash
--    ]
--prefixPrimMap = rawArgPrimMap ++
--    [ "const." ==> TA [star,hash :-> hash]
--    , "error." ==> star
--    ]

-- | A safe version of 'zipWith'.
pairWith  :: (a -> b -> c)
          -> [a] -> [b] -> Maybe [c]
pairWith f xs ys = g xs ys [] where
    g [] [] rs = Just $ reverse rs
    g (x:xs) (y:ys) rs = g xs ys (f x y:rs)
    g _ _ _ = Nothing

processPrim :: MonadWarn m
    => DataTable
    -> SrcLoc     -- ^ location of declaration
    -> Atom       -- ^ primitive name
    -> [E]        -- ^ arguments
    -> E          -- ^ return type
    -> Requires   -- ^ c requires
    -> m E        -- ^ result
processPrim dT srcLoc pName args rType req = ans where
    ans = case Map.lookup pName fullPlainPrimMap of
        Just typ -> checkType typ (return passThrough)
        Nothing -> prefixOp
    checkType (tas :-> trt) onFail =
        case pairWith match tas (map (getType . getType) args) of
            Just cs | and cs, match trt (getType rType) -> looksGood
            _ -> do
                warn srcLoc "primitive-badtype" $
                    printf "Primitive type mismatch. expected '%s' but found '%s -> %s'"
                        (show (tas :-> trt)) (show $ map (getType . getType) args) (show $ getType rType)
                onFail
    prefixOp = return passThrough
    looksGood = return passThrough
    passThrough = EPrim (APrim (PrimPrim pName) req) args rType

    match k e = f k where
        f Star = e == eStar
        f KHash = e == eHash
        f KQuestQuest = e == eStar || e == eHash
        f _  = False

{-
processPrimPrim :: DataTable -> E -> E
processPrimPrim dataTable o@(EPrim (APrim (PrimPrim s) _) es orig_t) = maybe o id (primopt (fromAtom s) es (followAliases dataTable orig_t)) where
    primBoundMap = [("maxBound",PrimMaxBound), ("minBound",PrimMinBound), ("umaxBound",PrimUMaxBound)]
    primopt "seq" [x,y] _  = return $ prim_seq x y
    primopt "exitFailure__" [w] rt  = return $ EError "" rt
    primopt op [a,b] t | Just cop <- readM op = mdo
        (pa,(ta,_sta)) <- extractPrimitive dataTable a
        (pb,(tb,_stb)) <- extractPrimitive dataTable b
        (bp,(tr,str))  <- boxPrimitive dataTable
                (EPrim (APrim Op { primCOp = Op.BinOp cop (stot cop 1 ta) (stot cop 2 tb), primRetTy = (stot cop 0 tr) } mempty) [pa, pb] str) t
        return bp
    primopt op [a] t | Just cop <- readM op = mdo
        (pa,(ta,_sta)) <- extractPrimitive dataTable a
        (bp,(tr,str)) <- boxPrimitive dataTable
                (EPrim (APrim Op { primCOp = Op.UnOp cop (stringToOpTy ta), primRetTy = (stringToOpTy tr) } mempty) [pa] str) t
        return bp
    primopt op [a] t | Just cop <- readM op = mdo
        (pa,(ta,_sta)) <- extractPrimitive dataTable a
        (bp,(tr,str)) <- boxPrimitive dataTable
                (EPrim (APrim Op { primCOp = Op.ConvOp cop (stringToOpTy ta), primRetTy = (stringToOpTy tr) } mempty) [pa] str) t
        return bp
    primopt "constPeekByte" [a] t = return (EPrim (APrim (Peek Op.bits8) mempty) [a] t)
    primopt "box" [a] t = return ans where
        (ExtTypeBoxed cna _ _) = fromMaybe (error $ "lookupExtTypeInfo(box): " ++ show t) $ lookupExtTypeInfo dataTable t
        ans = ELit litCons { litName = cna, litArgs = [a], litType = orig_t }
    primopt "unbox" [a] t = return ans where
        (vara:_) = newIds (freeVars (a,t,orig_t))
        ans = unbox dataTable a vara $ \tvra -> EVar tvra
    primopt op [a] t | Just o <- lookup op unop = do
        (pa,(ta,sta)) <- extractPrimitive dataTable a
        let tvra = tVr vn sta; (vn:_) = newIds (freeVars (a,t))
        (bp,(tr,str)) <- boxPrimitive dataTable (EVar tvra) t
        let res = EPrim (APrim (Op (Op.BinOp o (stringToOpTy ta) (stringToOpTy ta)) (stringToOpTy tr)) mempty) [pa, ELit (LitInt 1 sta)] str
        return $ eStrictLet tvra res bp
        where unop = [("increment",Op.Add),("decrement",Op.Sub),("fincrement",Op.FAdd),("fdecrement",Op.FSub)]
    primopt n [] t | Just num <- lookup n vs = mdo
        (res,(_,sta)) <- boxPrimitive dataTable (ELit (LitInt num sta)) t; return res
        where vs = [("zero",0),("one",1)]
    primopt "options_target" [] t     = return (ELit (LitInt 0 t))
    primopt pn@(flip lookup primBoundMap -> Just c) [] t  = return $ ans where
        Just tt = Op.readTy $ show rtn
        (ExtTypeBoxed cna rt _) = fromMaybe (error $ "lookupExtTypeInfo(box): " ++ show t) $ lookupExtTypeInfo dataTable t
        ELit LitCons { litName = rtn } = rt
        ee = (EPrim (APrim (PrimTypeInfo tt tt c) mempty) [] rt)
        ans = ELit litCons { litName = cna, litArgs = [ee], litType = orig_t }
    primopt pn [] t | Just c <- getPrefix "options_" pn      = return (EPrim (APrim (CConst ("JHC_" ++ c) "int") mempty) [] t)
    primopt pn [a,w] t | Just c <- getPrefix "peek." pn      >>= Op.readTy = return (EPrim (APrim (Peek c) mempty) [w,a] t)
    --primopt pn [a,w] t | Just c <- getPrefix "peek." pn >>= Op.readTy =
    --    boxResult dataTable t $ \_ pt -> (EPrim (APrim (Peek c) mempty) [w,a] pt)
    primopt pn [a,v,w] t | Just c <- getPrefix "poke." pn    >>= Op.readTy = return (EPrim (APrim (Poke c) mempty) [w,a,v] t)
    primopt pn [v] t | Just c <- getPrefix "sizeOf." pn      >>= Op.readTy = return (EPrim (APrim (PrimTypeInfo c Op.bits32 PrimSizeOf) mempty) [] t)
    primopt pn [v] t | Just c <- getPrefix "alignmentOf." pn >>= Op.readTy = return (EPrim (APrim (PrimTypeInfo c Op.bits32 PrimAlignmentOf) mempty) [] t)
    primopt pn [v] t | Just c <- getPrefix "maxBound." pn    >>= Op.readTy = return (EPrim (APrim (PrimTypeInfo c c PrimMaxBound) mempty) [] t)
    primopt pn [v] t | Just c <- getPrefix "minBound." pn    >>= Op.readTy = return (EPrim (APrim (PrimTypeInfo c c PrimMinBound) mempty) [] t)
    primopt pn [v] t | Just c <- getPrefix "umaxBound." pn   >>= Op.readTy = return (EPrim (APrim (PrimTypeInfo c c PrimUMaxBound) mempty) [] t)
    primopt pn [] t | Just c <-  getPrefix "const.M_PI" pn = mdo
        (res,(_ta,sta)) <- boxPrimitive dataTable (ELit (LitInt (realToFrac (pi :: Double)) sta)) t; return res
    primopt pn [] t | Just c <-  getPrefix "const." pn = mdo
        (res,(ta,sta)) <- boxPrimitive dataTable (EPrim (APrim (CConst c ta) mempty) [] sta) t; return res
    primopt pn [] _ | Just c <-  getPrefix "error." pn = return (EError c orig_t)
    primopt _ _ _ = fail "not a primopt we care about"
processPrimPrim _ e = e

type T = E
boxResult :: DataTable -> T -> (ExtType -> T -> E) -> Maybe E
boxResult dataTable t fn = mdo
        (res,(ta,sta)) <- boxPrimitive dataTable (fn ta sta) t
	return res
stringToOpTy :: String -> Ty
stringToOpTy s = case readTy s of
    Just t -> t
    _ -> error $ printf "stringToOpTy(%s)" s

stringToOpTy' :: String -> String -> Ty
stringToOpTy' x s = case readTy s of
    Just t -> t
    _ -> error $ printf "stringToOpTy(%s): '%s'" x s

stot :: Show a => a -> Int -> String -> Ty
stot op n s = stringToOpTy' (show op ++ show n) s
-}
