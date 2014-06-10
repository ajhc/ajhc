{-# LANGUAGE OverloadedStrings #-}
-- translate foreign Prim statements to jhc core
module E.PrimDecode(processPrim) where

import Text.Printf

import C.Prims
import Cmm.Op(readTy,Ty)
import Data.Maybe
import DataConstructors
import E.E
import E.Values
import FrontEnd.SrcLoc
import FrontEnd.Tc.Kind
import FrontEnd.Warning
import Name.Name
import Name.Names
import PackedString
import StringTable.Atom
import Support.CanType
import Support.FreeVars
import Util.Gen
import qualified Cmm.Op as Op
import qualified Data.Map as Map

data Typ = [BType] :-> BType
    deriving (Show)

data BType
    = BKind KBase
    | BTup [BType]
    | BState

instance Show BType where
    showsPrec n (BKind k) = showsPrec n k
    showsPrec _ BState = showString "State#"
    showsPrec n (BTup ts) = showsPrec n ts

star = [] :-> BKind Star
hash = [] :-> BKind KHash
starHash = [] :-> BKind KQuestQuest
state = [] :-> BState
utup ~([] :-> t1) ~([] :-> t2) = [] :-> BTup [t1,t2]
utup1 ~([] :-> t1) = [] :-> BTup [t1]
array = hash
bang = hash

infixr 3 +>
(+>) :: Typ -> Typ -> Typ
~([] :-> k) +> ks :-> rt = (k:ks) :-> rt

infix 1 ==>
a ==> b = (a,b)
--plainPrimMap, rawArgPrimMap, prefixPrimMap :: Map.Map Atom Typ
plainPrimMap :: Map.Map Atom Typ
plainPrimMap = Map.fromList
    [ "seq"            ==> star +> starHash +> starHash
    , "dependingOn"    ==> star +> starHash +> star
    , "newWorld__"     ==> star +> state
    , "unsafeCoerce"   ==> star +> star
    , "options_target" ==> hash
    , "touch_"         ==> starHash +> state +> state
    , "zero"           ==> starHash
    , "one"            ==> starHash
    , "box"            ==> hash +> star
    , "unbox"          ==> star +> hash
    , "exitFailure__"  ==> hash +> hash
    , "constPeekByte"  ==> hash +> hash
    -- array operations
    , "newArray__"     ==> hash +> star +> state +> utup state array
    , "newBlankArray__"==> hash +> state +> utup state array
    , "copyArray__"    ==> hash +> hash +> hash +> array +> array +> state +> state
    , "readArray__"    ==> array +> hash +> state +> utup state star
    , "writeArray__"   ==> array +> hash +> star +> state +> state
    , "indexArray__"   ==> array +> hash +> utup1 star
    -- accessing the rts directly
    , "toBang_"        ==> star +> bang
    , "fromBang_"      ==> bang +> star
    , "isWHNF"         ==> star +> hash
    , "isInHeap"       ==> bang +> hash
    , "bangPtr"        ==> bang +> hash
    , "bangBits"       ==> bang +> hash
    ] `Map.union` fmap (const (starHash +> starHash)) incDec
      `Map.union` fmap (const star) primBoundMap

primBoundMap = Map.fromList [("maxBound",PrimMaxBound),
                             ("minBound",PrimMinBound),
                             ("umaxBound",PrimUMaxBound)]
incDec = Map.fromList [("increment",Op.Add),("decrement",Op.Sub),
                       ("fincrement",Op.FAdd),("fdecrement",Op.FSub)]

ashow op = toAtom (show op)
binOpMap = Map.fromList [ ashow op ==> (op, starHash +> starHash +> starHash)
        | op :: Op.BinOp <- [minBound .. maxBound] ]
unOpMap = Map.fromList [ ashow op ==> (op,starHash +> starHash)
    | op :: Op.UnOp <- [minBound .. maxBound] ]
convOpMap = Map.fromList [ ashow op ==> (op,starHash +> starHash)
    | op :: Op.ConvOp <- [minBound .. maxBound] ]

-- | A safe version of 'zipWith'.
pairWith  :: (a -> b -> c)
          -> [a] -> [b] -> Maybe [c]
pairWith f xs ys = g xs ys [] where
    g [] [] rs = Just $ reverse rs
    g (x:xs) (y:ys) rs = g xs ys (f x y:rs)
    g _ _ _ = Nothing

ePrim prim as t = EPrim prim as t

processPrim :: MonadWarn m
    => DataTable
    -> SrcLoc     -- ^ location of declaration
    -> Atom       -- ^ primitive name
    -> [E]        -- ^ arguments
    -> E          -- ^ return type
    -> Requires   -- ^ c requires
    -> m E        -- ^ result
processPrim dataTable srcLoc pName args rType req = ans where
    passThrough = EPrim (PrimPrim pName) args rType
    ans = checkOp binOpMap doBinOp $ checkOp unOpMap (doUnOp Op.UnOp) $
        checkOp convOpMap (doUnOp Op.ConvOp) primCheckOther
    checkOp table yesMatch noMatch = case Map.lookup pName table of
        Just (op,ty) -> checkType ty (return passThrough) (yesMatch op)
        Nothing -> noMatch
    primCheckOther = case Map.lookup pName plainPrimMap of
        Just ty -> checkType ty (return passThrough) (primOther pName args)
        Nothing -> primPrefix (show pName) args
      where primOther "box" [a] = return ans where
                Just (ExtTypeBoxed cna _ _) = lookupExtTypeInfo dataTable rType
                ans = ELit litCons { litName = cna, litArgs = [a], litType = rType }
            primOther "unbox" [a] = return ans where
                (vara:_) = newIds (freeVars (a,rType))
                ans = unbox dataTable a vara $ \tvra -> EVar tvra
            primOther "seq" [a,b] = return $ prim_seq a b
            primOther "exitFailure__" [_] = return $ EError "" rType
            primOther "options_target" _ = return (ELit (LitInt 0 rType))
            primOther "constPeekByte" [a] = return $ ePrim (Peek Op.bits8) [a] rType
            primOther op [a] | Just x <- Map.lookup op incDec = do
                (pa,(ta,sta)) <- extractPrimitive dataTable a
                Just ret <- return $ boxResult dataTable rType $ \tr str ->
                    ePrim (Op (Op.BinOp x (stringToOpTy ta) (stringToOpTy ta)) tr)
                        [pa, ELit (LitInt 1 sta)] str
                return ret
            primOther op [] | Just x <- Map.lookup op primBoundMap = do
                Just res <- return $ boxResult dataTable rType $ \tr str ->
                    ePrim (PrimTypeInfo tr tr x) [] str
                return res
            primOther op [] | Just x <- lookup op ["zero" ==> 0,"one" ==> 1] = do
                Just res <- return $ boxResult dataTable rType $ \tr str ->
                    ELit (LitInt x str)
                return res
            -- since the primitive was found in the plainPrimMap file and
            -- typechecked we pass it through unchanged.
            primOther _ _ = return passThrough
    preType n s = getPrefix n s >>= Op.readTy
    checkType' ty os = checkType ty (return passThrough) os
    primPrefix (preType "peek." -> Just c) ~[a,w] = checkType'
        (hash +> state +> utup state hash) $ return
            (ePrim (Peek c) [w,a] rType)
    primPrefix (preType "poke." -> Just c) ~[a,v,w] = checkType'
        (hash +> hash +> state +> state) $ return
           (ePrim (Poke c) [w,a,v] rType)
    primPrefix (preType "sizeOf." -> Just c) _ = primInfo c Op.bits32 PrimSizeOf
    primPrefix (preType "alignmentOf." -> Just c) _ = primInfo c Op.bits32 PrimAlignmentOf
    primPrefix (preType "maxBound." -> Just c) _ = primInfo c c PrimMaxBound
    primPrefix (preType "minBound." -> Just c) _ = primInfo c c PrimMinBound
    primPrefix (preType "umaxBound." -> Just c) _ = primInfo c c PrimUMaxBound
    primPrefix (getPrefix "options_" -> Just c) _ =
        return (ePrim (CConst req (packString $ "JHC_" ++ c)) [] rType)
    primPrefix (getPrefix "const." -> Just c) _ = checkType' star $ do
        Just ret <- return $ boxResult dataTable rType $ \tr str ->
            ePrim (CConst req $ packString c) [] str
        return ret
    primPrefix (getPrefix "error." -> Just c) _ = return (EError c rType)
    primPrefix _ _ = primUnknown
    primInfo c cr wh = checkType' (hash +> hash) $ return
       (ePrim (PrimTypeInfo c cr wh) [] rType)
    primUnknown = do
        warn srcLoc CodeWarning $
                    printf "Unknown primitive '%s'" (fromAtom pName :: String)
        return passThrough
    doBinOp op = do
        let [a,b] = args
        (pa,(ta,_)) <- extractPrimitive dataTable a
        (pb,(tb,_)) <- extractPrimitive dataTable b
        Just res <- return $ boxResult dataTable rType $ \tr str ->
                 ePrim Op { primCOp = Op.BinOp op (stot op 1 ta) (stot op 2 tb), primRetTy = tr } [pa, pb] str
        return res
    doUnOp bOp op = do
        let [a] = args
        (pa,(ta,_)) <- extractPrimitive dataTable a
        Just res <- return $ boxResult dataTable rType $ \tr str ->
                 ePrim Op { primCOp = bOp op (stot op 1 ta), primRetTy = tr } [pa] str
        return res

    checkType (tas :-> trt) onFail onPass =
        case pairWith match tas (map getType args) of
            Just cs | and cs, match trt rType -> onPass
            _ -> do
                warn srcLoc CodeWarning $
                    printf "Primitive type mismatch. expected '%s' but found '%s -> %s'"
                        (show (tas :-> trt)) (show $ map (getType . getType) args) (show $ getType rType)
                onFail

match k e = g k where
    t = getType e
    g BState = isState_ e
    g (BTup ks) = case e of
        ELit (LitCons { litName = n, litArgs = as }) ->
            n == unboxedNameTuple TypeConstructor (length as) && matches ks as
        _ -> False
    g (BKind k) = f k
    -- check the kind
    f Star = t == eStar
    f KHash = t == eHash
    f KQuestQuest = t == eStar || t == eHash
    f _  = False

matches ks es = maybe False and $ pairWith match ks es

type T = E

boxResult :: DataTable -> T -> (Ty -> T -> E) -> Maybe E
boxResult dataTable t fn = mdo
        (res,(ta,sta)) <- boxPrimitive dataTable (fn (stringToOpTy ta) sta) t
	return res

stringToOpTy :: ExtType -> Ty
stringToOpTy s = stringToOpTy' "" s

stringToOpTy' :: String -> ExtType -> Ty
stringToOpTy' x (show -> s) = case readTy s of
    Just t -> t
    _ -> error $ printf "stringToOpTy(%s): '%s'" x s

stot :: Show a => a -> Int -> ExtType -> Ty
stot op n s = stringToOpTy' (show op ++ show n) s

unbox :: DataTable -> E -> Id -> (TVr -> E) -> E
unbox dataTable e vn wtd = eCase e  [Alt (litCons { litName = cna, litArgs = [tvra], litType = te }) (wtd tvra)] Unknown where
    te = getType e
    tvra = tVr vn sta
    (ExtTypeBoxed cna sta _) = fromMaybe (error $ "lookupExtTypeInfo(unbox): " ++ show te) $ lookupExtTypeInfo dataTable te
