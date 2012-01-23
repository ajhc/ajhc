module DataConstructors(
    AliasType(..),
    boxPrimitive,
    collectDeriving,
    conSlots,
    constructionExpression,
    Constructor(..),
    DataFamily(..),
    DataTable(..),
    DataTableMonad(..),
    dataTablePrims,
    deconstructionExpression,
    deriveClasses,
    extractIO,
    extractIO',
    extractPrimitive,
    ExtTypeInfo(..),
    extTypeInfoExtType,
    followAlias,
    followAliases,
    getConstructor,
    getConstructorArities,
    getProduct,
    getSiblings,
    lookupExtTypeInfo,
    mktBox,
    modBox,
    numberSiblings,
    onlyChild,
    pprintTypeOfCons,
    primitiveAliases,
    removeNewtypes,
    samplePrimitiveDataTable,
    showDataTable,
    Slot(..),
    slotTypes,
    slotTypesHs,
    tAbsurd,
    toDataTable,
    typesCompatable,
    updateLit
    ) where

import Control.Monad.Identity
import Control.Monad.Writer(tell,execWriter)
import Data.Maybe
import Data.Monoid hiding(getProduct)
import List(sortBy)
import qualified Data.Map as Map hiding(map)

import C.Prims
import Data.Binary
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.Binary()
import E.E
import E.Show
import E.Subst
import E.Traverse
import E.TypeCheck
import E.Values
import FrontEnd.Class(instanceName)
import FrontEnd.HsSyn
import FrontEnd.SrcLoc
import FrontEnd.Syn.Traverse
import FrontEnd.Tc.Type
import GenUtil
import Info.Types
import Name.Id
import Name.Name as Name
import Name.Names
import Name.VConsts
import PrimitiveOperators
import Support.CanType
import Support.FreeVars
import Support.MapBinaryInstance
import Support.Unparse
import Util.HasSize
import Util.SameShape
import Util.SetLike as S
import Util.VarName
import qualified Cmm.Op as Op
import qualified Util.Graph as G
import qualified Util.Seq as Seq

tipe' (TAp t1 t2) = liftM2 eAp (tipe' t1) (tipe' t2)
tipe' (TArrow t1 t2) =  do
    t1' <- tipe' t1
    t2' <- tipe' t2
    return $ EPi (tVr emptyId (t1')) t2'
tipe' (TCon (Tycon n k)) | Just n' <- Map.lookup n primitiveAliases = return $ ELit litCons { litName = n', litType = kind k }
tipe' (TCon (Tycon n k)) =  return $ ELit litCons { litName = n, litType = kind k }
tipe' (TVar tv@Tyvar { tyvarKind = k}) = do
    v <- lookupName tv
    return $ EVar $ tVr v (kind k)
tipe' (TForAll [] (_ :=> t)) = tipe' t
tipe' (TExists [] (_ :=> t)) = tipe' t
tipe' (TForAll xs (_ :=> t)) = do
    xs' <- flip mapM xs $ \tv -> do
        v <- newName (map anonymous [35 .. ]) () tv
        return $ tVr v (kind $ tyvarKind tv)
    t' <- tipe' t
    return $ foldr EPi t' xs' -- [ tVr n (kind k) | n <- [2,4..] | k <- xs ]
tipe' ~(TExists xs (_ :=> t)) = do
    xs' <- flip mapM xs $ \tv -> do
        --v <- newName [70,72..] () tv
        --return $ tVr v (kind $ tyvarKind tv)
        return $ (kind $ tyvarKind tv)
    t' <- tipe' t
    return $ ELit litCons { litName = unboxedNameTuple TypeConstructor (length xs' + 1), litArgs = (t':xs'), litType = eHash }

kind (KBase KUTuple) = eHash
kind (KBase KHash) = eHash
kind (KBase Star) = eStar
kind (Kfun k1 k2) = EPi (tVr emptyId (kind k1)) (kind k2)
kind (KVar _) = error "Kind variable still existing."
kind _ = error "DataConstructors.kind"

data AliasType = NotAlias | ErasedAlias | RecursiveAlias
    deriving(Eq,Ord,Show)
    {-! derive: Binary !-}

-- these apply to types
data DataFamily =
    DataAbstract                   -- abstract internal type, has children of representation unknown and irrelevant.
    | DataNone                     -- children don't apply. data constructor for instance
    | DataPrimitive                -- primitive type, children are all numbers.
    | DataEnum {-# UNPACK #-} !Int -- bounded integral type, argument is maximum number
    | DataNormal [Name]            -- child constructors
    deriving(Eq,Ord,Show)
    {-! derive: Binary !-}

-- | Record describing a data type.
-- * is also a data type containing the type constructors, which are unlifted, yet boxed.

data Constructor = Constructor {
    conName      :: Name,         -- name of constructor
    conType      :: E,            -- type of constructor
    conExpr      :: E,            -- expression which constructs this value
    conOrigSlots :: [Slot],       -- original slots
    conAlias     :: !AliasType,   -- whether this is a simple alias and has no tag of its own.
    conInhabits  :: Name,         -- what constructor it inhabits, similar to conType, but not quite.
    conVirtual   :: Maybe [Name], -- whether this is a virtual constructor that translates into an enum and its siblings
    conChildren  :: DataFamily
    } deriving(Show)
    {-! derive: Binary !-}

data Slot =
    SlotNormal E
    | SlotUnpacked E !Name [E]
    | SlotExistential TVr
    deriving(Eq,Ord,Show)
    {-! derive: Binary !-}

mapESlot f (SlotExistential t) = SlotExistential t { tvrType = f (tvrType t) }
mapESlot f (SlotNormal e) = SlotNormal $ f e
mapESlot f (SlotUnpacked e n es) = SlotUnpacked (f e) n (map f es)

conSlots s = getSlots $ conOrigSlots s

getSlots ss = concatMap f ss where
    f (SlotNormal e) = [e]
    f (SlotUnpacked _ _ es) = es
    f (SlotExistential e) = [tvrType e]

getHsSlots ss = map f ss where
    f (SlotNormal e) = e
    f (SlotUnpacked e _ es) = e
    f (SlotExistential e) = tvrType e

newtype DataTable = DataTable {
    constructorMap :: (Map.Map Name Constructor)
    }
    {-! derive: Monoid !-}

instance Binary DataTable where
    put (DataTable dt) = putMap dt
    get = fmap DataTable getMap

emptyConstructor = Constructor {
                conName = error "emptyConstructor.conName",
                conType = Unknown,
                conOrigSlots = [],
                conExpr = Unknown,
                conInhabits = error "emptyConstructor.conInhabits",
                conAlias = NotAlias,
                conVirtual = Nothing,
                conChildren = DataNone
                }

instance HasSize DataTable where
    size (DataTable d) = Map.size d

{-# NOINLINE getConstructor #-}
getConstructor :: Monad m => Name -> DataTable -> m Constructor
getConstructor n _ | isJust me = return (emptyConstructor {
    conName = n, conType = e,
    conExpr = foldr ELam (foldl eAp (mktBox e) (map EVar tvrs)) tvrs,
    conInhabits = s_Star, conOrigSlots = map SlotNormal sts }) where
        sts = map tvrType ss
        tvrs = [ tvr { tvrIdent = i , tvrType = t } | i <- anonymousIds | t <- sts ]
        (_,ss) = fromPi e
        me@(~(Just e)) = fromConjured modBox n `mplus` fromConjured modAbsurd n
getConstructor n _ | RawType <- nameType n = return $ primitiveConstructor n
getConstructor n _ | Just v <- fromUnboxedNameTuple n, DataConstructor <- nameType n = return $ snd $ tunboxedtuple v
getConstructor n _ | Just v <- fromUnboxedNameTuple n, TypeConstructor <- nameType n = return $ fst $ tunboxedtuple v
getConstructor n (DataTable map) = case Map.lookup n map of
    Just x -> return x
    Nothing -> fail $ "getConstructor: " ++ show (nameType n,n)

-- | return the single constructor of product types
getProduct :: Monad m => DataTable -> E -> m Constructor
getProduct dataTable e | (ELit LitCons { litName = cn }) <- followAliases dataTable e, Just c <- getConstructor cn dataTable = f c where
    f c | DataNormal [x] <- conChildren c = getConstructor x dataTable
        | otherwise = fail "Not Product type"
getProduct _ _ = fail "Not Product type"

tunboxedtuple :: Int -> (Constructor,Constructor)
tunboxedtuple n = (typeCons,dataCons) where
        dataCons = emptyConstructor {
            conName = dc,
            conType = dtipe,
            conOrigSlots = map (SlotNormal . EVar) typeVars,
            conExpr =  foldr ($) (ELit litCons { litName = dc, litArgs = map EVar vars, litType = ftipe }) (map ELam vars),
            conInhabits = tc
           }
        typeCons = emptyConstructor {
            conName = tc,
            conType = foldr EPi eHash (replicate n tvr { tvrType = eStar }),
            conOrigSlots = replicate n (SlotNormal eStar),
            conExpr = tipe,
            conInhabits = s_Hash,
            conChildren = DataNormal [dc]
           }

        dc = unboxedNameTuple DataConstructor n
        tc = unboxedNameTuple TypeConstructor n
        tipe = foldr ELam ftipe typeVars
        typeVars = take n [ tvr { tvrType = eStar, tvrIdent = v } | v <- anonymousIds ]
        vars =  [ tvr { tvrType = EVar t, tvrIdent = v } | v <- map anonymous [ n + 8, n + 9 ..] | t <- typeVars ]
        ftipe = ELit (litCons { litName = tc, litArgs = map EVar typeVars, litType = eHash })
        dtipe = foldr EPi (foldr EPi ftipe [ v { tvrIdent = emptyId } | v <- vars]) typeVars

-- | conjured data types, these data types are created as needed and can be of any type, their
-- actual type is encoded in their names.
--
-- Absurd - this is a type that it used to default otherwise unconstrained
-- types, it is not special in any particular way but is just an arbitrary type
-- to give to things.
--
-- Box - this type can be used to represent any boxed values. It is considered
-- equivalent to all boxed values so is not a very precise type. It is used in
-- the final stages of compilation before core mangling so that optimizations
-- that were previously blocked by type variables can be carried out.

tAbsurd k = ELit (litCons { litName = nameConjured modAbsurd k, litArgs = [], litType = k })
mktBox  k = ELit (litCons { litName = nameConjured modBox k, litArgs = [], litType = k, litAliasFor = af }) where
    af = case k of
        EPi TVr { tvrType = t1 } t2 -> Just (ELam tvr { tvrType = t1 } (mktBox t2))
        _ -> Nothing

tarrow = emptyConstructor {
            conName = tc_Arrow,
            conType = EPi (tVr emptyId eStar) (EPi (tVr emptyId eStar) eStar),
            conOrigSlots = [SlotNormal eStar,SlotNormal eStar],
            conExpr = ELam (tVr va1 eStar) (ELam (tVr va2 eStar) (EPi (tVr emptyId (EVar $ tVr va1 eStar)) (EVar $ tVr va2 eStar))),
            conInhabits = s_Star,
            conChildren = DataAbstract
        }

primitiveConstructor name = emptyConstructor {
    conName = name,
    conType = eHash,
    conExpr = ELit (litCons { litName = name, litArgs = [], litType = eHash }),
    conInhabits = s_Hash,
    conChildren = DataPrimitive
    }

primitiveTable = concatMap f allCTypes  where
    f (dc,tc,rt) = [typeCons,dataCons] where
        dataCons = emptyConstructor {
            conName = dc,
            conType = tipe,
            conOrigSlots = [SlotNormal rt],
            conExpr = ELam (tVr va1 rt) (ELit (litCons { litName = dc, litArgs = [EVar (tVr va1 rt)], litType = tipe })),
            conInhabits = tc
           }
        typeCons = emptyConstructor {
            conName = tc,
            conType = eStar,
            conExpr = tipe,
            conInhabits = s_Star,
            conChildren = DataNormal [dc]
           }
        tipe = ELit (litCons { litName = tc, litArgs = [], litType = eStar })

typesCompatable :: forall m . Monad m => DataTable -> E -> E -> m ()
typesCompatable dataTable a b = f etherealIds a b where
        f :: [Id] -> E -> E -> m ()
        f _ (ESort a) (ESort b) = when (a /= b) $ fail $ "Sorts don't match: " ++ pprint (ESort a,ESort b)
        f _ (EVar a) (EVar b) = when (a /= b) $ fail $ "Vars don't match: " ++ pprint (a,b)
        -- we expand aliases first, because the newtype might have phantom types as arguments
        f c (ELit (LitCons {  litAliasFor = Just af, litArgs = as })) b = do
            f c (foldl eAp af as) b
        f c a (ELit (LitCons {  litAliasFor = Just af, litArgs = as })) = do
            f c a (foldl eAp af as)
        f c (ELit LitCons { litName = n, litArgs = xs, litType = t }) (ELit LitCons { litName = n', litArgs = xs', litType = t' }) | n == n' = do
            f c t t'
            when (not $ sameShape1 xs xs') $ fail "Arg lists don't match"
            zipWithM_ (f c) xs xs'
        f c (EAp a b) (EAp a' b') = do
            f c a a'
            f c b b'
        f c (ELam va ea) (ELam vb eb) = lam va ea vb eb c
        f c (EPi va ea) (EPi vb eb)   = lam va ea vb eb c
        f c (EPi (TVr { tvrIdent = eid, tvrType =  a}) b) (ELit (LitCons { litName = n, litArgs = [a',b'], litType = t })) | eid == emptyId, conName tarrow == n, t == eStar = do
            f c a a'
            f c b b'
        f c (ELit (LitCons { litName = n, litArgs = [a',b'], litType = t })) (EPi (TVr { tvrIdent = eid, tvrType =  a}) b)  | eid == emptyId, conName tarrow == n, t == eStar = do
            f c a a'
            f c b b'
        f _ a b | boxCompat a b || boxCompat b a = return ()
        f _ a b = fail $ "Types don't match:" ++ pprint (a,b)

        lam :: TVr -> E -> TVr -> E -> [Id] -> m ()
        lam va ea vb eb ~(c:cs) = do
            f (c:cs) (tvrType va) (tvrType vb)
            f cs (subst va (EVar va { tvrIdent = c }) ea) (subst vb (EVar vb { tvrIdent = c }) eb)
        boxCompat (ELit (LitCons { litName = n }))  t | Just e <- fromConjured modBox n =  e == getType t
        boxCompat _ _ = False

extractPrimitive :: Monad m => DataTable -> E -> m (E,(ExtType,E))
extractPrimitive dataTable e = case followAliases dataTable (getType e) of
    st@(ELit LitCons { litName = c, litArgs = [], litType = t })
        | t == eHash -> return (e,(show c,st))
        | otherwise -> do
            Constructor { conChildren = DataNormal [cn] }  <- getConstructor c dataTable
            Constructor { conOrigSlots = [SlotNormal st] } <- getConstructor cn dataTable
            (ELit LitCons { litName = n, litArgs = []}) <- return $ followAliases dataTable st
            let tvra = tVr vn st
                (vn:_) = newIds (freeIds e)
            return (eCase e  [Alt (litCons { litName = cn, litArgs = [tvra], litType = (getType e) }) (EVar tvra)] Unknown,(show n,st))
    e' -> fail $ "extractPrimitive: " ++ show (e,e')

boxPrimitive ::
    Monad m
    => DataTable
    -> E         -- primitive to box
    -> E         -- what type we want it to have
    -> m (E,(ExtType,E))
boxPrimitive dataTable e et = case followAliases dataTable et of
    st@(ELit LitCons { litName = c, litArgs = [], litType = t })
        | t == eHash -> return (e,(show c,st))
        | otherwise -> do
            Constructor { conChildren = DataNormal [cn] }  <- getConstructor c dataTable
            Constructor { conOrigSlots = [SlotNormal st] } <- getConstructor cn dataTable
            (ELit LitCons { litName = n, litArgs = []}) <- return $ followAliases dataTable st
            let tvra = tVr vn st
                (vn:_) = newIds (freeVars (e,et))
            if isManifestAtomic e then
                return $ (ELit litCons { litName = cn, litArgs = [e], litType = et },(show n,st))
             else
                return $ (eStrictLet tvra e $ ELit litCons { litName = cn, litArgs = [EVar tvra], litType = et },(show n,st))
    e' -> fail $ "boxPrimitive: " ++ show (e,e')

extractIO :: Monad m => E -> m E
extractIO e = f e where
    f (ELit LitCons { litName = c, litArgs = [x] }) | c == tc_IO  = return x
    f (ELit LitCons { litAliasFor = Just af, litArgs = as }) = f (foldl eAp af as)
    f _ = fail "extractIO: not an IO type"

extractIO' :: E -> (Bool,E)
extractIO' e = case extractIO e of
    Just x -> (True,x)
    Nothing -> (False,e)

data ExtTypeInfo
    = ExtTypeVoid                  -- maps to 'void'
    | ExtTypeRaw ExtType           -- value is an unboxed type suitable for passing with the argument calling convention
    | ExtTypeBoxed Name E ExtType  -- boxed type, name is constructor of box, E is type of the slice, and ExtType is the calling convention to use

extTypeInfoExtType (ExtTypeRaw et) = et
extTypeInfoExtType (ExtTypeBoxed _ _ et) = et
extTypeInfoExtType ExtTypeVoid = "void"

lookupExtTypeInfo :: Monad m => DataTable -> E -> m ExtTypeInfo
lookupExtTypeInfo dataTable oe = f oe where
    -- handle the void context ones first
    f e@(ELit LitCons { litName = c }) | c == tc_Unit || c == tc_State_ = return ExtTypeVoid
    -- if the constructor is in the external type map, replace its external
    -- type with the one in the map
    f e@(ELit LitCons { litName = c }) | Just et <- Map.lookup c typeTable = do
        res <- g e
        return $ case res of
            ExtTypeRaw _ -> ExtTypeRaw et
            ExtTypeBoxed b t _ -> ExtTypeBoxed b t et
            ExtTypeVoid -> ExtTypeVoid
    f e = g e
    -- if we are a single constructor data type with a single foreignable unboxed
    -- slot, we are foreiginable
    g (ELit LitCons { litName = c, litAliasFor = Nothing })
        | Just Constructor { conChildren = DataNormal [cn] }  <- getConstructor c dataTable,
          Just Constructor { conOrigSlots = [SlotNormal st] } <- getConstructor cn dataTable,
          Just (ExtTypeRaw et) <- lookupExtTypeInfo dataTable st = return $ ExtTypeBoxed cn st et
    -- if we are a raw type, we can be foreigned
    g (ELit LitCons { litName = c, litAliasFor = Nothing }) | Just et <- Map.lookup c rawExtTypeMap = return (ExtTypeRaw et)
    g e | Just e' <- followAlias dataTable e = f e'
        | otherwise = fail $ "lookupExtTypeInfo: " ++ show (oe,e)

followAlias :: Monad m => DataTable -> E -> m E
followAlias _ (ELit LitCons { litAliasFor = Just af, litArgs = as }) = return (foldl eAp af as)
followAlias _  _ = fail "followAlias: not alias"

followAliases :: DataTable -> E -> E
followAliases _dataTable e = f e where
    f (ELit LitCons { litAliasFor = Just af, litArgs = as }) = f (foldl eAp af as)
    f e = e

dataTablePrims = DataTable $ Map.fromList ([ (conName x,x) | x <- tarrow:primitiveTable ])

deriveClasses :: IdMap Comb -> DataTable -> [(SrcLoc,Name,Name)] -> [(TVr,E)]
deriveClasses cmap dt@(DataTable mp) ctd = concatMap f ctd where
    f (_,cd,t) | Just c <- getConstructor t dt, TypeConstructor == nameType (conName c), Just is <- conVirtual c = g is c cd
    f _ = []
    g is c cl = h cl where
        lupvar v = EVar (combHead comb) where
            Just comb = mlookup (toId v) cmap
        typ = conExpr c
        DataNormal [con] = conChildren c
        Just conr = getConstructor con (DataTable mp)
        [it@(ELit LitCons { litName = it_name })] = conSlots conr
        Just itr = getConstructor it_name (DataTable mp)
        DataEnum mv = conChildren itr
        v1 = tvr { tvrIdent = anonymous 1,  tvrType = typ }
        v2 = tvr { tvrIdent = anonymous 2,  tvrType = typ }
        i1 = tvr { tvrIdent = anonymous 3,  tvrType = it }
        i2 = tvr { tvrIdent = anonymous 4,  tvrType = it }
        b3 = tvr { tvrIdent = anonymous 5,  tvrType = tBoolzh }
        val1 = tvr { tvrIdent = anonymous 7, tvrType = typ }
        unbox e = ELam v1 (ELam v2 (ec (EVar v1) i1 (ec (EVar v2) i2 e)))  where
            ec v i e = eCase v [Alt (litCons { litName = con, litArgs = [i], litType = typ }) e] Unknown
        h cl | cl == class_Eq = [mkCmpFunc v_equals Op.Eq]
        h cl | cl == class_Ord = [
                mkCmpFunc v_geq Op.UGte,
                mkCmpFunc v_leq Op.ULte,
                mkCmpFunc v_lt  Op.ULt,
                mkCmpFunc v_gt  Op.UGt]
        h cl | Just ans <- lookup cl mthds = ans where
            mthds = [(class_Enum,[
                    (iv_te,ib_te),
                    (iv_fe,ib_fe),
                    iv v_succ succ_body,
                    iv v_pred pred_body,
                    iv v_enumFrom from_body,
                    iv v_enumFromTo fromTo_body,
                    iv v_enumFromThen fromThen_body,
                    iv v_enumFromThenTo fromThenTo_body
                ]),
                (class_Ix,[
                    iv v_range range_body,
--                    iv v_inRange inRange_body,
                    iv v_index index_body
                ])]
            iv_te = setProperty prop_INSTANCE tvr { tvrIdent = toId $ instanceName v_toEnum (nameName $ conName c), tvrType = getType ib_te }
            iv_fe = setProperty prop_INSTANCE tvr { tvrIdent = toId $ instanceName v_fromEnum (nameName $ conName c), tvrType = getType ib_fe }
            iv fname body = (setProperty prop_INSTANCE tvr { tvrIdent = toId $ instanceName fname (nameName $ conName c), tvrType = getType body },body)
            succ_body = foldl EAp (lupvar v_enum_succ) [typ, box, debox, max]
            pred_body = foldl EAp (lupvar v_enum_pred) [typ, box, debox]
            from_body = foldl EAp (lupvar v_enum_from) [typ, box, debox, max]
            fromTo_body = foldl EAp (lupvar v_enum_fromTo) [typ, box, debox]
            fromThen_body = foldl EAp (lupvar v_enum_fromThen) [typ, box, debox, max]
            fromThenTo_body = foldl EAp (lupvar v_enum_fromThenTo) [typ, box, debox]
            range_body = foldl EAp (lupvar v_ix_range) [typ, box, debox]
            --inRange_body = foldl EAp (lupvar v_ix_inRange) [typ, box, debox]
            index_body = foldl EAp (lupvar v_ix_index) [typ, box, debox]

            ib_te = foldl EAp (lupvar v_enum_toEnum) [typ, box, toEzh (mv - 1)]
            ib_fe = ELam val1 (create_uintegralCast_toInt con tEnumzh (EVar val1))

            max = ELit (LitInt (fromIntegral $ mv - 1) tEnumzh)

            box = ELam i1 (ELit (litCons { litName = con, litArgs = [EVar i1], litType = typ }))
            debox = ELam v1 (ec (EVar v1) i1 (EVar i1))  where
                ec v i e = eCase v [Alt (litCons { litName = con, litArgs = [i], litType = typ }) e] Unknown

        h _ = []
        mkCmpFunc fname op = (iv_eq,ib_eq) where
            ib_eq = unbox (eStrictLet b3 (oper_IIB op (EVar i1) (EVar i2)) (ELit (litCons { litName = dc_Boolzh, litArgs = [EVar b3], litType = tBool })))
            iv_eq = setProperty prop_INSTANCE tvr { tvrIdent = toId $ instanceName fname (nameName $ conName c), tvrType = getType ib_eq }
    oper_IIB op a b = EPrim (APrim (Op (Op.BinOp op Op.bits16 Op.bits16) Op.bits16) mempty) [a,b] tBoolzh

create_integralCast conv c1 t1 c2 t2 e t = eCase e [Alt (litCons { litName = c1, litArgs = [tvra], litType = te }) cc] Unknown  where
    te = getType e
    ELit LitCons { litName = n1, litArgs = [] } = t1
    ELit LitCons { litName = n2, litArgs = [] } = t2
    Just n1' = nameToOpTy n1
    Just n2' = nameToOpTy n2
    tvra =  tVr va2 t1
    tvrb =  tVr va3 t2
    cc = if n1 == n2 then ELit (litCons { litName = c2, litArgs = [EVar tvra], litType = t }) else
        eStrictLet  tvrb (EPrim (APrim (Op (Op.ConvOp conv n1') n2') mempty) [EVar tvra] t2)  (ELit (litCons { litName = c2, litArgs = [EVar tvrb], litType = t }))

nameToOpTy n = do RawType <- return $ nameType n; Op.readTy (show n)

create_uintegralCast_toInt c1 t1 e = create_integralCast Op.U2U c1 t1 dc_Int tIntzh e tInt

updateLit :: DataTable -> Lit e t -> Lit e t
updateLit _ l@LitInt {} = l
updateLit dataTable lc@LitCons { litAliasFor = Just {} } = lc
updateLit dataTable lc@LitCons { litName = n } =  lc { litAliasFor = af } where
    af = do
        Constructor { conChildren = DataNormal [x], conOrigSlots = cs } <- getConstructor n dataTable
        Constructor { conAlias = ErasedAlias, conOrigSlots = [SlotNormal sl] } <- getConstructor x dataTable
        return (foldr ELam sl [ tVr i s | s <- getSlots cs | i <- anonymousIds])

removeNewtypes :: DataTable -> E -> E
removeNewtypes dataTable e = runIdentity (f e) where
    f ec@ECase {} = emapEGH f f return ec { eCaseAlts = map g (eCaseAlts ec) } where
        g (Alt l e) = Alt (gl $ updateLit dataTable l) e
    f (ELit l) = emapEGH f f return (ELit (gl $ updateLit dataTable l))
    f e = emapEGH f f return e
    gl lc@LitCons { litAliasFor = Just e }  = lc { litAliasFor = Just $ removeNewtypes dataTable e }
    gl l = l

collectDeriving :: [HsDecl] -> [(SrcLoc,Name,Name)]
collectDeriving ds = concatMap f ds where
    f decl@HsNewTypeDecl {} = g decl
    f decl@HsDataDecl {} = g decl
    f decl@HsDeclDeriving {} = h decl
    f _ = []
    g decl = [(hsDeclSrcLoc decl, toName ClassName c,
        toName TypeConstructor (hsDeclName decl)) | c <- hsDeclDerives decl ]
    h decl@(hsDeclClassHead -> ch) | [(ltc -> Just t)] <- hsClassHeadArgs ch = [(hsDeclSrcLoc decl,toName ClassName (hsClassHead ch), t)] where
            ltc (HsTyApp t1 _) = ltc t1
            ltc (HsTyCon n) = Just (toName TypeConstructor n)
            ltc x = Nothing
    h _ = []

{-# NOINLINE toDataTable #-}
toDataTable :: (Map.Map Name Kind) -> (Map.Map Name Type) -> [HsDecl] -> DataTable -> DataTable
toDataTable km cm ds currentDataTable = newDataTable  where
    newDataTable = DataTable (Map.mapWithKey fixupMap $ Map.fromList [ (conName x,procNewTypes x) | x <- ds', conName x `notElem` keys primitiveAliases ])
    fullDataTable = (newDataTable `mappend` currentDataTable)
    procNewTypes c = c { conExpr = f (conExpr c), conType = f (conType c), conOrigSlots = map (mapESlot f) (conOrigSlots c) } where
        f = removeNewtypes fullDataTable
    fixupMap k _ | Just n <- getConstructor k dataTablePrims = n
    fixupMap _ n = n
    ds' = Seq.toList $ execWriter (mapM_ f ds)
    newtypeLoopBreakers = map fst $ fst $  G.findLoopBreakers (const 0) (const True) (G.newGraph newtypeDeps fst snd) where
        newtypeDeps = [ (n,concatMap (fm . hsBangType) $ hsConDeclArgs c) | HsNewTypeDecl { hsDeclName = n, hsDeclCon = c } <- ds  ]
        fm t = execWriter $ f t
        f HsTyCon { hsTypeName = n } = tell [n]
        f t = traverseHsType_ f t
    f decl@HsNewTypeDecl {  hsDeclName = nn, hsDeclCon = c } = dt decl (if nn `elem` newtypeLoopBreakers then RecursiveAlias else ErasedAlias)  [c]
    f decl@HsDataDecl {  hsDeclCons = cs } = dt decl NotAlias  cs
    f _ = return ()
    dt decl NotAlias cs@(_:_:_) | all null (map hsConDeclArgs cs) = do
        let virtualCons'@(fc:_) = map (makeData NotAlias typeInfo) cs
            typeInfo@(theType,_,_) = makeType decl
            virt = Just (map conName virtualCons')
            f (n,vc) = vc { conExpr = ELit (litCons { litName = consName, litArgs = [ELit (LitInt (fromIntegral n) rtype)], litType = conType vc }), conVirtual = virt }
            virtualCons = map f (zip [(0 :: Int) ..] virtualCons')
            consName =  mapName (id,(++ "#")) $ toName DataConstructor (nameName (conName theType))
            rtypeName =  mapName (id,(++ "#")) $ toName TypeConstructor (nameName (conName theType))
            rtype = ELit litCons { litName = rtypeName, litType = eHash, litAliasFor = Just tEnumzh }
            dataCons = fc { conName = consName, conType = getType (conExpr dataCons), conOrigSlots = [SlotNormal rtype], conExpr = ELam (tVr (anonymous 3) rtype) (ELit (litCons { litName = consName, litArgs = [EVar (tVr (anonymous 6) rtype)], litType =  conExpr theType })) }
            rtypeCons = emptyConstructor {
                conName = rtypeName,
                conType = eHash,
                conExpr = rtype,
                conInhabits = s_Hash,
                conChildren = DataEnum (length virtualCons)
                }
        tell (Seq.fromList virtualCons)
        tell (Seq.singleton dataCons)
        tell (Seq.singleton rtypeCons)
        tell $ Seq.singleton theType { conChildren = DataNormal [consName], conVirtual = virt }
        return ()

    dt decl alias cs = do
        let dataCons = map (makeData alias typeInfo) cs
            typeInfo@(theType,_,_) = makeType decl
        tell (Seq.fromList dataCons)
        tell $ Seq.singleton theType { conChildren = DataNormal (map conName dataCons) }

    makeData alias (theType,theTypeArgs,theTypeExpr) x = theData where
        theData = emptyConstructor {
            conName = dataConsName,
            conType =foldr ($) (getType theExpr) (map EPi theTypeArgs),
            conOrigSlots = origSlots,
            conExpr = theExpr,
            conInhabits = conName theType,
            conAlias = alias
            }
        dataConsName =  toName Name.DataConstructor (hsConDeclName x)

        theExpr =  foldr ELam (strictize tslots $ ELit litCons { litName = dataConsName, litArgs = map EVar dvars, litType = theTypeExpr }) hsvars

        strictize tslots con = E.Subst.subst tvr { tvrIdent = sillyId } Unknown $ f tslots con where
            f (Left (v,False):rs) con = f rs con
            f (Left (v,True):rs) con = eStrictLet v (EVar v) (f rs con)
            f (Right (v,dc,rcs):rs) con = eCase (EVar v) [Alt pat (f rs con)] Unknown where
                pat = litCons { litName = dc, litArgs = rcs, litType = (getType v) }
            f [] con = con

        -- substitution is only about substituting type variables
        (ELit LitCons { litArgs = thisTypeArgs }, origArgs) = fromPi $ runVarName $ do
            let (vs,ty) = case Map.lookup dataConsName cm of Just (TForAll vs (_ :=> ty)) -> (vs,ty); ~(Just ty) -> ([],ty)
            mapM_ (newName anonymousIds ()) vs
            tipe' ty
        subst = substMap $ fromList [ (tvrIdent tv ,EVar $ tv { tvrIdent = p }) | EVar tv <- thisTypeArgs | p <- anonymousIds ]

        origSlots = map SlotExistential existentials ++ map f tslots where
            f (Left (e,_)) = SlotNormal (getType e)
            f (Right (e,n,es)) = SlotUnpacked (getType e) n (map getType es)
        hsvars = existentials ++ map f tslots where
            f (Left (e,_)) = e
            f (Right (e,_,_)) = e
        dvars = existentials ++ concatMap f tslots where
            f (Left (e,_)) = [e]
            f (Right (_,_,es)) = es
        tslots = f (newIds fvset) (map isHsBangedTy (hsConDeclArgs x)) origArgs where
            f (i:is) (False:bs) (e:es) = Left (e { tvrIdent = i, tvrType = subst (tvrType e) },False):f is bs es
            f (i:j:is) (True:bs) (e:es) = maybe  (Left (e { tvrIdent = i, tvrType = subst (tvrType e) },True):f is bs es) id $ g e (tvrType e) where
                g e te = do
                    ELit LitCons { litName = n } <- return $ followAliases fullDataTable te
                    Constructor { conChildren = DataNormal [dc] } <- getConstructor n fullDataTable
                    con <- getConstructor dc fullDataTable
                    case (conAlias con,slotTypes fullDataTable dc te) of
                        (ErasedAlias,[nt]) -> g e nt
                        (_,[st]) -> do
                            let nv = tvr { tvrIdent = j, tvrType = st }
                            return $ Right (e { tvrIdent = i, tvrType = subst (tvrType e)},dc,[nv]):f is bs es
                        _ -> fail "not unboxable"
            f _ [] [] = []
            f _ _ _ = error "DataConstructors.tslots"
            fvset = freeVars (thisTypeArgs,origArgs) `mappend` fromList (take (length theTypeArgs + 2) anonymousIds)

        -- existentials are free variables in the arguments, that arn't bound in the type
        existentials = values $ freeVars (map getType origArgs) S.\\ (freeVars thisTypeArgs :: IdMap TVr)

        -- arguments that the front end passes or pulls out of this constructor
        --hsArgs = existentials ++ [ tvr {tvrIdent = x} | tvr <- origArgs | x <- drop (5 + length theTypeArgs) [2,4..] ]

    makeType decl = (theType,theTypeArgs,theTypeExpr) where
        theTypeName = toName Name.TypeConstructor (hsDeclName decl)
        theKind = kind $ fromJust (Map.lookup theTypeName km)
        (theTypeFKind,theTypeKArgs') = fromPi theKind
        theTypeArgs = [ tvr { tvrIdent = x } | tvr  <- theTypeKArgs' | x <- anonymousIds ]
        theTypeExpr = ELit litCons { litName = theTypeName, litArgs = map EVar theTypeArgs, litType = theTypeFKind }
        theType = emptyConstructor {
            conName = theTypeName,
            conType = theKind,
            conOrigSlots = map (SlotNormal . tvrType) theTypeArgs,
            conExpr = foldr ($) theTypeExpr (map ELam theTypeArgs),
            conInhabits = if theTypeFKind == eStar then s_Star else s_Hash,
            conVirtual = Nothing,
            conChildren = undefined
            }

isHsBangedTy HsBangedTy {} = True
isHsBangedTy _ = False

getConstructorArities  :: DataTable -> [(Name,Int)]
getConstructorArities (DataTable dt) = [ (n,length $ conSlots c) | (n,c) <- Map.toList dt]

constructionExpression ::
    DataTable -- ^ table of data constructors
    -> Name   -- ^ name of said constructor
    -> E      -- ^ type of eventual constructor
    -> E      -- ^ saturated lambda calculus term
constructionExpression dataTable n typ@(ELit LitCons { litName = pn, litArgs = xs })
    | ErasedAlias <- conAlias mc = ELam var (EVar var)
    | RecursiveAlias <- conAlias mc = let var' = var { tvrType = st } in ELam var' (prim_unsafeCoerce (EVar var') typ)
    | pn == conName pc = sub (conExpr mc) where
    ~[st] = slotTypes dataTable n typ
    var = tvr { tvrIdent = vid, tvrType = typ }
    (vid:_) = newIds (freeVars typ)
    Just mc = getConstructor n dataTable
    Just pc = getConstructor (conInhabits mc) dataTable
    sub = substMap $ fromDistinctAscList [ (i,sl) | sl <- xs | i <- anonymousIds ]
constructionExpression wdt n e | Just fa <- followAlias wdt e  = constructionExpression wdt n fa
constructionExpression _ n e = error $ "constructionExpression: error in " ++ show n ++ ": " ++ show e

deconstructionExpression ::
    UniqueProducer m
    => DataTable  -- ^ table of data constructors
    -> Name       -- ^ name of said constructor
    -> E          -- ^ type of pattern
    -> [TVr]      -- ^ variables to be bound
    -> E          -- ^ body of alt
    -> m (Alt E)  -- ^ resulting alternative
deconstructionExpression dataTable name typ@(ELit LitCons { litName = pn, litArgs = xs }) vs  e | pn == conName pc = ans where
    Just mc = getConstructor name dataTable
    Just pc = getConstructor (conInhabits mc) dataTable
    sub = substMap $ fromDistinctAscList [ (i,sl) | sl <- xs | i <- anonymousIds ]
    ans = case conVirtual mc of
        Just _ -> return $ let ELit LitCons {  litArgs = [ELit (LitInt n t)] } = conExpr mc in Alt (LitInt n t) e
        Nothing -> do
            let f vs (SlotExistential t:ss) rs ls = f vs ss (t:rs) ls
                f (v:vs) (SlotNormal _:ss) rs ls = f vs ss (v:rs) ls
                f (v:vs) (SlotUnpacked e n es:ss) rs ls = do
                    let g t = do
                            s <- newUniq
                            return $ tVr (anonymous s) t
                    as <- mapM g (map sub es)
                    f vs ss (reverse as ++ rs) ((v,ELit litCons { litName = n, litArgs = map EVar as, litType = sub e }):ls)
                f [] [] rs ls = return $ Alt (litCons { litName = name, litArgs = reverse rs, litType = typ }) (eLetRec ls e)
                f _ _ _ _ = error "DataConstructors.deconstructuonExpression.f"
            f vs (conOrigSlots mc) [] []
deconstructionExpression wdt n ty vs e | Just fa <- followAlias wdt ty  = deconstructionExpression wdt n fa vs e
deconstructionExpression _ n e _ _ = error $ "deconstructionExpression: error in " ++ show n ++ ": " ++ show e

slotTypes ::
    DataTable -- ^ table of data constructors
    -> Name   -- ^ name of constructor
    -> E      -- ^ type of value
    -> [E]    -- ^ type of each slot
slotTypes wdt n (ELit LitCons { litName = pn, litArgs = xs, litType = _ })
    | pn == conName pc = [sub x | x <- conSlots mc ]
    where
    Identity mc = getConstructor n wdt
    Identity pc = getConstructor (conInhabits mc) wdt
    sub = substMap $ fromDistinctAscList [ (i,sl) | sl <- xs | i <- anonymousIds ]
slotTypes wdt n kind
    | sortKindLike kind, (e,ts) <- fromPi kind = take (length (conSlots mc) - length ts) (conSlots mc)
    -- | sortKindLike kind, (e,ts) <- fromPi kind = (conSlots mc)
    where Identity mc = getConstructor n wdt
slotTypes wdt n e | Just fa <- followAlias wdt e  = slotTypes wdt n fa
slotTypes _ n e = error $ "slotTypes: error in " ++ show n ++ ": " ++ show e

slotTypesHs ::
    DataTable -- ^ table of data constructors
    -> Name   -- ^ name of constructor
    -> E      -- ^ type of value
    -> [E]    -- ^ type of each slot
slotTypesHs wdt n (ELit LitCons { litName = pn, litArgs = xs, litType = _ })
    | pn == conName pc = [sub x | x <- getHsSlots $ conOrigSlots mc ]
    where
    Identity mc = getConstructor n wdt
    Identity pc = getConstructor (conInhabits mc) wdt
    sub = substMap $ fromDistinctAscList [ (i,sl) | sl <- xs | i <- anonymousIds ]
slotTypesHs wdt n kind
    | sortKindLike kind, (e,ts) <- fromPi kind = take (length (conSlots mc) - length ts) (conSlots mc)
    where Identity mc = getConstructor n wdt
slotTypesHs wdt n e | Just fa <- followAlias wdt e  = slotTypes wdt n fa
slotTypesHs _ n e = error $ "slotTypes: error in " ++ show n ++ ": " ++ show e

{-# NOINLINE showDataTable #-}
showDataTable (DataTable mp) = vcat xs where
    c  const = vcat [t,e,cs,al,vt,ih,ch] where
        t  = text "::" <+> ePretty conType
        e  = text "=" <+> ePretty conExpr
        cs = text "slots:" <+> tupled (map ePretty (conSlots const))
        al = text "alias:" <+> tshow conAlias
        vt = text "virtual:" <+> tshow conVirtual
        ih = text "inhabits:" <+> tshow conInhabits
        ch = text "children:" <+> tshow conChildren
        Constructor {
            conType = conType,
            conExpr = conExpr,
            conAlias = conAlias,
            conVirtual = conVirtual,
            conInhabits = conInhabits,
            conChildren = conChildren
            } = const
    xs = [text x <+> hang 0 (c y) | (x,y) <- ds ]
    ds = sortBy (\(x,_) (y,_) -> compare x y) [(show x,y)  | (x,y) <-  Map.toList mp]

{-# NOINLINE samplePrimitiveDataTable #-}
samplePrimitiveDataTable :: DataTable
samplePrimitiveDataTable = DataTable $ Map.fromList [ (x,c) | x <- xs, c <- getConstructor x mempty] where
    nt v = map (flip unboxedNameTuple (v::Int)) [DataConstructor, TypeConstructor]
    xs = nt 0 ++ nt 3 ++ [nameConjured modAbsurd eStar,nameConjured modBox hs, nameConjured modAbsurd hs', nameConjured modBox hs',rt_bits16,rt_bits_ptr_]
    hs = EPi (tVr emptyId eHash) eStar
    hs' = tFunc eStar (tFunc (tFunc eStar eHash) eStar)

getSiblings :: DataTable -> Name -> Maybe [Name]
getSiblings dt n
    | Just c <- getConstructor n dt, Just Constructor { conChildren = DataNormal cs } <- getConstructor (conInhabits c) dt = Just cs
    | otherwise =  Nothing

numberSiblings :: DataTable -> Name -> Maybe Int
numberSiblings dt n
    | Just c <- getConstructor n dt, Just Constructor { conChildren = cc } <- getConstructor (conInhabits c) dt = case cc of
        DataNormal ds -> Just $ length ds
        DataEnum n -> Just n
        _ -> Nothing
    | otherwise =  Nothing

-- whether the type has a single slot
onlyChild :: DataTable -> Name -> Bool
onlyChild dt n = isJust ans where
    ans = do
        c <- getConstructor n dt
        case conChildren c of
            DataNormal [_] -> return ()
            _ -> do
                c <- getConstructor (conInhabits c) dt
                case conChildren c of
                    DataNormal [_] -> return ()
                    _ -> fail "not cpr"

pprintTypeOfCons :: (Monad m,DocLike a) => DataTable -> Name -> m a
pprintTypeOfCons dataTable name = do
    c <- getConstructor name dataTable
    return $ pprintTypeAsHs (conType c)

pprintTypeAsHs :: DocLike a => E -> a
pprintTypeAsHs e = unparse $ runVarName (f e) where
    f e | e == eStar = return $ atom $ text "*"
        | e == eHash = return $ atom $ text "#"
    f (EPi (TVr { tvrIdent = eid, tvrType = t1 }) t2) | eid == emptyId = do
        t1 <- f t1
        t2 <- f t2
        return $ t1 `arr` t2
    f (ELit LitCons { litName = n, litArgs = as }) | (a:as') <- reverse as = f $ EAp (ELit litCons { litName = n, litArgs = reverse as' }) a
    f (ELit LitCons { litName = n, litArgs = [] }) = return $ atom $ text $ show n
    f (EAp a b) = do
        a <- f a
        b <- f b
        return $ a `app` b
    f (EVar v) = do
        vo <- newLookupName ['a' .. ] () (tvrIdent v)
        return $ atom $ char vo
    f v | (e,ts@(_:_)) <- fromPi v = do
        ts' <- mapM (newLookupName ['a'..] () . tvrIdent) ts
        r <- f e
        return $ fixitize (N,-3) $ pop (text "forall" <+> hsep (map char ts') <+> text ". ")  (atomize r)
    f e = error $ "printTypeAsHs: " ++ show e
    arr = bop (R,0) (space <> text "->" <> space)
    app = bop (L,100) (text " ")

class Monad m => DataTableMonad m where
    getDataTable :: m DataTable
    getDataTable = return mempty

instance DataTableMonad Identity

-- | list of declared data types that map
-- directly to primitive real types

primitiveAliases :: Map.Map Name Name
primitiveAliases = Map.fromList [
    (tc_Bits1, rt_bool),
    (tc_Bits8, rt_bits8),
    (tc_Bits16, rt_bits16),
    (tc_Bits32, rt_bits32),
    (tc_Bits64, rt_bits64),
    (tc_Bits128, rt_bits128),
    (tc_BitsPtr, rt_bits_ptr_),
    (tc_BitsMax, rt_bits_max_),
    (tc_BitsInt, rt_bits_int_),
    (tc_BitsWchar, rt_bits_wchar_t_),
    (tc_BitsSize, rt_bits_size_t_),

    (tc_Float32, rt_float32),
    (tc_Float64, rt_float64),
    (tc_Float80, rt_float80),
    (tc_Float128, rt_float128)
    ]

-- mapping of primitive types to the C calling convention used
-- when passing to/from foreign functions

rawExtTypeMap :: Map.Map Name ExtType
rawExtTypeMap = Map.fromList [
    (rt_bool,  "bool"),
    (rt_bits8,  "uint8_t"),
    (rt_bits16, "uint16_t"),
    (rt_bits32, "uint32_t"),
    (rt_bits64, "uint64_t"),
    (rt_bits128,"uint128_t"),
    (rt_bits_ptr_, "uintptr_t" ),
    (rt_bits_max_, "uintmax_t"),
    (rt_bits_size_t_, "size_t" ),
    (rt_bits_time_t_, "time_t" ),
    (rt_bits_wchar_t_, "wchar_t" ),
    (rt_bits_int_, "int" ),
    (rt_bits_short_, "short" ),

    (rt_float32, "float"),
    (rt_float64, "double"),
    (rt_float80, "long double"),
    (rt_float128, "__float128")
    ]

-- which C types these convert to in FFI specifications for
-- figuring out calling conventions. not necessarily related
-- to the representation.
--
-- ideally, these could be set via a pragma

typeTable :: Map.Map Name ExtType
typeTable = Map.fromList [
    (tc_Char,"wchar_t"),
    (tc_Char_,"wchar_t"),
    (tc_Int, "int"),
    (tc_Int8, "int8_t"),
    (tc_Int16, "int16_t"),
    (tc_Int32, "int32_t"),
    (tc_Int64, "int64_t"),
    (tc_IntMax, "intmax_t"),
    (tc_IntPtr, "intptr_t"),
    (tc_Word, "unsigned"),
    (tc_Word8, "uint8_t"),
    (tc_Word16, "uint16_t"),
    (tc_Word32, "uint32_t"),
    (tc_Word64, "uint64_t"),
    (tc_WordMax, "uintmax_t"),
    (tc_WordPtr, "uintptr_t"),
    (tc_Float, "float"),
    (tc_Double, "double"),
    (tc_Ptr, "HsPtr"),
    (tc_FunPtr, "HsFunPtr"),
    (tc_BitsSize, "size_t"),
    (tc_BitsWchar, "wchar_t"),
    (tc_BitsInt, "Int"),

    (tc_Addr_, "HsPtr"),
    (tc_FunAddr_, "HsFunPtr"),

    (tc_CChar, "char"),
    (tc_CShort, "short"),
    (tc_CInt, "int"),
    (tc_CLong, "long"),

    (tc_CSChar, "signed char"),

    (tc_CUChar, "unsigned char"),
    (tc_CUShort, "unsigned short"),
    (tc_CUInt, "unsigned int"),
    (tc_CULong, "unsigned long"),
    (tc_Bool, "HsBool"),

    (tc_CWchar, "wchar_t"),
    (tc_CWint, "wint_t"),
    (tc_CTime, "time_t"),
    (tc_CSize, "size_t"),
    (tc_Unit,  "void"),
    (tc_State_,  "void")
    ]
