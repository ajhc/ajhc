module DataConstructors(
    Constructor(..),
    DataTable(..),
    DataTableMonad(..),
    AliasType(..),
    dataTablePrims,
    constructionExpression,
    deconstructionExpression,
    followAliases,
    removeNewtypes,
    getConstructor,
    getConstructorArities,
    getProduct,
    getSiblings,
    lookupCType',
    lookupCType,
    pprintTypeOfCons,
    showDataTable,
    slotTypes,
    toDataTable,
    updateLit,
    deriveClasses,
    typesCompatable
    ) where

import Control.Monad.Identity
import Control.Monad.Writer(tell,execWriter)
import Data.Monoid hiding(getProduct)
import List(sortBy)
import qualified Data.Map as Map hiding(map)

import Binary
import C.Prims
import FrontEnd.Class(instanceName)
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.E
import E.Show
import E.Subst
import E.TypeCheck
import E.Traverse
import E.Values
import FrontEnd.Syn.Traverse
import GenUtil
import HsSyn
import Info.Types
import MapBinaryInstance()
import Name.Id
import Name.Name as Name
import Name.Names
import Name.VConsts
import PrimitiveOperators
import FrontEnd.Tc.Type
import FrontEnd.Tc.Kind
import Support.CanType
import Support.FreeVars
import Support.Unparse
import Util.HasSize
import Util.SameShape
import Util.SetLike as S
import Util.VarName
import qualified Util.Graph as G
import qualified Util.Seq as Seq

tipe' (TAp t1 t2) = liftM2 eAp (tipe' t1) (tipe' t2)
tipe' (TArrow t1 t2) =  do
    t1' <- tipe' t1
    t2' <- tipe' t2
    return $ EPi (tVr 0 (t1')) t2'
tipe' (TCon (Tycon n k)) =  return $ ELit litCons { litName = n, litType = kind k }
tipe' (TVar tv@Tyvar { tyvarKind = k}) = do
    v <- lookupName tv
    return $ EVar $ tVr v (kind k)
tipe' (TForAll [] (_ :=> t)) = tipe' t
tipe' (TExists [] (_ :=> t)) = tipe' t
tipe' (TForAll xs (_ :=> t)) = do
    xs' <- flip mapM xs $ \tv -> do
        v <- newName [70,72..] () tv
        return $ tVr v (kind $ tyvarKind tv)
    t' <- tipe' t
    return $ foldr EPi t' xs' -- [ tVr n (kind k) | n <- [2,4..] | k <- xs ]
tipe' (TExists xs (_ :=> t)) = do
    xs' <- flip mapM xs $ \tv -> do
        --v <- newName [70,72..] () tv
        --return $ tVr v (kind $ tyvarKind tv)
        return $ (kind $ tyvarKind tv)
    t' <- tipe' t
    return $ ELit litCons { litName = unboxedNameTuple TypeConstructor (length xs' + 1), litArgs = (t':xs'), litType = eHash }



kind (KBase KUTuple) = eHash
kind (KBase KHash) = eHash
kind (KBase Star) = eStar
kind (Kfun k1 k2) = EPi (tVr 0 (kind k1)) (kind k2)
kind (KVar _) = error "Kind variable still existing."


data AliasType = NotAlias | ErasedAlias | RecursiveAlias
    deriving(Eq,Ord,Show)
    {-! derive: GhcBinary !-}

-- | Record describing a data type.
-- * is also a data type containing the type constructors, which are unlifted, yet boxed.

data Constructor = Constructor {
    conName     :: Name,         -- name of constructor
    conType     :: E,            -- type of constructor
    conExpr     :: E,            -- expression which constructs this value
    conSlots    :: [E],          -- slots
    conDeriving :: [Name],       -- classes this type derives
    conAlias    :: AliasType,         -- whether this is a simple alias and has no tag of its own.
    conInhabits :: Name,         -- what constructor it inhabits, similar to conType, but not quite.
    conVirtual  :: Maybe [Name], -- whether this is a virtual constructor that translates into an enum and its siblings
    conChildren :: Maybe [Name]  -- if nothing, then type is abstract
    } deriving(Show)
    {-! derive: GhcBinary !-}

newtype DataTable = DataTable {
    constructorMap :: (Map.Map Name Constructor)
    }
    {-! derive: GhcBinary, Monoid !-}

instance HasSize DataTable where
    size (DataTable d) = Map.size d

getConstructor :: Monad m => Name -> DataTable -> m Constructor
getConstructor n _ | Just v <- fromUnboxedNameTuple n, DataConstructor <- nameType n = return $ snd $ tunboxedtuple v
getConstructor n _ | Just v <- fromUnboxedNameTuple n, TypeConstructor <- nameType n = return $ fst $ tunboxedtuple v
getConstructor n (DataTable map) = case Map.lookup n map of
    Just x -> return x
    Nothing -> fail $ "getConstructor: " ++ show (nameType n,n)

-- | return the single constructor of product types

getProduct :: Monad m => DataTable -> E -> m Constructor
getProduct dataTable e | (ELit LitCons { litName = cn }) <- followAliases dataTable e, Just c <- getConstructor cn dataTable = f c where
    f c | Just [x] <- conChildren c = getConstructor x dataTable
        | otherwise = fail "Not Product type"
getProduct _ _ = fail "Not Product type"


tunboxedtuple :: Int -> (Constructor,Constructor)
tunboxedtuple n = (typeCons,dataCons) where
        dataCons = Constructor {
            conName = dc,
            conType = dtipe,
            conSlots = map EVar typeVars,
            conDeriving = [],
            conExpr =  foldr ($) (ELit litCons { litName = dc, litArgs = map EVar vars, litType = ftipe }) (map ELam vars),
            conAlias = NotAlias,
            conInhabits = tc,
            conVirtual = Nothing,
            conChildren = Nothing
           }
        typeCons = Constructor {
            conName = tc,
            conType = foldr EPi eHash (replicate n tvr { tvrType = eStar }),
            conSlots = replicate n eStar,
            conDeriving = [],
            conExpr = tipe,
            conAlias = NotAlias,
            conVirtual = Nothing,
            conInhabits = tHash,
            conChildren = Just [dc]
           }

        dc = unboxedNameTuple DataConstructor n
        tc = unboxedNameTuple TypeConstructor n
        tipe = foldr ELam ftipe typeVars
        typeVars = take n [ tvr { tvrType = eStar, tvrIdent = v } | v <- [ 2,4 ..]]
        vars =  [ tvr { tvrType = EVar t, tvrIdent = v } | v <- [ 2*n + 16, 2*n + 18 ..] | t <- typeVars ]
        ftipe = ELit (litCons { litName = tc, litArgs = map EVar typeVars, litType = eHash })
        dtipe = foldr EPi (foldr EPi ftipe [ v { tvrIdent = 0 } | v <- vars]) typeVars

tabsurd = Constructor {
            conName = tc_Absurd,
            conType = eStar,
            conSlots = [],
            conDeriving = [],
            conExpr = tAbsurd eStar,
            conAlias = NotAlias,
            conVirtual = Nothing,
            conInhabits = tStar,
            conChildren = Nothing
    }

-- | Jhc@.Box can hold any boxed value (something whose type inhabits *, or is a function)
-- so, there is a bit of subtyping that goes on.

tbox = Constructor {
            conName = tc_Box,
            conType = eStar,
            conSlots = [],
            conDeriving = [],
            conExpr = tBox,
            conAlias = NotAlias,
            conVirtual = Nothing,
            conInhabits = tStar,
            conChildren = Nothing
    }

worlds = []


tarrow = Constructor {
            conName = tc_Arrow,
            conType = EPi (tVr 0 eStar) (EPi (tVr 0 eStar) eStar),
            conSlots = [eStar,eStar],
            conDeriving = [],
            conExpr = ELam (tVr 2 eStar) (ELam (tVr 4 eStar) (EPi (tVr 0 (EVar $ tVr 2 eStar)) (EVar $ tVr 4 eStar))),
            conAlias = NotAlias,
            conInhabits = tStar,
            conVirtual = Nothing,
            conChildren = Nothing
        }


primitiveTable = concatMap f allCTypes ++ map g (snub $ map ( \ (_,_,_,b,_) -> b) allCTypes) where
    g n = Constructor {
        conName = rn,
        conType = eHash,
        conSlots = [],
        conDeriving = [],
        conExpr = ELit (litCons { litName = rn, litArgs = [], litType = eHash }),
        conAlias = NotAlias,
        conVirtual = Nothing,
        conInhabits = tHash,
        conChildren = Nothing
       } where rn = toName RawType n
    f (dc,tc,rt,y,z) | z /= "void" = [typeCons,dataCons] where
        dataCons = Constructor {
            conName = dc,
            conType = tipe,
            conSlots = [rt],
            conDeriving = [],
            conExpr = ELam (tVr 2 rt) (ELit (litCons { litName = dc, litArgs = [EVar (tVr 2 rt)], litType = tipe })),
            conAlias = NotAlias,
            conVirtual = Nothing,
            conInhabits = tc,
            conChildren = Nothing
           }
        typeCons = Constructor {
            conName = tc,
            conType = eStar,
            conSlots = [],
            conDeriving = [],
            conExpr = tipe,
            conAlias = NotAlias,
            conVirtual = Nothing,
            conInhabits = tStar,
            conChildren = Just [dc]
           }

        tipe = ELit (litCons { litName = tc, litArgs = [], litType = eStar })
    f _ = []

isAbsurd (ELit LitCons { litName = n, litArgs = [], litType = _ }) | n == tc_Absurd = True
isAbsurd (ELit LitCons { litArgs = xs@(_:_) }) = all isAbsurd xs
isAbsurd _ = False


typesCompatable :: forall m . Monad m => DataTable -> E -> E -> m ()
typesCompatable dataTable a b = f (-2 :: Id) a b where
        f :: Id -> E -> E -> m ()
        f _ (ESort a) (ESort b) = when (a /= b) $ fail $ "Sorts don't match: " ++ pprint (ESort a,ESort b)
        f _ (EVar a) (EVar b) = when (a /= b) $ fail $ "Vars don't match: " ++ pprint (a,b)
        f c (ELit LitCons { litName = n, litArgs = xs, litType = t }) (ELit LitCons { litName = n', litArgs = xs', litType = t' }) | n == n' = do
            f c t t'
            when (not $ sameShape1 xs xs') $ fail "Arg lists don't match"
            zipWithM_ (f c) xs xs'
        f c (EAp a b) (EAp a' b') = do
            f c a a'
            f c b b'
        f c (ELam va ea) (ELam vb eb) = lam va ea vb eb c
        f c (EPi va ea) (EPi vb eb)   = lam va ea vb eb c
        f c (EPi (TVr { tvrIdent = 0, tvrType =  a}) b) (ELit (LitCons { litName = n, litArgs = [a',b'], litType = t })) | conName tarrow == n, t == eStar = do
            f c a a'
            f c b b'
        f c (ELit (LitCons { litName = n, litArgs = [a',b'], litType = t })) (EPi (TVr { tvrIdent = 0, tvrType =  a}) b)  | conName tarrow == n, t == eStar = do
            f c a a'
            f c b b'
        f c (ELit (LitCons {  litAliasFor = Just af, litArgs = as })) b = do
            f c (foldl eAp af as) b
        f c a (ELit (LitCons {  litAliasFor = Just af, litArgs = as })) = do
            f c a (foldl eAp af as)
        f _ a b = fail $ "Types don't match:" ++ pprint (a,b)

        lam :: TVr -> E -> TVr -> E -> Id -> m ()
        lam va ea vb eb c = do
            f c (tvrType va) (tvrType vb)
            f (c - 2) (subst va (EVar va { tvrIdent = c }) ea) (subst vb (EVar vb { tvrIdent = c }) eb)




lookupCType dataTable e = case followAliases (mappend dataTablePrims dataTable) e of
    ELit LitCons { litName = c, litArgs = [], litType = _ }
        | c == tc_Unit -> return (c,"void")
        | c == tc_World__ -> return (c,"void")
        | Just pt <- Map.lookup c ctypeMap -> return (c,pt)

    e' -> fail $ "lookupCType: " ++ show (e,e')

lookupCType' dataTable e = case followAliases (mappend dataTablePrims dataTable) e of
    ELit LitCons { litName = c, litArgs = [], litType = _ }
        | Just Constructor { conChildren = Just [cn] }  <- getConstructor c dataTable,
          Just Constructor { conSlots = [st@(ELit LitCons { litName = n, litArgs = [], litType = _ })] } <- getConstructor cn dataTable
            -> return (cn,st,show n)
    ELit LitCons { litName = c, litArgs = [], litType = _ } | Just cn  <- getConstructor c dataTable -> fail $ "lookupCType: " ++ show cn
    e' -> fail $ "lookupCType': " ++ show (e,e')


followAlias :: Monad m => DataTable -> E -> m E
followAlias _ (ELit LitCons { litAliasFor = Just af, litArgs = as }) = return (foldl eAp af as)
followAlias _  _ = fail "followAlias: not alias"

followAliases :: DataTable -> E -> E
followAliases _dataTable e = f e where
    f (ELit LitCons { litAliasFor = Just af, litArgs = as }) = f (foldl eAp af as)
    f e = e

dataTablePrims = DataTable $ Map.fromList ([ (conName x,x) | x <- tbox:tabsurd:tarrow:primitiveTable ] ++ worlds)

deriveClasses :: DataTable -> [(TVr,E)]
deriveClasses (DataTable mp) = concatMap f (Map.elems mp) where
    f c | TypeConstructor == nameType (conName c), Just is <- conVirtual c = concatMap (g is c) (conDeriving c)
    f _ = []
    g is c cl = h cl where
        typ = conExpr c
        Just [con] = conChildren c
        v1 = tvr { tvrIdent = 2,  tvrType = typ }
        v2 = tvr { tvrIdent = 4,  tvrType = typ }
        i1 = tvr { tvrIdent = 6,  tvrType = tIntzh }
        i2 = tvr { tvrIdent = 8,  tvrType = tIntzh }
        i3 = tvr { tvrIdent = 10, tvrType = tIntzh }
        int1 = tvr { tvrIdent = 12, tvrType = tInt }
        val1 = tvr { tvrIdent = 14, tvrType = typ }
        unbox e = ELam v1 (ELam v2 (ec (EVar v1) i1 (ec (EVar v2) i2 e)))  where
            ec v i e = eCase v [Alt (litCons { litName = con, litArgs = [i], litType = typ }) e] Unknown
        h cl | cl == class_Eq = [mkCmpFunc (func_equals sFuncNames) "=="]
        h cl | cl == class_Ord = [
                mkCmpFunc (func_geq sFuncNames) ">=",
                mkCmpFunc (func_leq sFuncNames) "<=",
                mkCmpFunc (func_lt sFuncNames) "<",
                mkCmpFunc (func_gt sFuncNames) ">"]
        h cl | cl == class_Enum = [{- (iv_te,ib_te), -}(iv_fe,ib_fe)] where
            iv_te = setProperty prop_INSTANCE tvr { tvrIdent = toId $ instanceName (func_toEnum sFuncNames) (nameName $ conName c), tvrType = getType ib_te }
            iv_fe = setProperty prop_INSTANCE tvr { tvrIdent = toId $ instanceName (func_fromEnum sFuncNames) (nameName $ conName c), tvrType = getType ib_fe }
            ib_te = ELam int1 (ec tInt dc_Int (EVar int1) i1 (ELit (litCons { litName = con, litArgs = [EVar i1], litType = typ })))
            ib_fe = ELam val1 (ec typ con (EVar val1) i1 (ELit (litCons { litName = dc_Int, litArgs = [EVar i1], litType = tInt })))
            ec typ con v i e = eCase v [Alt (litCons { litName = con, litArgs = [i], litType = typ }) e] Unknown

        h _ = []
        mkCmpFunc fname op = (iv_eq,ib_eq) where
            ib_eq = unbox (eStrictLet i3 (oper_III op (EVar i1) (EVar i2)) (ELit (litCons { litName = dc_Boolzh, litArgs = [EVar i3], litType = tBool })))
            iv_eq = setProperty prop_INSTANCE tvr { tvrIdent = toId $ instanceName fname (nameName $ conName c), tvrType = getType ib_eq }
    oper_III op a b = EPrim (APrim (Operator op ["int","int"] "int") mempty) [a,b] tIntzh



updateLit :: DataTable -> Lit e t -> Lit e t
updateLit _ l@LitInt {} = l
updateLit dataTable lc@LitCons { litName = n } =  lc { litAliasFor = af } where
    af = do
        Constructor { conChildren = Just [x], conSlots = cs } <- getConstructor n dataTable
        Constructor { conAlias = ErasedAlias, conSlots = [sl] } <- getConstructor x dataTable
        return (foldr ELam sl [ tVr i s | s <- cs | i <- [2,4..]])

removeNewtypes :: DataTable -> E -> E
removeNewtypes dataTable e = runIdentity (f e) where
    f ec@ECase {} = emapEGH f f return ec { eCaseAlts = map g (eCaseAlts ec) } where
        g (Alt l e) = Alt (gl $ updateLit dataTable l) e
    f (ELit l) = emapEGH f f return (ELit (gl $ updateLit dataTable l))
    f e = emapEGH f f return e
    gl lc@LitCons { litAliasFor = Just e }  = lc { litAliasFor = Just $ removeNewtypes dataTable e }
    gl l = l


{-# NOINLINE toDataTable #-}
toDataTable :: (Map.Map Name Kind) -> (Map.Map Name Type) -> [HsDecl] -> DataTable -> DataTable
toDataTable km cm ds currentDataTable = newDataTable  where
    newDataTable = DataTable (Map.mapWithKey fixupMap $ Map.fromList [ (conName x,procNewTypes x) | x <- ds' ])
    procNewTypes c = c { conExpr = f (conExpr c), conType = f (conType c), conSlots = map f (conSlots c) } where
        f = removeNewtypes (newDataTable `mappend` currentDataTable)
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
            f (n,vc) = vc { conExpr = ELit (litCons { litName = consName, litArgs = [ELit (LitInt (fromIntegral n) tIntzh)], litType = conType vc }), conVirtual = virt }
            virtualCons = map f (zip [(0 :: Int) ..] virtualCons')
            consName =  mapName (id,(++ "#")) $ toName DataConstructor (nameName (conName theType))
            dataCons = fc { conName = consName, conType = getType (conExpr dataCons), conSlots = [tIntzh], conExpr = ELam (tVr 12 tIntzh) (ELit (litCons { litName = consName, litArgs = [EVar (tVr 12 tIntzh)], litType =  conExpr theType })) }
        tell (Seq.fromList virtualCons)
        tell (Seq.singleton dataCons)
        tell $ Seq.singleton theType { conChildren = Just [consName], conVirtual = virt }
        return ()

    dt decl alias cs = do
        let dataCons = map (makeData alias typeInfo) cs
            typeInfo@(theType,_,_) = makeType decl
        tell (Seq.fromList dataCons)
        tell $ Seq.singleton theType { conChildren = Just (map conName dataCons) }
    makeData alias (theType,theTypeArgs,theTypeExpr) x = theData where
        theData = Constructor {
            conName = dataConsName,
            conType =foldr ($) (getType theExpr) (map EPi theTypeArgs),
            conSlots =  slots,
            conExpr = theExpr,
            conInhabits = conName theType,
            conDeriving = [],
            conVirtual = Nothing,
            conAlias = alias,
            conChildren = Nothing
            }
        theExpr =  foldr ($) (strictize $ ELit litCons { litName = dataConsName, litArgs = map EVar vars, litType = theTypeExpr }) (map ELam vars)
        slots = map (subst . tvrType) ts -- XXX TODO fix this mapping
        vars = [ tvr { tvrType = t } | tvr <- ts | t <- slots ]
        strictize con = E.Subst.subst tvr { tvrIdent = -1 } Unknown $ f (zip (map isHsBangedTy args) vars) con where
            f ((False,_):rs) con = f rs con
            f ((True,var):rs) con = eStrictLet var (EVar var) con
            f [] con = con
        dataConsName =  toName Name.DataConstructor (hsConDeclName x)
        args = hsConDeclArgs x
        (ELit LitCons { litName = _, litArgs = xs, litType = _ } ,ts') = fromPi $ runVarName $ do
            flip mapM_ vs $ \tv -> do
                newName [2,4..] () tv
            tipe' ty
        existentials = melems $ freeVars (map getType ts') S.\\ (freeVars xs :: IdMap TVr)
        subst = substMap $ fromList [ (tvrIdent tv ,EVar $ tv { tvrIdent = p }) | EVar tv <- xs | p <- [2,4..] ]
        ts = existentials ++ [ tvr {tvrIdent = x} | tvr <- ts' | x <- drop (5 + length theTypeArgs) [2,4..] ]
        (vs,ty) = case Map.lookup dataConsName cm of
            Just (TForAll vs (_ :=> ty)) -> (vs,ty)
            Just ty -> ([],ty)
    makeType decl = (theType,theTypeArgs,theTypeExpr) where
        theTypeName = toName Name.TypeConstructor (hsDeclName decl)
        theKind = kind $ runIdentity (Map.lookup theTypeName km)
        (theTypeFKind,theTypeKArgs') = fromPi theKind
        theTypeArgs = [ tvr { tvrIdent = x } | tvr  <- theTypeKArgs' | x <- [2,4..] ]
        theTypeExpr = ELit litCons { litName = theTypeName, litArgs = map EVar theTypeArgs, litType = theTypeFKind }
        theType = Constructor {
            conName = theTypeName,
            conType = theKind,
            conSlots = map tvrType theTypeArgs,
            conExpr = foldr ($) theTypeExpr (map ELam theTypeArgs),
            conDeriving = [ toName ClassName n | n <- hsDeclDerives decl],
            conAlias = NotAlias,
            conInhabits = if theTypeFKind == eStar then tStar else tHash,
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
    var = tvr { tvrIdent = 2, tvrType = typ }
    Just mc = getConstructor n dataTable
    Just pc = getConstructor (conInhabits mc) dataTable
    sub = substMap $ fromDistinctAscList [ (i,sl) | sl <- xs | i <- [2,4..] ]
constructionExpression wdt n e | Just fa <- followAlias wdt e  = constructionExpression wdt n fa
constructionExpression _ n e = error $ "constructionExpression: error in " ++ show n ++ ": " ++ show e

deconstructionExpression ::
    DataTable -- ^ table of data constructors
    -> Name   -- ^ name of said constructor
    -> E      -- ^ type of pattern
    -> [TVr]  -- ^ variables to be bound
    -> [TVr]  -- ^ name supply, types ignored, must be at least as many as bound variables exist
    -> E      -- ^ body of alt
    -> Alt E  -- ^ resulting alternative
deconstructionExpression dataTable name typ@(ELit LitCons { litName = pn, litArgs = xs, litType = _ }) vs _vs' e | pn == conName pc = ans where
    Just mc = getConstructor name dataTable
    Just pc = getConstructor (conInhabits mc) dataTable
    ans = case conVirtual mc of
        Nothing -> Alt (litCons { litName = name, litArgs = vs, litType = typ }) e
        Just _ -> let ELit LitCons {  litArgs = [ELit (LitInt n t)] } = conExpr mc in Alt (LitInt n t) e
deconstructionExpression wdt n ty vs vs' e | Just fa <- followAlias wdt ty  = deconstructionExpression wdt n fa vs vs' e
deconstructionExpression _ n e _ _ _ = error $ "deconstructionExpression: error in " ++ show n ++ ": " ++ show e

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
    sub = substMap $ fromDistinctAscList [ (i,sl) | sl <- xs | i <- [2,4..] ]
slotTypes wdt n kind
    | sortStarLike kind, (e,ts) <- fromPi kind = drop (length ts) (conSlots mc)
    where Identity mc = getConstructor n wdt
slotTypes wdt n e | Just fa <- followAlias wdt e  = slotTypes wdt n fa
slotTypes _ n e = error $ "slotTypes: error in " ++ show n ++ ": " ++ show e

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
    (ubt,ubd) = tunboxedtuple 3
    ds = sortBy (\(x,_) (y,_) -> compare x y) [ (show x,y)  | (x,y) <-  Map.toList mp ++ [(conName ubt,ubt),(conName ubd,ubd)]]


getSiblings :: DataTable -> Name -> Maybe [Name]
getSiblings dt n
    | Just c <- getConstructor n dt, Just s <- getConstructor (conInhabits c) dt = conChildren s
    | otherwise =  Nothing


pprintTypeOfCons :: (Monad m,DocLike a) => DataTable -> Name -> m a
pprintTypeOfCons dataTable name = do
    c <- getConstructor name dataTable
    return $ pprintTypeAsHs (conType c)




pprintTypeAsHs :: DocLike a => E -> a
pprintTypeAsHs e = unparse $ runVarName (f e) where
    f e | e == eStar = return $ atom $ text "*"
        | e == eHash = return $ atom $ text "#"
    f (EPi (TVr { tvrIdent = 0, tvrType = t1 }) t2) = do
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



