module DataConstructors(
    Constructor(..),
    DataTable(..),
    DataTableMonad(..),
    dataTablePrims,
    constructionExpression,
    deconstructionExpression,
    followAliases,
    followAlias,
    expandAliases,
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
    deriveClasses,
    typesCompatable
    ) where

import Control.Monad.Identity
import Control.Monad.Writer
import qualified Data.Map as Map hiding(map)
import List(sortBy)

import Binary
import C.Prims
import Class(instanceName)
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.E
import E.Shadow
import E.Show
import E.Subst
import E.TypeCheck
import E.Values
import Info.Types
import GenUtil
import HsSyn
import MapBinaryInstance()
import Name.Id
import Name.Name as Name
import Name.Names
import Name.VConsts
import PrimitiveOperators
import Representation
import Support.CanType
import Support.FreeVars
import Support.Unparse
import Util.HasSize
import Util.SameShape
import Util.SetLike as S
import Util.VarName
import qualified Util.Seq as Seq

tipe t = runVarName (tipe' t)
tipe' (TAp t1 t2) = liftM2 eAp (tipe' t1) (tipe' t2)
tipe' (TArrow t1 t2) =  do
    t1' <- tipe' t1
    t2' <- tipe' t2
    return $ EPi (tVr 0 (t1')) t2'
tipe' (TCon (Tycon n k)) | n == tc_World__ = return $ ELit (LitCons rt_Worldzh [] eHash)
tipe' (TCon (Tycon n k)) =  return $ ELit (LitCons n [] (kind k))
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
    return $ ELit (LitCons (unboxedNameTuple TypeConstructor (length xs' + 1)) (t':xs') eHash)

--tipe (TForAll (Forall xs (_ :=> t))) = foldr EPi (tipe t) [ tVr n (kind k) | n <- [2,4..] | k <- xs ]


kind Star = eStar
kind (Kfun k1 k2) = EPi (tVr 0 (kind k1)) (kind k2)
kind (KVar _) = error "Kind variable still existing."

-- | Record describing a data type.
-- * is also a data type containing the type constructors, which are unlifted, yet boxed.

data Constructor = Constructor {
    conName     :: Name,         -- name of constructor
    conType     :: E,            -- type of constructor
    conExpr     :: E,            -- expression which constructs this value
    conSlots    :: [E],          -- slots
    conDeriving :: [Name],       -- classes this type derives
    conAlias    :: Bool,         -- whether this is a simple alias and has no tag of its own.
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
getProduct dataTable e | (ELit (LitCons cn _ _)) <- followAliases dataTable e, Just c <- getConstructor cn dataTable = f c where
    f c | Just [x] <- conChildren c = getConstructor x dataTable
        | otherwise = fail "Not Product type"
getProduct _ _ = fail "Not Product type"


tunboxedtuple n = (typeCons,dataCons) where
        dataCons = Constructor {
            conName = dc,
            conType = tipe,
            conSlots = replicate n Unknown,
            conDeriving = [],
            conExpr = Unknown, -- error "expr" ELam (tVr 2 rt) (ELit (LitCons dc [EVar (tVr 2 rt)] tipe)),
            conAlias = False,
            conInhabits = tc,
            conVirtual = Nothing,
            conChildren = Nothing
           }
        typeCons = Constructor {
            conName = tc,
            conType = eHash,
            conSlots = replicate n Unknown,
            conDeriving = [],
            conExpr = tipe,
            conAlias = False,
            conVirtual = Nothing,
            conInhabits = tHash,
            conChildren = Just [dc]
           }

        dc = unboxedNameTuple DataConstructor n
        tc = unboxedNameTuple TypeConstructor n
        tipe = ELit (LitCons tc [] eHash)


tabsurd = Constructor {
            conName = tc_Absurd,
            conType = eStar,
            conSlots = [],
            conDeriving = [],
            conExpr = tAbsurd eStar,
            conAlias = False,
            conVirtual = Nothing,
            conInhabits = tStar,
            conChildren = Nothing
    }

worlds = [(rt_Worldzh,tWorld__),(tc_World__,tWorld__)] where
    tWorld__ = Constructor {
                conName = rt_Worldzh,
                conType = eHash,
                conSlots = [],
                conDeriving = [],
                conExpr = ELit (LitCons rt_Worldzh [] eHash),
                conAlias = False,
                conVirtual = Nothing,
                conInhabits = tHash,
                conChildren = Nothing
        }


tarrow = Constructor {
            conName = tc_Arrow,
            conType = EPi (tVr 0 eStar) (EPi (tVr 0 eStar) eStar),
            conSlots = [eStar,eStar],
            conDeriving = [],
            conExpr = ELam (tVr 2 eStar) (ELam (tVr 4 eStar) (EPi (tVr 0 (EVar $ tVr 2 eStar)) (EVar $ tVr 4 eStar))),
            conAlias = False,
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
        conExpr = ELit (LitCons rn [] eHash),
        conAlias = False,
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
            conExpr = ELam (tVr 2 rt) (ELit (LitCons dc [EVar (tVr 2 rt)] tipe)),
            conAlias = False,
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
            conAlias = False,
            conVirtual = Nothing,
            conInhabits = tStar,
            conChildren = Just [dc]
           }

        rn = toName RawType y
        tipe = ELit (LitCons tc [] eStar)
    f _ = []

isAbsurd (ELit (LitCons n [] _)) | n == tc_Absurd = True
isAbsurd (ELit (LitCons _ xs@(_:_) _)) = all isAbsurd xs
isAbsurd _ = False

-- | determine if types are the same expanding newtypes and
typesCompatable :: Monad m => DataTable -> E -> E -> m ()
typesCompatable dataTable a b = go a b where
    go :: Monad m => E -> E -> m ()
    go a b = g' [] [] a b
    g' xs ys a b = g a b where
        g (ELit (LitCons n xs t)) (ELit (LitCons n' xs' t')) | n == n' = do
            go t t'
            when (not $ sameShape1 xs xs') $ fail "Arg lists don't match"
            zipWithM_ go xs xs'
        g a b | isAbsurd a && isAbsurd b = do
            go (getType a) (getType b)
            return ()
        g (ESort a) (ESort b) = when (a /= b) $ fail $ "Sorts don't match: " ++ pprint (ESort a,ESort b)
        g (EVar a) (EVar b) = when (a /= b) $ fail $ "Vars don't match: " ++ pprint (a,b)
        g (EAp a b) (EAp a' b') = do
            go a a'
            go b b'
        g x@(EPi {}) y@(EPi {}) = do
            let EPi (TVr { tvrType =  a}) b = allShadow x
                EPi (TVr { tvrType =  a'}) b' = allShadow y
            go a a'
            go b b'
        g (EPi (TVr { tvrIdent = 0, tvrType =  a}) b) (ELit (LitCons n [a',b'] t)) | conName tarrow == n, t == eStar = do go a a'; go b b'
        g (ELit (LitCons n [a',b'] t)) (EPi (TVr { tvrIdent = 0, tvrType =  a}) b) | conName tarrow == n, t == eStar = do go a a'; go b b'
        g x@(ELam {}) y@(ELam {}) = do
            let ELam (TVr { tvrType = a}) b = allShadow x
                ELam (TVr { tvrType =  a'}) b' = allShadow y
            go a a'
            go b b'
        g a b = case f xs ys a b of
            Right () -> return ()
            Left s' -> case f ys xs b a of
                Right () -> return ()
                Left s -> fail (s ++ "\n" ++ s' ++ "\n")
    f :: Monad m => [Name] -> [Name] -> E -> E -> m ()
    f xs ys (ELit (LitCons n _ _)) _ | n `elem` xs = fail "Loop detected"
    f xs ys a@(ELit (LitCons n _ _)) b | Just x <- followAlias dataTable a = g' (n:xs) ys x b
    f _ _ a b = fail $ "Types don't match: " ++ pprint (a,b)


lookupCType dataTable e = case followAliases (mappend dataTablePrims dataTable) e of
    ELit (LitCons c [] _)
        | c == tc_Unit -> return (c,"void")
        | c == tc_World__ -> return (c,"void")
        | Just pt <- Map.lookup c ctypeMap -> return (c,pt)

    e' -> fail $ "lookupCType: " ++ show (e,e')

lookupCType' dataTable e = case followAliases (mappend dataTablePrims dataTable) e of
    ELit (LitCons c [] _)
        | Just Constructor { conChildren = Just [cn] }  <- getConstructor c dataTable,
          Just Constructor { conSlots = [st@(ELit (LitCons n [] _))] } <- getConstructor cn dataTable
            -> return (cn,st,show n)
    ELit (LitCons c [] _) | Just cn  <- getConstructor c dataTable -> fail $ "lookupCType: " ++ show cn
    e' -> fail $ "lookupCType': " ++ show (e,e')

followAlias :: Monad m => DataTable -> E -> m E
--followAlias _ e | not (sortTypeLike e) = fail "followAlias: not a type"
followAlias dataTable (EAp a b) = do
    a' <- followAlias dataTable a
    return (eAp a' b)
followAlias dataTable (ELit (LitCons c ts e)) = do
    con <- getConstructor c dataTable
    Just [cn] <- return $ conChildren con
    ccon <- getConstructor cn dataTable
    [sl] <- return $ conSlots ccon
    True <- return $ sameLength (conSlots con) ts && conAlias ccon
    return $ doSubst False False (fromList $ zip [2..] (map Just ts)) sl
followAlias _ e = fail "followAlias: not an alias"

followAliases :: DataTable -> E -> E
followAliases dataTable l = f l (10::Int) where
    f l 0 = l
    f l n = case followAlias dataTable l of
        Just e -> f e (n - 1)
        Nothing -> l

dataTablePrims = DataTable $ Map.fromList ([ (conName x,x) | x <- tabsurd:tarrow:primitiveTable ] ++ worlds)

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
            ec v i e = eCase v [Alt (LitCons con [i] typ) e] Unknown
        h cl | cl == class_Eq = [mkCmpFunc (func_equals sFuncNames) "=="]
        h cl | cl == class_Ord = [
                mkCmpFunc (func_geq sFuncNames) ">=",
                mkCmpFunc (func_leq sFuncNames) "<=",
                mkCmpFunc (func_lt sFuncNames) "<",
                mkCmpFunc (func_gt sFuncNames) ">"]
        h cl | cl == class_Enum = [{- (iv_te,ib_te), -}(iv_fe,ib_fe)] where
            iv_te = setProperty prop_INSTANCE tvr { tvrIdent = toId $ instanceName (func_toEnum sFuncNames) (nameName $ conName c), tvrType = getType ib_te }
            iv_fe = setProperty prop_INSTANCE tvr { tvrIdent = toId $ instanceName (func_fromEnum sFuncNames) (nameName $ conName c), tvrType = getType ib_fe }
            ib_te = ELam int1 (ec tInt dc_Int (EVar int1) i1 (ELit (LitCons con [EVar i1] typ)))
            ib_fe = ELam val1 (ec typ con (EVar val1) i1 (ELit (LitCons dc_Int [EVar i1] tInt)))
            ec typ con v i e = eCase v [Alt (LitCons con [i] typ) e] Unknown

        h _ = []
        mkCmpFunc fname op = (iv_eq,ib_eq) where
            ib_eq = unbox (eStrictLet i3 (oper_III op (EVar i1) (EVar i2)) (ELit (LitCons dc_Boolzh [EVar i3] tBool)))
            iv_eq = setProperty prop_INSTANCE tvr { tvrIdent = toId $ instanceName fname (nameName $ conName c), tvrType = getType ib_eq }
    oper_III op a b = EPrim (APrim (Operator op ["int","int"] "int") mempty) [a,b] tIntzh


{-# NOINLINE toDataTable #-}
toDataTable :: (Map.Map Name Kind) -> (Map.Map Name Type) -> [HsDecl] -> DataTable
toDataTable km cm ds = DataTable (Map.mapWithKey fixupMap $ Map.fromList [ (conName x,x) | x <- ds' ])  where
    fixupMap k _ | Just n <- getConstructor k dataTablePrims = n
    fixupMap _ n = n
    ds' = Seq.toList $ execWriter (mapM_ f ds)
    f decl@HsNewTypeDecl {  hsDeclCon = c } = dt decl True  [c]
    f decl@HsDataDecl {  hsDeclCons = cs } = dt decl False  cs
    f _ = return ()
    dt decl False cs@(_:_:_) | all null (map hsConDeclArgs cs) = do
        let virtualCons'@(fc:_) = map (makeData False typeInfo) cs
            typeInfo@(theType,_,_) = makeType decl
            virt = Just (map conName virtualCons')
            f (n,vc) = vc { conExpr = ELit (LitCons consName [ELit (LitInt (fromIntegral n) tIntzh)] (conType vc)), conVirtual = virt }
            virtualCons = map f (zip [(0 :: Int) ..] virtualCons')
            consName =  mapName (id,(++ "#")) $ toName DataConstructor (nameName (conName theType))
            dataCons = fc { conName = consName, conType = getType (conExpr dataCons), conSlots = [tIntzh], conExpr = ELam (tVr 12 tIntzh) (ELit (LitCons consName [EVar (tVr 12 tIntzh)] (conExpr theType))) }
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
        theExpr =  foldr ($) (strictize $ ELit (LitCons dataConsName (map EVar vars) theTypeExpr)) (map ELam vars)
        slots = map (subst . tvrType) ts -- XXX TODO fix this mapping
        vars = [ tvr { tvrType = t } | tvr <- ts | t <- slots ]
        strictize con = E.Subst.subst tvr { tvrIdent = -1 } Unknown $ f (zip (map isHsBangedTy args) vars) con where
            f ((False,_):rs) con = f rs con
            f ((True,var):rs) con = eStrictLet var (EVar var) con
            f [] con = con
        dataConsName =  toName Name.DataConstructor (hsConDeclName x)
        args = hsConDeclArgs x
        (ELit (LitCons _ xs _) ,ts') = fromPi $ runVarName $ do
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
        theTypeExpr =  (ELit (LitCons theTypeName (map EVar theTypeArgs) theTypeFKind))
        theType = Constructor {
            conName = theTypeName,
            conType = theKind,
            conSlots = map tvrType theTypeArgs,
            conExpr = foldr ($) theTypeExpr (map ELam theTypeArgs),
            conDeriving = [ toName ClassName n | n <- hsDeclDerives decl],
            conAlias = False,
            conInhabits = tStar,
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
constructionExpression dataTable n typ@(ELit (LitCons pn xs _))
    | conAlias mc = ELam var (EVar var)
    | pn == conName pc = sub (conExpr mc) where
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
deconstructionExpression dataTable name typ@(ELit (LitCons pn xs _)) vs _vs' e | pn == conName pc = ans where
    Just mc = getConstructor name dataTable
    Just pc = getConstructor (conInhabits mc) dataTable
    ans = case conVirtual mc of
        Nothing -> Alt (LitCons name vs typ) e
        Just _ -> let ELit (LitCons _ [ELit (LitInt n t)] _) = conExpr mc in Alt (LitInt n t) e
deconstructionExpression wdt n ty vs vs' e | Just fa <- followAlias wdt ty  = deconstructionExpression wdt n fa vs vs' e
deconstructionExpression _ n e _ _ _ = error $ "deconstructionExpression: error in " ++ show n ++ ": " ++ show e

slotTypes ::
    DataTable -- ^ table of data constructors
    -> Name   -- ^ name of constructor
    -> E      -- ^ type of value
    -> [E]    -- ^ type of each slot
slotTypes wdt n (ELit (LitCons pn xs _))
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
        vt = case conVirtual const of
            Nothing -> empty
            Just ss -> text "virtual:" <+> tshow ss
        ih = text "inhabits:" <+> tshow conInhabits
        ch = text "children:" <+> tshow conChildren
        Constructor {
            conName = conName,
            conType = conType,
            conExpr = conExpr,
            conAlias = conAlias,
            conInhabits = conInhabits,
            conChildren = conChildren
            } = const
    xs = [text x <+> hang 0 (c y) | (x,y) <- ds]
    ds = sortBy (\(x,_) (y,_) -> compare x y) [ (show x,y)  | (x,y) <-  Map.toList mp]


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
    f (ELit (LitCons n as _)) | (a:as') <- reverse as = f $ EAp (ELit (LitCons n (reverse as') undefined)) a
    f (ELit (LitCons n [] _)) = return $ atom $ text $ show n
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

expandAliases :: DataTableMonad m => E -> m E
expandAliases e = do
    dt <- getDataTable
    return (followAliases dt e)


