module DataConstructors(
    Constructor(..),
    DataTable(..),
    dataTablePrims,
    constructionExpression,
    deconstructionExpression,
    followAliases,
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
    typesCompatable
    ) where

import Control.Monad.Identity
import Control.Monad.Writer
import Data.Map as Map hiding(map)
import qualified Data.Set as Set
import List(sortBy,(\\))

import Binary
import Doc.DocLike
import Doc.Pretty
import E.E
import E.Show
import E.Shadow
import E.Subst
import E.Values
import GenUtil
import HsSyn
import MapBinaryInstance()
import Name.Name as Name
import Name.Names
import Name.VConsts
import Support.FreeVars
import PrimitiveOperators
import qualified Util.Seq as Seq
import Representation
import Support.CanType
import Support.Unparse
import Type(schemeToType)
import Util.HasSize
import Util.SameShape
import Util.VarName

tipe t = runVarName (tipe' t) where
    tipe' (TAp t1 t2) = liftM2 eAp (tipe' t1) (tipe' t2)
    tipe' (TArrow t1 t2) =  do
        t1' <- tipe' t1
        t2' <- tipe' t2
        return $ EPi (tVr 0 (t1')) t2'
    tipe' (TCon (Tycon n k)) =  return $ ELit (LitCons n [] (kind k))
    tipe' (TGen n (Tyvar { tyvarKind = k })) = return $  EVar (tVr ((n + 1) * 2 ) (kind k))
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
    conName :: Name,             -- name of constructor
    conType :: E,                -- type of constructor
    conExpr :: E,                -- expression which constructs this value
    conSlots :: [E],             -- slots
    conDeriving :: [Name],       -- classes this type derives
    conAlias :: Bool,            -- whether this is a simple alias and has no tag of its own.
    conInhabits :: Name,         -- what constructor it inhabits, similar to conType, but not quite.
    conChildren :: Maybe [Name]  -- if nothing, then type is abstract
    } deriving(Show)
    {-! derive: GhcBinary !-}

newtype DataTable = DataTable {
    constructorMap :: (Map Name Constructor)
    }
    {-! derive: GhcBinary, Monoid !-}

instance HasSize DataTable where
    size (DataTable d) = Map.size d

getConstructor :: Monad m => Name -> DataTable -> m Constructor
getConstructor n (DataTable dt) | n == tc_World__, Just c <- Map.lookup n dt = return c { conChildren = Nothing }
getConstructor n _ | Just v <- fromUnboxedNameTuple n, DataConstructor <- nameType n = return $ snd $ tunboxedtuple v
getConstructor n _ | Just v <- fromUnboxedNameTuple n, TypeConstructor <- nameType n = return $ fst $ tunboxedtuple v
getConstructor n (DataTable map) = case Map.lookup n map of
    Just x -> return x
    Nothing -> fail $ "getConstructor: " ++ show (nameType n,n)

-- | return the single constructor of product types

getProduct :: Monad m => DataTable -> E -> m Constructor
getProduct dataTable e | (ELit (LitCons cn _ _)) <- followAliases dataTable e, cn /= tc_World__, Just c <- getConstructor cn dataTable = f c where
    f c | Just [x] <- conChildren c = getConstructor x dataTable
        | otherwise = fail "Not Product type"
getProduct _ _ = fail "Not Product type"


tunboxedtuple n = (typeCons,dataCons) where
        dataCons = Constructor {
            conName = dc,
            conType = tipe,
            conSlots = [],
            conDeriving = [],
            conExpr = Unknown, -- error "expr" ELam (tVr 2 rt) (ELit (LitCons dc [EVar (tVr 2 rt)] tipe)),
            conAlias = False,
            conInhabits = tc,
            conChildren = Nothing
           }
        typeCons = Constructor {
            conName = tc,
            conType = eHash,
            conSlots = [],
            conDeriving = [],
            conExpr = tipe,
            conAlias = False,
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
            conInhabits = tStar,
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
            conChildren = Nothing
        }


primitiveTable = concatMap f allCTypes ++ map g (snub $ map ( \ (_,b,_) -> b) allCTypes) where
    g n = Constructor {
        conName = rn,
        conType = eHash,
        conSlots = [],
        conDeriving = [],
        conExpr = ELit (LitCons rn [] eHash),
        conAlias = False,
        conInhabits = tHash,
        conChildren = Nothing
       } where rn = toName RawType n
    f (x,y,z) | z /= "void" = [typeCons,dataCons] where
        dataCons = Constructor {
            conName = dc,
            conType = tipe,
            conSlots = [rt],
            conDeriving = [],
            conExpr = ELam (tVr 2 rt) (ELit (LitCons dc [EVar (tVr 2 rt)] tipe)),
            conAlias = False,
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
            conInhabits = tStar,
            conChildren = Just [dc]
           }

        rn = toName RawType y
        rt = ELit (LitCons rn [] eHash)
        dc = parseName DataConstructor x
        tc = parseName TypeConstructor x
        tipe = ELit (LitCons tc [] eStar)
    f _ = []

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
        g (ESort a) (ESort b) = when (a /= b) $ fail "Sorts don't match"
        g (EVar a) (EVar b) = when (a /= b) $ fail "Vars don't match"
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
                Left s -> fail (s ++ ":" ++ s')
    f :: Monad m => [Name] -> [Name] -> E -> E -> m ()
    f xs ys (ELit (LitCons n _ _)) _ | n `elem` xs = fail "Loop detected"
    f xs ys a@(ELit (LitCons n _ _)) b | Just x <- followAlias dataTable a = g' (n:xs) ys x b
    f _ _ _ _ = fail "Types don't match"


lookupCType dataTable e = case followAliases (mappend dataTablePrims dataTable) e of
    ELit (LitCons c [] _) | Just pt <- Map.lookup c ctypeMap -> return (c,pt)
    e' -> fail $ "lookupCType: " ++ show (e,e')

lookupCType' dataTable e = case followAliases (mappend dataTablePrims dataTable) e of
    ELit (LitCons c [] _)
        | Just Constructor { conChildren = Just [cn] }  <- getConstructor c dataTable,
          Just Constructor { conSlots = [st@(ELit (LitCons n [] _))] } <- getConstructor cn dataTable
            -> return (cn,st,show n)
    ELit (LitCons c [] _) | Just cn  <- getConstructor c dataTable -> fail $ "lookupCType: " ++ show cn
    e' -> fail $ "lookupCType': " ++ show (e,e')

followAlias :: Monad m => DataTable -> E -> m E
followAlias dataTable (EAp a b) = do
    a' <- followAlias dataTable a
    return (eAp a' b)
followAlias dataTable (ELit (LitCons c ts e))
    | Just con <- jcon, Just [cn] <- jcn, conAlias ccon  = return ans where
        jcn@(~(Just [cn])) = conChildren con
        Identity ccon = getConstructor cn dataTable
        jcon@(~(Just con)) = getConstructor c dataTable
        [sl] = conSlots ccon
        ans = doSubst False False (Map.fromList $ zip [2..] (map Just ts)) sl
followAlias _ e = fail "followAlias: not an alias"

followAliases :: DataTable -> E -> E
followAliases dataTable ap@EAp {} = case followAlias dataTable ap of
    Just x -> followAliases dataTable x
    Nothing -> ap
followAliases dataTable (ELit (LitCons c ts e))
    | Just con <- jcon, Just [cn] <- jcn, conAlias ccon  = followAliases dataTable ans where
        jcn@(~(Just [cn])) = conChildren con
        Identity ccon = getConstructor cn dataTable
        jcon@(~(Just con)) = getConstructor c dataTable
        [sl] = conSlots ccon
        ans = doSubst False False (Map.fromList $ zip [2..] (map Just ts)) sl
followAliases _ e = e

dataTablePrims = DataTable $ Map.fromList [ (conName x,x) | x <- tabsurd:tarrow:primitiveTable ]

{-# NOINLINE toDataTable #-}
toDataTable :: (Map Name Kind) -> (Map Name Scheme) -> [HsDecl] -> DataTable
toDataTable km cm ds = DataTable (Map.mapWithKey fixupMap $ Map.fromList [ (conName x,x) | x <- ds' ])  where
    fixupMap k _ | Just n <- getConstructor k dataTablePrims = n
    fixupMap _ n = n
    ds' = Seq.toList $ execWriter (mapM_ f ds)
    f decl@HsNewTypeDecl {  hsDeclCon = c } = dt decl True  [c]
    f decl@HsDataDecl {  hsDeclCons = cs } = dt decl False  cs
    f _ = return ()
    dt decl alias cs = do
        let dataCons = map makeData cs
        tell (Seq.fromList dataCons)
        tell $ Seq.singleton theType { conChildren = Just (map conName dataCons) }
        where
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
            conChildren = undefined
            } where
        makeData x = Constructor {
            conName = dataConsName,
            conType =foldr ($) (getType theExpr) (map EPi theTypeArgs),
            conSlots =  slots,
            conExpr = theExpr,
            conInhabits = theTypeName,
            conDeriving = [],
            conAlias = alias,
            conChildren = Nothing
            } where
            theExpr =  foldr ($) (strictize $ ELit (LitCons dataConsName (map EVar vars) theTypeExpr)) (map ELam vars)
            slots = map (subst . tvrType) ts -- XXX TODO fix this mapping
            vars = [ tvr { tvrType = t } | tvr <- ts | t <- slots ]
            strictize con = E.Subst.subst tvr { tvrIdent = -1 } Unknown $ f (zip (map isHsBangedTy args) vars) con where
                f ((False,_):rs) con = f rs con
                f ((True,var):rs) con = eStrictLet var (EVar var) con
                f [] con = con
            dataConsName =  toName Name.DataConstructor (hsConDeclName x)
            args = hsConDeclArgs x
            (ELit (LitCons _ xs _) ,ts') = fromPi $ tipe ty
            existentials = Set.toList $ freeVars (map getType ts') Set.\\ freeVars xs
            subst = substMap $ Map.fromList [ (tvrIdent tv ,EVar $ tv { tvrIdent = p }) | EVar tv <- xs | p <- [2,4..] ]
            ts = existentials ++ [ tvr {tvrIdent = x} | tvr <- ts' | x <- drop (5 + length theTypeArgs) [2,4..] ]
            Just (Forall _ (_ :=> ty)) = Map.lookup dataConsName cm

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
    sub = substMap $ Map.fromDistinctAscList [ (i,sl) | sl <- xs | i <- [2,4..] ]
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
    ans = Alt (LitCons name vs typ) e
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
    sub = substMap $ Map.fromDistinctAscList [ (i,sl) | sl <- xs | i <- [2,4..] ]
slotTypes wdt n e | Just fa <- followAlias wdt e  = slotTypes wdt n fa
slotTypes _ n e = error $ "slotTypes: error in " ++ show n ++ ": " ++ show e

showDataTable (DataTable mp) = vcat xs where
    c  const = vcat [t,e,cs,al,ih,ch] where
        t  = text "::" <+> ePretty conType
        e  = text "=" <+> ePretty conExpr
        cs = text "slots:" <+> tupled (map ePretty (conSlots const))
        al = text "alias:" <+> tshow conAlias
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
    arr = bop (R,0) (space <> text "->" <> space)
    app = bop (L,100) (text " ")


