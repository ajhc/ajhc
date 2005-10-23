module DataConstructors(
    Constructor(..),
    DataTable(..),
    followAliases,
    getConstructor,
    getConstructorArities,
    getProduct,
    getSiblings,
    lookupCType,
    lookupCType',
    showDataTable,
    slotTypes,
    toDataTable,
    typesCompatable
    ) where

import Control.Monad.Identity
import Control.Monad.Writer
import Data.Map as Map hiding(map)
import List(sortBy)

import Binary
import Doc.DocLike
import Doc.Pretty
import E.E
import E.Pretty
import E.Shadow
import E.Subst
import GenUtil
import HsSyn
import MapBinaryInstance()
import Name.Name as Name
import Name.Names
import PrimitiveOperators
import qualified Util.Seq as Seq
import Representation
import Util.HasSize
import Util.SameShape
import Name.VConsts


tipe (TAp t1 t2) = eAp (tipe t1) (tipe t2)
tipe (TArrow t1 t2) =  EPi (tVr 0 (tipe t1)) (tipe t2)
tipe (TCon (Tycon n k)) =  ELit (LitCons (toName TypeConstructor n) [] (kind k))
tipe (TGen n (Tyvar { tyvarKind = k })) = EVar (tVr ((n + 1) * 2 ) (kind k))
tipe (TVar Tyvar {}) = error "tipe': Tyvar"
kind Star = eStar
kind (Kfun k1 k2) = EPi (tVr 0 (kind k1)) (kind k2)
kind (KVar _) = error "Kind variable still existing."
{-
data DataType = Alias |
    Boxed               -- ^ values are always tagged and the domain includes closures which evaluate to a term of this type as well as it's data constructors.
    | BoxedPrimitive    -- ^ values are always tagged and the domain includes closures which evaluate to a term of this type, other values in the domain are system dependent however.
    | UnboxedPrimitive  -- ^ values do not have a tag and the representation is system dependent
    | Unboxed           -- ^ values do not have a tag, only a single constructor is allowed.
    | UnboxedTagged     -- ^ values do have a tag, but closures not in the domain.
    | Alias             -- ^ this type is isomorphic to an existing type
-}

-- | Record describing a data type.
-- * is also a data type containing the type constructors, which are unboxed, yet tagged.


data Constructor = Constructor {
    conName :: Name,             -- name of constructor
    conType :: E,                -- type of constructor
    conExpr :: E,                -- expression which constructs this value
    conSlots :: [E],             -- slots
    conDeriving :: [Name],       -- classes this type derives
    conClosures :: Bool,         -- does the domain contain closures?
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
            conClosures = False,
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
            conClosures = False,
            conAlias = False,
            conInhabits = tHash,
            conChildren = Just [dc]
           }

        dc = unboxedNameTuple DataConstructor n
        tc = unboxedNameTuple TypeConstructor n
        tipe = ELit (LitCons tc [] eHash)


tabsurd = Constructor {
            conName = toName TypeConstructor "Absurd#",
            conType = eStar,
            conSlots = [],
            conDeriving = [],
            conExpr = tAbsurd eStar,
            conClosures = False,
            conAlias = False,
            conInhabits = tStar,
            conChildren = Nothing
    }

tarrow = Constructor {
            conName = toName TypeConstructor ("Prelude","->"),
            conType = EPi (tVr 0 eStar) (EPi (tVr 0 eStar) eStar),
            conSlots = [eStar,eStar],
            conDeriving = [],
            conExpr = ELam (tVr 2 eStar) (ELam (tVr 4 eStar) (EPi (tVr 0 (EVar $ tVr 2 eStar)) (EVar $ tVr 4 eStar))),
            conClosures = True,
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
        conClosures = False,
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
            conClosures = True,
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
            conClosures = True,
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


lookupCType dataTable e = case followAliases dataTable e of
    ELit (LitCons c [] _) | Just pt <- Map.lookup c ctypeMap -> return (c,pt)
    e' -> fail $ "lookupCType: " ++ show (e,e')

lookupCType' dataTable e = case followAliases dataTable e of
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

dataTablePrims =  Map.fromList [ (conName x,x) | x <- tabsurd:tarrow:primitiveTable ]

toDataTable :: (Map Name Kind) -> (Map Name Scheme) -> [HsDecl] -> DataTable
toDataTable km cm ds = DataTable $ Map.union dataTablePrims  (Map.fromList [ (conName x,x) | x <- ds' ])  where
    ds' = Seq.toList $ execWriter (mapM_ f ds)
    f decl@HsNewTypeDecl {  hsDeclCon = c } = dt decl True  [c]
    f decl@HsDataDecl {  hsDeclCons = cs } = dt decl False  cs
    f _ = return ()
    dt decl alias cs = do
        cs' <- mapM dc cs
        tell $ Seq.singleton d { conChildren = Just cs' }
        where
        as = hsDeclArgs decl
        name = hsDeclName decl
        d = Constructor {
            conName = nm,
            conType = kind $ runIdentity (Map.lookup nm km),
            conSlots = map tvrType ts,
            conExpr = foldr ($) (ELit (LitCons  nm (map EVar ts) rt)) (map ELam ts),
            conClosures = True,
            conDeriving = [ toName ClassName n | n <- hsDeclDerives decl],
            conAlias = False,
            conInhabits = tStar,
            conChildren = undefined
            }
        (rt,ts') = fromPi (conType d)
        ts = [ tvr { tvrIdent = x } | tvr  <- ts' | x <- [2,4..] ]
        nm = toName Name.TypeConstructor name
        dc x = let z = dc' x in tell (Seq.singleton z) >> return (conName z)
        dc' x = Constructor {
            conName = nm',
            conType = ty',
            conSlots = map (subst . tvrType) ts,  -- XXX TODO fix this mapping
            conExpr = foldr ($) (ELit (LitCons  nm' (map EVar ts) rt)) (map ELam ts),
            conInhabits = nm,
            conDeriving = [],
            conAlias = alias,
            conClosures = False,
            conChildren = Nothing
            } where
            nm' =  toName Name.DataConstructor (hsConDeclName x)
            (rt@(ELit (LitCons _ xs _)) ,ts') = fromPi ty'
            subst = substMap $ Map.fromList [ (tvrIdent tv ,EVar $ tv { tvrIdent = p }) | EVar tv <- xs | p <- [2,4..] ]
            ts = [ tvr { tvrIdent =  (x)}   | tvr <- ts' | x <- [2,4..] ]
            ty' = tipe ty
            (Forall _ (_ :=> ty)) = runIdentity $ Map.lookup nm' cm



getConstructorArities  :: DataTable -> [(Name,Int)]
getConstructorArities (DataTable dt) = [ (n,length $ conSlots c) | (n,c) <- Map.toList dt]




slotTypes ::
    DataTable -- ^ table of data constructors
    -> Name   -- ^ name of constructor
    -> E      -- ^ type of value
    -> [E]    -- ^ type of each slot
slotTypes wdt@(DataTable dt) n (ELit (LitCons pn xs _))
    | pn == conName pc = [sub x | x <- conSlots mc ]
    where
    Identity mc = getConstructor n wdt
    Just pc = Map.lookup (conInhabits mc) dt
    sub = substMap $ Map.fromDistinctAscList [ (i,sl) | sl <- xs | i <- [2,4..] ]
slotTypes wdt n e | Just fa <- followAlias wdt e  = slotTypes wdt n fa
slotTypes _ n e = error $ "slotTypes: error in " ++ show n ++ ": " ++ show e

showDataTable (DataTable mp) = vcat xs where
    c  const = vcat [t,e,cl,cs,al,ih,ch] where
        t = text "::" <+> ePretty conType
        e = text "=" <+> ePretty conExpr
        cl = text "closures:" <+> tshow conClosures
        cs = text "slots:" <+> tupled (map ePretty (conSlots const))
        al = text "alias:" <+> tshow conAlias
        ih = text "inhabits:" <+> tshow conInhabits
        ch = text "children:" <+> tshow conChildren
        Constructor {
            conName = conName, conType = conType, conExpr = conExpr, conClosures = conClosures,
                conAlias  = conAlias, conInhabits = conInhabits, conChildren = conChildren
                    } = const
    xs =  [ text x <+> hang 0  (c y) | (x,y) <- ds]
    ds = sortBy (\(x,_) (y,_) -> compare x y) [ (show x,y)  | (x,y) <-  Map.toList mp]


getSiblings :: DataTable -> Name -> Maybe [Name]
getSiblings (DataTable mp) n
    | Just c <- Map.lookup n mp, Just s <- Map.lookup (conInhabits c) mp = conChildren s
    | otherwise =  Nothing
--    | otherwise = error $ "getSiblings: " ++ show n ++ show (Map.keys mp) ++ show (n `elem` (Map.keys mp))


-- These will eventually be described in the Prelude directly as boxed versions of the
-- underlying unboxed type.
--
-- TODO float, double, integer

--builtinTypes = [ btype tInt, btype tChar ]

--btype x = Data {
--    dtName =  x,
--    dtType = tStar,
--    dtArgs = [],
--    dtAlias = False,
--    dtCLosures = True,
--    dtCons = Nothing
--}
