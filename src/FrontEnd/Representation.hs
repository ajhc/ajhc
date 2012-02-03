module FrontEnd.Representation(
    Type(..),
    Tyvar(..),
    tyvar,
    Tycon(..),
    fn,
    Pred(..),
    Qual(..),
    Class,
    tForAll,
    tExists,
    MetaVarType(..),
    prettyPrintType,
    fromTAp,
    fromTArrow,
    tassocToAp,
    MetaVar(..),
    tTTuple,
    tTTuple',
    tList,
    tArrow,
    tAp
    )where

import Control.Monad.Identity
import Data.IORef

import Data.Binary
import Doc.DocLike
import Doc.PPrint
import FrontEnd.Tc.Kind
import Name.Name
import Name.Names
import Name.VConsts
import Support.CanType
import Support.Unparse
import Util.VarName

-- Types

data MetaVarType = Tau | Rho | Sigma
             deriving(Eq,Ord)
    {-! derive: Binary !-}

data Type  = TVar     { typeVar :: !Tyvar }
           | TCon     { typeCon :: !Tycon }
           | TAp      Type Type
           | TArrow   Type Type
           | TForAll  { typeArgs :: [Tyvar], typeBody :: (Qual Type) }
           | TExists  { typeArgs :: [Tyvar], typeBody :: (Qual Type) }
           | TMetaVar { metaVar :: MetaVar }
           | TAssoc   { typeCon :: !Tycon, typeClassArgs :: [Type], typeExtraArgs :: [Type] }
             deriving(Ord,Show)
    {-! derive: Binary !-}

-- | metavars are used in type checking
data MetaVar = MetaVar {
    metaUniq :: {-# UNPACK #-} !Int,
    metaKind :: Kind,
    metaRef :: {-# UNPACK #-} !(IORef (Maybe Type)),
    metaType :: {-# UNPACK #-} !MetaVarType
    }
    {-! derive: Binary !-}

instance Eq MetaVar where
    a == b = metaUniq a == metaUniq b

instance Ord MetaVar where
    compare a b = compare (metaUniq a) (metaUniq b)

instance TypeNames Type where
    tBool   = TCon (Tycon tc_Bool kindStar)
    tString = TAp tList tChar
    tChar   = TCon (Tycon tc_Char kindStar)
    tUnit   = TCon (Tycon tc_Unit kindStar)
    tCharzh = TCon (Tycon tc_Char_ kindHash)

instance Ord (IORef a)
instance Binary (IORef a)

tList :: Type
tList = TCon (Tycon tc_List (Kfun kindStar kindStar))

-- | The @(->)@ type constructor. Invariant: @tArrow@ shall not be fully applied. To this end, see 'tAp'.
tArrow :: Type
tArrow = TCon (Tycon tc_Arrow (kindArg `Kfun` (kindFunRet `Kfun` kindStar)))

-- | Type application, enforcing the invariant that there be no fully-applied 'tArrow's
tAp :: Type -> Type -> Type
tAp (TAp c@TCon{} a) b | c == tArrow = TArrow a b
tAp a b = TAp a b

instance Eq Type where
    (TVar a) == (TVar b) = a == b
    (TMetaVar a) == (TMetaVar b) = a == b
    (TCon a) == (TCon b) = a == b
    (TAp a' a) == (TAp b' b) = a' == b' && b == a
    (TArrow a' a) == (TArrow b' b) = a' == b' && b == a
    _ == _ = False

tassocToAp TAssoc { typeCon = con, typeClassArgs = cas, typeExtraArgs = eas } = foldl tAp (TCon con) (cas ++ eas)

-- Unquantified type variables

data Tyvar = Tyvar { tyvarName ::  {-# UNPACK #-} !Name, tyvarKind :: Kind }
    {-  derive: Binary -}

instance Show Tyvar where
    showsPrec _ Tyvar { tyvarName = hn, tyvarKind = k } = shows hn . ("::" ++) . shows k

tForAll [] ([] :=> t) = t
tForAll vs (ps :=> TForAll vs' (ps' :=> t)) = tForAll (vs ++ vs') ((ps ++ ps') :=> t)
tForAll x y = TForAll x y

tExists [] ([] :=> t) = t
tExists vs (ps :=> TExists vs' (ps' :=> t)) = tExists (vs ++ vs') ((ps ++ ps') :=> t)
tExists x y = TExists x y

tyvar n k = Tyvar n k

instance Eq Tyvar where
    Tyvar { tyvarName = x } == Tyvar { tyvarName = y } = x == y
    Tyvar { tyvarName = x } /= Tyvar { tyvarName = y } = x /= y

instance Ord Tyvar where
    compare (Tyvar { tyvarName = x }) (Tyvar { tyvarName = y }) = compare x y
    (Tyvar { tyvarName = x }) <= (Tyvar { tyvarName = y }) = x <= y
    (Tyvar { tyvarName = x }) >= (Tyvar { tyvarName = y }) = x >= y
    (Tyvar { tyvarName = x }) <  (Tyvar { tyvarName = y })  = x < y
    (Tyvar { tyvarName = x }) >  (Tyvar { tyvarName = y })  = x > y

-- Type constructors

data Tycon = Tycon { tyconName :: Name, tyconKind :: Kind }
    deriving(Eq, Show,Ord)
    {-! derive: Binary !-}

instance ToTuple Tycon where
    toTuple n = Tycon (nameTuple TypeConstructor n) (foldr Kfun kindStar $ replicate n kindStar)
instance ToTuple Type where
    toTuple n = TCon $ toTuple n

instance DocLike d => PPrint d Tycon where
   pprint (Tycon i _) = pprint i

infixr      4 `fn`
fn         :: Type -> Type -> Type
a `fn` b    = TArrow a b

--------------------------------------------------------------------------------

-- Predicates
data Pred   = IsIn Class Type | IsEq Type Type
              deriving(Show, Eq,Ord)
    {-! derive: Binary !-}

-- Qualified entities
data Qual t =  [Pred] :=> t
              deriving(Show, Eq,Ord)
    {-! derive: Binary !-}

instance (DocLike d,PPrint d t) => PPrint d (Qual t) where
    pprint ([] :=> r) = pprint r
    pprint ([x] :=> r) = pprint x <+> text "=>" <+> pprint r
    pprint (xs :=> r) = tupled (map pprint xs) <+> text "=>" <+> pprint r

instance  DocLike d => PPrint d Tyvar where
  pprint tv = tshow (tyvarName tv)

instance Binary Tyvar where
    put (Tyvar aa ab) = do
        put aa
        put ab
    get = do
        aa <- get
        ab <- get
        return (Tyvar aa ab)

instance DocLike d => PPrint d Module where
   pprint (Module s) = tshow s

withNewNames ts action = subVarName $ do
    ts' <- mapM newTyvarName ts
    action ts'

newTyvarName t = case tyvarKind t of
    x@(KBase Star) -> newLookupName (map (:[]) ['a' ..]) x t
    y@(KBase Star `Kfun` KBase Star) -> newLookupName (map (('f':) . show) [0 :: Int ..]) y t
    z@(KBase KUTuple) -> newLookupName (map (('u':) . show) [0 :: Int ..]) z t
    z@(KBase KQuest) -> newLookupName (map (('q':) . show) [0 :: Int ..]) z t
    z@(KBase KQuestQuest) -> newLookupName (map (('q':) . ('q':) . show) [0 :: Int ..]) z t
    z -> newLookupName (map (('t':) . show) [0 :: Int ..]) z t

prettyPrintType :: DocLike d => Type -> d
prettyPrintType = pprint

instance DocLike d => PPrint d Type where
    pprintAssoc _ n t = prettyPrintTypePrec n t

prettyPrintTypePrec :: DocLike d => Int -> Type -> d
prettyPrintTypePrec n t  = unparse $ zup (runIdentity (runVarNameT (f t))) where
    zup = if n >= 10 then pop empty else id
    arr = bop (R,0) (space <> text "->" <> space)
    app = bop (L,100) (text " ")
    fp (IsIn cn t) = do
        t' <- f t
        return (atom (text $ show cn) `app` t')
    fp (IsEq t1 t2) = do
        t1' <- f t1
        t2' <- f t2
        return (atom (parens $ unparse t1' <+> text "=" <+> unparse t2'))
    f (TForAll [] ([] :=> t)) = f t
    f (TForAll vs (ps :=> t)) = do
        withNewNames vs $ \ts' -> do
        t' <- f t
        ps' <- mapM fp ps
        return $ case ps' of
            [] ->  fixitize (N,-3) $ pop (text "forall" <+> hsep (map text ts') <+> text ". ")  (atomize t')
            [p] -> fixitize (N,-3) $ pop (text "forall" <+> hsep (map text ts') <+> text "." <+> unparse p <+> text "=> ")  (atomize t')
            ps ->  fixitize (N,-3) $ pop (text "forall" <+> hsep (map text ts') <+> text "." <+> tupled (map unparse ps) <+> text "=> ")  (atomize t')
    f (TExists [] ([] :=> t)) = f t
    f (TExists vs (ps :=> t)) = do
        withNewNames vs $ \ts' -> do
        t' <- f t
        ps' <- mapM fp ps
        return $ case ps' of
            [] ->  fixitize (N,-3) $ pop (text "exists" <+> hsep (map text ts') <+> text ". ")  (atomize t')
            [p] -> fixitize (N,-3) $ pop (text "exists" <+> hsep (map text ts') <+> text "." <+> unparse p <+> text "=> ")  (atomize t')
            ps ->  fixitize (N,-3) $ pop (text "exists" <+> hsep (map text ts') <+> text "." <+> tupled (map unparse ps) <+> text "=> ")  (atomize t')
    f (TCon tycon) = return $ atom (pprint tycon)
    f (TVar tyvar) = do
        vo <- maybeLookupName tyvar
        case vo of
            Just c  -> return $ atom $ text c
            Nothing -> return $ atom $ tshow (tyvarName tyvar)
    f (TAp (TCon (Tycon n _)) x) | n == tc_List = do
        x <- f x
        return $ atom (char '[' <> unparse x <> char ']')
    f TAssoc { typeCon = con, typeClassArgs = cas, typeExtraArgs = eas } = do
        let x = atom (pprint con)
        xs <- mapM f (cas ++ eas)
        return $ foldl app x xs
    f ta@(TAp {}) | (TCon (Tycon c _),xs) <- fromTAp ta, Just _ <- fromTupname c = do
        xs <- mapM f xs
        return $ atom (tupled (map unparse xs))
    f (TAp t1 t2) = do
        t1 <- f t1
        t2 <- f t2
        return $ t1 `app` t2
    f (TArrow t1 t2) = do
        t1 <- f t1
        t2 <- f t2
        return $ t1 `arr` t2
    f (TMetaVar mv) = return $ atom $ pprint mv
    f tv = return $ atom $ parens $ text ("FrontEnd.Tc.Type.pp: " ++ show tv)

instance DocLike d => PPrint d MetaVarType where
    pprint  t = case t of
        Tau -> char 't'
        Rho -> char 'r'
        Sigma -> char 's'

instance DocLike d => PPrint d Pred where
    pprint (IsIn c t) = tshow c <+> pprintParen t
    pprint (IsEq t1 t2) = parens $ prettyPrintType t1 <+> text "=" <+> prettyPrintType t2

instance DocLike d => PPrint d MetaVar where
    pprint MetaVar { metaUniq = u, metaKind = k, metaType = t }
        | KBase Star <- k =  pprint t <> tshow u
        | otherwise = parens $ pprint t <> tshow u <> text " :: " <> pprint k

instance Show MetaVarType where
    show mv = pprint mv

instance Show MetaVar where
    show mv = pprint mv

fromTAp t = f t [] where
    f (TAp a b) rs = f a (b:rs)
    f t rs = (t,rs)

fromTArrow t = f t [] where
    f (TArrow a b) rs = f b (a:rs)
    f t rs = (reverse rs,t)

instance CanType MetaVar where
    type TypeOf MetaVar = Kind
    getType mv = metaKind mv

instance CanType Tycon where
    type TypeOf Tycon = Kind
    getType (Tycon _ k) = k

instance CanType Tyvar where
    type TypeOf Tyvar = Kind
    getType = tyvarKind

instance CanType Type where
    type TypeOf Type = Kind
    getType (TCon tc) = getType tc
    getType (TVar u)  = getType u
    getType typ@(TAp t _) = case (getType t) of
                       (Kfun _ k) -> k
                       x -> error $ "Representation.getType: kind error in: " ++ (show typ)
    getType (TArrow _l _r) = kindStar
    getType (TForAll _ (_ :=> t)) = getType t
    getType (TExists _ (_ :=> t)) = getType t
    getType (TMetaVar mv) = getType mv
    getType ta@TAssoc {} = getType (tassocToAp ta)

tTTuple ts | length ts < 2 = error "tTTuple"
tTTuple ts = foldl TAp (toTuple (length ts)) ts

tTTuple' ts = foldl TAp (TCon $ Tycon (unboxedNameTuple TypeConstructor  n) (foldr Kfun kindUTuple $ replicate n kindStar)) ts where
    n = length ts
