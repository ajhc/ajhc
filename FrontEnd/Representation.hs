{-
        Copyright:        Mark Jones and The Hatchet Team
                          (see file Contributors)
        Module:           Representation
        Primary Authors:  Mark Jones and Bernie Pope
        Description:      The basic data types for representing objects
                          in the type inference algorithm.
        Notes:            See the file License for license information
                          Large parts of this module were derived from
                          the work of Mark Jones' "Typing Haskell in
                          Haskell", (http://www.cse.ogi.edu/~mpj/thih/)
-}

module Representation(
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
    tList
    )where


import Control.Monad.Identity
import Data.IORef
import Text.PrettyPrint.HughesPJ(Doc)

import Atom
import Binary
import Doc.DocLike
import Doc.PPrint(pprint,PPrint)
import HsSyn
import Name.Name
import Name.Names
import Support.CanType
import Name.VConsts
import qualified Doc.DocLike as D
import Support.Unparse
import Util.VarName
import FrontEnd.Tc.Kind


--------------------------------------------------------------------------------

-- Types

data MetaVarType = Tau | Rho | Sigma
             deriving(Eq,Ord,Show)
    {-! derive: GhcBinary !-}

data Type  = TVar { typeVar :: {-# UNPACK #-} !Tyvar }
           | TCon { typeCon :: !Tycon }
           | TAp  Type Type
           | TArrow Type Type
           | TForAll { typeArgs :: [Tyvar], typeBody :: (Qual Type) }
           | TExists { typeArgs :: [Tyvar], typeBody :: (Qual Type) }
           | TMetaVar { metaVar :: MetaVar }
           | TAssoc   { typeCon :: !Tycon, typeClassArgs :: [Type], typeExtraArgs :: [Type] }
             deriving(Ord,Show)
    {-! derive: GhcBinary !-}

data MetaVar = MetaVar { metaUniq :: !Int, metaKind :: Kind, metaRef :: (IORef (Maybe Type)), metaType :: MetaVarType } -- ^ used only in typechecker
             deriving(Show)
    {-! derive: GhcBinary !-}

instance Eq MetaVar where
    a == b = metaUniq a == metaUniq b

instance Ord MetaVar where
    compare a b = compare (metaUniq a) (metaUniq b)

instance TypeNames Type where
    tBool = TCon (Tycon tc_Bool kindStar)
    tString = TAp tList tChar
    tChar      = TCon (Tycon tc_Char kindStar)
    tUnit = TCon (Tycon tc_Unit kindStar)

instance Ord (IORef a)
instance Binary (IORef a)

tList = TCon (Tycon tc_List (Kfun kindStar kindStar))

instance Eq Type where
    (TVar a) == (TVar b) = a == b
    (TMetaVar a) == (TMetaVar b) = a == b
    (TCon a) == (TCon b) = a == b
    (TAp a' a) == (TAp b' b) = a' == b' && b == a
    (TArrow a' a) == (TArrow b' b) = a' == b' && b == a
    _ == _ = False

tassocToAp TAssoc { typeCon = con, typeClassArgs = cas, typeExtraArgs = eas } = foldl TAp (TCon con) (cas ++ eas)

-- Unquantified type variables

data Tyvar = Tyvar { tyvarAtom :: {-# UNPACK #-} !Atom, tyvarName ::  !Name, tyvarKind :: Kind }
    {-  derive: GhcBinary -}

instance Show Tyvar where
    showsPrec _ Tyvar { tyvarName = hn, tyvarKind = k } = shows hn . ("::" ++) . shows k

tForAll [] ([] :=> t) = t
tForAll vs (ps :=> TForAll vs' (ps' :=> t)) = tForAll (vs ++ vs') ((ps ++ ps') :=> t)
tForAll x y = TForAll x y

tExists [] ([] :=> t) = t
tExists vs (ps :=> TExists vs' (ps' :=> t)) = tExists (vs ++ vs') ((ps ++ ps') :=> t)
tExists x y = TExists x y


instance Show (IORef a) where
    showsPrec _ _ = ("<IORef>" ++)

tyvar n k = Tyvar (fromString $ show n) n k

instance Eq Tyvar where
    Tyvar { tyvarAtom = x } == Tyvar { tyvarAtom = y } = x == y
    Tyvar { tyvarAtom = x } /= Tyvar { tyvarAtom = y } = x /= y

instance Ord Tyvar where
    compare (Tyvar { tyvarAtom = x }) (Tyvar { tyvarAtom = y }) = compare x y
    (Tyvar { tyvarAtom = x }) <= (Tyvar { tyvarAtom = y }) = x <= y
    (Tyvar { tyvarAtom = x }) >= (Tyvar { tyvarAtom = y }) = x >= y
    (Tyvar { tyvarAtom = x }) <  (Tyvar { tyvarAtom = y })  = x < y
    (Tyvar { tyvarAtom = x }) >  (Tyvar { tyvarAtom = y })  = x > y



-- Type constructors

data Tycon = Tycon { tyconName :: Name, tyconKind :: Kind }
    deriving(Eq, Show,Ord)
    {-! derive: GhcBinary !-}

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
    {-! derive: GhcBinary !-}

-- Qualified entities
data Qual t =  [Pred] :=> t
              deriving(Show, Eq,Ord)
    {-! derive: GhcBinary !-}


instance (DocLike d,PPrint d t) => PPrint d (Qual t) where
    pprint ([] :=> r) = pprint r
    pprint ([x] :=> r) = pprint x <+> text "=>" <+> pprint r
    pprint (xs :=> r) = tupled (map pprint xs) <+> text "=>" <+> pprint r




type Class = Name
--------------------------------------------------------------------------------



instance  DocLike d => PPrint d Tyvar where
  pprint tv = tshow (tyvarName tv)

instance Binary Tyvar where
    put_ bh (Tyvar aa ab ac) = do
        put_ bh aa
        put_ bh ab
        put_ bh ac
    get bh = do
        aa <- get bh
        ab <- get bh
        ac <- get bh
        return (Tyvar aa ab ac)


instance FromTupname HsName where
    fromTupname (Qual (Module "Jhc.Basics") (HsIdent xs))  = fromTupname xs
    fromTupname _ = fail "fromTupname: not Prelude"

instance ToTuple HsName where
    toTuple n = (Qual (Module "Jhc.Basics") (HsIdent $ toTuple n))

-- pretty printing a HsName, Module and HsIdentifier

instance DocLike d => PPrint d HsName where
   pprint (Qual mod ident)
      -- don't print the Prelude module qualifier
      | mod == Module "Prelude" = pprint ident
      | otherwise               = pprint mod <> text "." <> pprint ident
   pprint (UnQual ident)
      = pprint ident

instance DocLike d => PPrint d Module where
   pprint (Module s) = text s

instance DocLike d => PPrint d HsIdentifier where
   pprint (HsIdent   s) = text s

instance DocLike d => PPrint d Type where
    pprint = prettyPrintType

withNewNames ts action = subVarName $ do
    ts' <- mapM newTyvarName ts
    action ts'

newTyvarName t = case tyvarKind t of
    x@(KBase Star) -> newLookupName (map (:[]) ['a' ..]) x t
    y@(KBase Star `Kfun` KBase Star) -> newLookupName (map (('f':) . show) [0 :: Int ..]) y t
    z@(KBase KUTuple) -> newLookupName (map (('u':) . show) [0 :: Int ..]) z t
    z@(KBase KFunRet) -> newLookupName (map (('r':) . show) [0 :: Int ..]) z t
    z -> newLookupName (map (('t':) . show) [0 :: Int ..]) z t


prettyPrintType :: DocLike d => Type -> d
prettyPrintType t  = unparse $ runIdentity (runVarNameT (f t)) where
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
            Nothing -> return $ atom $ tshow (tyvarAtom tyvar)
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
    pprint (IsIn c t) = text (show c) <+> prettyPrintType t
    pprint (IsEq t1 t2) = parens $ prettyPrintType t1 <+> text "=" <+> prettyPrintType t2

instance DocLike d => PPrint d MetaVar where
    pprint MetaVar { metaUniq = u, metaKind = k, metaType = t }
        | KBase Star <- k =  pprint t <> tshow u
        | otherwise = parens $ pprint t <> tshow u <> text " :: " <> pprint k

fromTAp t = f t [] where
    f (TAp a b) rs = f a (b:rs)
    f t rs = (t,rs)

fromTArrow t = f t [] where
    f (TArrow a b) rs = f b (a:rs)
    f t rs = (reverse rs,t)


instance CanType MetaVar Kind where
    getType mv = metaKind mv

instance CanType Tycon Kind where
    getType (Tycon _ k) = k

instance CanType Tyvar Kind where
    getType = tyvarKind

instance CanType Type Kind where
  getType (TCon tc) = getType tc
  getType (TVar u)  = getType u
  getType typ@(TAp t _) = case (getType t) of
                     (Kfun _ k) -> k
                     x -> error $ "Type.getType: kind error in: " ++ (show typ)
  getType (TArrow _l _r) = kindStar
  getType (TForAll _ (_ :=> t)) = getType t
  getType (TExists _ (_ :=> t)) = getType t
  getType (TMetaVar mv) = getType mv
  getType ta@TAssoc {} = getType (tassocToAp ta)

tTTuple ts | length ts < 2 = error "tTTuple"
tTTuple ts = foldl TAp (toTuple (length ts)) ts

tTTuple' ts = foldl TAp (TCon $ Tycon (unboxedNameTuple TypeConstructor  n) (foldr Kfun kindUTuple $ replicate n kindStar)) ts where
    n = length ts
