module FrontEnd.Tc.Type(
    findType,
    fn,
    kind,
    Kind(..),
    Sigma(),
    Pred(..),
    Tau(),
    Rho(),
    tForAll,
    module FrontEnd.Tc.Type,
    Qual(..),
    Tycon(..),
    Type(..),
    tyvar,
    tList,
    Tyvar(..)
    ) where

import Control.Monad.Writer
import List
import Data.IORef

import Doc.DocLike
import Util.VarName
import Unparse
import Doc.PPrint
import Representation
import Type(kind)

type Box = IORef Type
type Sigma' = Sigma
type Tau' = Tau
type Rho' = Rho

type MetaTV = Tyvar
type SkolemTV = Tyvar
type BoundTV = Tyvar

isMetaTV :: Tyvar -> Bool
isMetaTV Tyvar { tyvarRef = Just _ } = True
isMetaTV _ = False


openBox :: MonadIO m => Box -> m Sigma
openBox x = liftIO $ readIORef x

fillBox :: MonadIO m => Box -> Type -> m ()
fillBox x t | not (isBoxy t) = liftIO $ writeIORef x t
fillBox x t = error "attempt to fillBox with boxy type"

isTau :: Type -> Bool
isTau TForAll {} = False
isTau TBox {} = False
isTau (TAp a b) = isTau a && isTau b
isTau (TArrow a b) = isTau a && isTau b
isTau _ = True

isTau' :: Type -> Bool
isTau' TForAll {} = False
isTau' (TAp a b) = isTau a && isTau b
isTau' (TArrow a b) = isTau a && isTau b
isTau' _ = True

isBoxy :: Type -> Bool
isBoxy TBox {} = True
isBoxy (TForAll _ (_ :=> t)) = isBoxy t
isBoxy (TAp a b) = isBoxy a || isBoxy b
isBoxy (TArrow a b) = isBoxy a || isBoxy b
isBoxy _ = False

isRho' :: Type -> Bool
isRho' TForAll {} = False
isRho' _ = True

isRho :: Type -> Bool
isRho r = isRho' r && not (isBoxy r)


fromTAp t = f t [] where
    f (TAp a b) rs = f a (b:rs)
    f t rs = (t,reverse rs)

extractMetaTV :: Monad m => Type -> m MetaTV
extractMetaTV (TVar t) | isMetaTV t = return t
extractMetaTV t = fail $ "not a metaTyVar:" ++ prettyPrintType t

extractTyVar ::  Monad m => Type -> m Tyvar
extractTyVar (TVar t) | not $ isMetaTV t = return t
extractTyVar t = fail $ "not a Var:" ++ prettyPrintType t


prettyPrintType :: DocLike d => Type -> d
prettyPrintType t  = unparse $ runVarName (f t) where
    arr = bop (R,0) (space <> text "->" <> space)
    app = bop (L,100) (text " ")
--    fp :: DocLike d => Pred -> VarName Atom Char (Unparse d)
    fp (IsIn cn t) = do
        t' <- f t
        return (atom (text $ show cn) `app` t')
--    f :: DocLike d => Type -> VarName Atom Char (Unparse d)
    f (TForAll vs (ps :=> t)) = do
        ts' <- mapM (newLookupName ['a'..] ()) vs
        t' <- f t
        ps' <- mapM fp ps
        return $ fixitize (N,-3) $ pop (text "forall" <+> hsep (map char ts') <+> text ". " <> if null ps then empty else tupled (map unparse ps') <+> text "=> ")  (atomize t')
    f (TCon tycon) = return $ atom (pprint tycon)
    f (TVar tyvar) = do
        vo <- newLookupName ['a' .. ] () tyvar
        return $ atom $ char vo
           -- check for the Prelude.[] special case
    f (TAp t1 t2) = do
        t1 <- f t1
        t2 <- f t2
        return $ t1 `app` t2
    f (TArrow t1 t2) = do
        t1 <- f t1
        t2 <- f t2
        return $ t1 `arr` t2
    f (TBox Star i _) = return $ atom $ text "_" <> tshow i
    f t | ~(TBox k i _) <- t = return $ atom $ parens $ text "_" <> tshow i <> text " :: " <> pprint k



data UnVarOpt = UnVarOpt {
    openBoxes :: Bool,
    failEmptyMetaVar :: Bool
    }

flattenMetaVars t = unVar UnVarOpt { openBoxes = False, failEmptyMetaVar = False } t
flattenType t =  unVar UnVarOpt { openBoxes = True, failEmptyMetaVar = False } t


class UnVar t where
    unVar' ::  UnVarOpt -> t -> IO t

unVar :: (UnVar t, MonadIO m) => UnVarOpt -> t -> m t
unVar opt t = liftIO (unVar' opt t)

instance UnVar t => UnVar [t] where
   unVar' opt xs = mapM (unVar' opt) xs

instance UnVar Pred where
    unVar' opt (IsIn c t) = IsIn c `liftM` unVar' opt t

instance UnVar t => UnVar (Qual t) where
    unVar' opt (ps :=> t) = liftM2 (:=>) (unVar' opt ps) (unVar' opt t)

instance UnVar Type where
    unVar' opt tv =  do
        let ft (TAp x y) = liftM2 TAp (unVar' opt x) (unVar' opt y)
            ft (TArrow x y) = liftM2 TArrow (unVar' opt x) (unVar' opt y)
            ft t@TCon {} = return t
            ft (TForAll vs qt) = do
                when (any isMetaTV vs) $ error "metatv in forall binding"
                qt' <- unVar' opt qt
                return $ TForAll vs qt'
            ft t@TBox { typeBox = box }
                | openBoxes opt =  readIORef box >>= unVar' opt
                | otherwise = return t
            ft t | Just tv <- extractMetaTV t = if failEmptyMetaVar opt then fail $ "empty meta var" ++ prettyPrintType t else return (TVar tv)
            ft t | ~(Just tv) <- extractTyVar t  = return (TVar tv)
        tv' <- findType tv
        ft tv'



freeMetaVars :: Type -> [MetaTV]
freeMetaVars t = filter isMetaTV $ allFreeVars t

freeTyVars :: Type -> [Tyvar]
freeTyVars t = filter (not . isMetaTV) $ allFreeVars t

allFreeVars (TVar u)      = [u]
allFreeVars (TAp l r)     = allFreeVars l `union` allFreeVars r
allFreeVars (TArrow l r)  = allFreeVars l `union` allFreeVars r
allFreeVars TCon {}       = []
allFreeVars TBox {}       = []
allFreeVars typ | ~(TForAll vs (_ :=> t)) <- typ = allFreeVars t List.\\ vs

