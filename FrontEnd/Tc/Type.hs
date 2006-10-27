module FrontEnd.Tc.Type(
    Kind(..),
    MetaVar(..),
    MetaVarType(..),
    Pred(..),
    Preds(),
    Qual(..),
    Tycon(..),
    Type(..),
    Tyvar(..),
    fn,
    followTaus,
    fromTAp,
    fromTArrow,
    module FrontEnd.Tc.Type,
    prettyPrintType,
    readMetaVar,
    tForAll,
    tList,
    Constraint(..),
    applyTyvarMap,
    tyvar
    ) where

import Control.Monad.Identity
import Control.Monad.Writer
import Data.IORef
import List
import qualified Data.Map as Map

import Doc.DocLike
import Doc.PPrint
import Name.Name
import FrontEnd.SrcLoc
import Name.Names
import Name.VConsts
import Representation
import Support.CanType
import Support.FreeVars
import Support.Tickle
import qualified Type as T

type Sigma' = Sigma
type Tau' = Tau
type Rho' = Rho
type Sigma = Type
type Rho = Type
type Tau = Type

type SkolemTV = Tyvar
type BoundTV = Tyvar

type Preds = [Pred]

data Constraint = Equality {
    constraintSrcLoc :: SrcLoc,
    constraintType1 :: Type,
    constraintType2 ::Type
    }

instance HasLocation Constraint where
    srcLoc Equality { constraintSrcLoc = sl } = sl

applyTyvarMap :: Map.Map Tyvar Type -> Type -> Type
applyTyvarMap = T.apply

typeOfType :: Type -> (MetaVarType,Bool)
typeOfType TForAll { typeArgs = as, typeBody = _ :=> t } = (Sigma,isBoxy t)
typeOfType t | isTau' t = (Tau,isBoxy t)
typeOfType t = (Rho,isBoxy t)

fromType :: Sigma -> ([Tyvar],[Pred],Type)
fromType s = case s of
    TForAll as (ps :=> r) -> (as,ps,r)
    r -> ([],[],r)

isTau :: Type -> Bool
isTau TForAll {} = False
isTau (TMetaVar MetaVar { metaType = t })
    | t == Tau = True
    | otherwise = False
isTau t = and $ tickleCollect ((:[]) . isTau) t

isTau' :: Type -> Bool
isTau' TForAll {} = False
isTau' t = and $ tickleCollect ((:[]) . isTau') t

isBoxy :: Type -> Bool
isBoxy (TMetaVar MetaVar { metaType = t }) | t > Tau = True
isBoxy t = or $ tickleCollect ((:[]) . isBoxy) t


isRho' :: Type -> Bool
isRho' TForAll {} = False
isRho' _ = True

isRho :: Type -> Bool
isRho r = isRho' r && not (isBoxy r)


isBoxyMetaVar MetaVar { metaType = t } = t > Tau


extractTyVar ::  Monad m => Type -> m Tyvar
extractTyVar (TVar tv) = return tv
extractTyVar t = fail $ "not a Var:" ++ show t

extractMetaVar :: Monad m => Type -> m MetaVar
extractMetaVar (TMetaVar t)  = return t
extractMetaVar t = fail $ "not a metaTyVar:" ++ show t

extractBox :: Monad m => Type -> m MetaVar
extractBox (TMetaVar mv) | metaType mv > Tau  = return mv
extractBox t = fail $ "not a metaTyVar:" ++ show t



data UnVarOpt = UnVarOpt {
    openBoxes :: Bool,
    failEmptyMetaVar :: Bool
    }

--flattenMetaVars t = unVar UnVarOpt { openBoxes = False, failEmptyMetaVar = False } t
flattenType t =  unVar UnVarOpt { openBoxes = True, failEmptyMetaVar = False } t

-- flattenType t = do (t,_,_) <- unbox t ; return t


class UnVar t where
    unVar' ::  UnVarOpt -> t -> IO t

unVar :: (UnVar t, MonadIO m) => UnVarOpt -> t -> m t
unVar opt t = liftIO (unVar' opt t)

instance UnVar t => UnVar [t] where
   unVar' opt xs = mapM (unVar' opt) xs

instance UnVar Pred where
    unVar' opt (IsIn c t) = IsIn c `liftM` unVar' opt t
    unVar' opt (IsEq t1 t2) = liftM2 IsEq (unVar' opt t1) (unVar' opt t2)

instance (UnVar a,UnVar b) => UnVar (a,b) where
    unVar' opt (a,b) = do
        a <- unVar' opt a
        b <- unVar' opt b
        return (a,b)

instance UnVar t => UnVar (Qual t) where
    unVar' opt (ps :=> t) = liftM2 (:=>) (unVar' opt ps) (unVar' opt t)

instance UnVar Type where
    unVar' opt tv =  do
        let ft (TAp x y) = liftM2 TAp (unVar' opt x) (unVar' opt y)
            ft (TArrow x y) = liftM2 TArrow (unVar' opt x) (unVar' opt y)
            ft t@TCon {} = return t
            ft (TForAll vs qt) = do
                qt' <- unVar' opt qt
                return $ TForAll vs qt'
            ft (TExists vs qt) = do
                qt' <- unVar' opt qt
                return $ TExists vs qt'
            ft t@(TMetaVar _) = if failEmptyMetaVar opt then fail $ "empty meta var" ++ prettyPrintType t else return t
            ft t = tickleM (unVar' opt . (id :: Type -> Type)) t
            --ft t | Just tv <- extractMetaTV t = if failEmptyMetaVar opt then fail $ "empty meta var" ++ prettyPrintType t else return (TVar tv)
            --ft t | ~(Just tv) <- extractTyVar t  = return (TVar tv)
            --ft t | ~(Just tv) <- extractTyVar t  = return (TVar tv)
        tv' <- findType tv
        ft tv'

followTaus :: MonadIO m => Type -> m Type
followTaus tv@(TMetaVar mv@MetaVar {metaRef = r }) | not (isBoxyMetaVar mv) = liftIO $ do
    rt <- readIORef r
    case rt of
        Nothing -> return tv
        Just t -> do
            t' <- followTaus t
            writeIORef r (Just t')
            return t'
followTaus tv = return tv


findType :: MonadIO m => Type -> m Type
findType tv@(TMetaVar MetaVar {metaRef = r }) = liftIO $ do
    rt <- readIORef r
    case rt of
        Nothing -> return tv
        Just t -> do
            t' <- findType t
            writeIORef r (Just t')
            return t'
findType tv = return tv


readMetaVar :: MonadIO m => MetaVar -> m (Maybe Type)
readMetaVar MetaVar { metaRef = r }  = liftIO $ do
    rt <- readIORef r
    case rt of
        Nothing -> return Nothing
        Just t -> do
            t' <- findType t
            writeIORef r (Just t')
            return (Just t')



freeMetaVars :: Type -> [MetaVar]
freeMetaVars (TMetaVar mv) = [mv]
freeMetaVars t = foldr union [] $ tickleCollect ((:[]) . freeMetaVars) t
--freeMetaVars (TVar u)      = []
--freeMetaVars (TAp l r)     = freeMetaVars l `union` freeMetaVars r
--freeMetaVars (TArrow l r)  = freeMetaVars l `union` freeMetaVars r
--freeMetaVars TCon {}       = []
--freeMetaVars (TMetaVar mv) = [mv]
--freeMetaVars (TForAll vs qt) = freeVars qt
--freeMetaVars (TExists vs qt) = freeVars qt

instance FreeVars Type [Tyvar] where
    freeVars (TVar u)      = [u]
    freeVars (TForAll vs qt) = freeVars qt List.\\ vs
    freeVars (TExists vs qt) = freeVars qt List.\\ vs
    freeVars t = foldr union [] $ tickleCollect ((:[]) . (freeVars :: Type -> [Tyvar])) t
--    freeVars (TAp l r)     = freeVars l `union` freeVars r
--    freeVars (TArrow l r)  = freeVars l `union` freeVars r
--    freeVars TCon {}       = []
--    freeVars TMetaVar {}   = []

instance FreeVars Type [MetaVar] where
    freeVars t = freeMetaVars t

instance (FreeVars t b,FreeVars Pred b) => FreeVars (Qual t) b where
    freeVars (ps :=> t)  = freeVars t `mappend` freeVars ps

instance FreeVars Type b =>  FreeVars Pred b where
    freeVars (IsIn _c t)  = freeVars t
    freeVars (IsEq t1 t2)  = freeVars (t1,t2)


instance Tickleable Type Pred where
    tickleM f (IsIn c t) = liftM (IsIn c) (f t)
    tickleM f (IsEq t1 t2) = return IsEq `ap` f t1 `ap` f t2

instance Tickleable Type Type where
    tickleM f (TAp l r) = return TAp `ap` f l `ap` f r
    tickleM f (TArrow l r) = return TArrow `ap` f l `ap` f r
    tickleM f (TAssoc c cas eas) = return (TAssoc c) `ap` mapM f cas `ap` mapM f eas
    tickleM f (TForAll ta (ps :=> t)) = do
        ps <- mapM (tickleM f) ps
        return (TForAll ta . (ps :=>)) `ap` f t
    tickleM f (TExists ta (ps :=> t)) = do
        ps <- mapM (tickleM f) ps
        return (TExists ta . (ps :=>)) `ap` f t
    tickleM _ t = return t


-- returns (new type, any open boxes, any open tauvars)
unbox :: MonadIO m => Type -> m (Type,Bool,Bool)
unbox tv = do
    let ft (TAp x y) = liftM2 TAp (ft' x) (ft' y)
        ft (TArrow x y) = liftM2 TArrow (ft' x) (ft' y)
        ft t@TCon {} = return t
        ft (TForAll vs (ps :=> t)) = do
            ps' <- sequence (map (tickleM ft') ps) --[  ft' t >>= return . IsIn c | ~(IsIn c t) <- ps ]
            t' <- ft' t
            return $ TForAll vs (ps' :=> t')
        ft (TExists vs (ps :=> t)) = do
            ps' <- sequence (map (tickleM ft') ps) --[  ft' t >>= return . IsIn c | ~(IsIn c t) <- ps ]
            --ps' <- sequence [ ft' t >>= return . IsIn c | ~(IsIn c t) <- ps ]
            t' <- ft' t
            return $ TExists vs (ps' :=> t')
        ft t@(TMetaVar mv)
            | isBoxyMetaVar mv = tell ([True],[]) >> return t
            | otherwise =  tell ([],[True]) >> return t
        ft t = tickleM ft' t
        --ft t | ~(Just tv) <- extractTyVar t  = return (TVar tv)
        ft' t = findType t >>= ft
    (tv,(eb,et)) <- runWriterT (ft' tv)
    return (tv,or eb, or et)




data Rule = RuleSpec {
    ruleUniq :: (Module,Int),
    ruleName :: Name,
    ruleSuper :: Bool,
    ruleType :: Type
    } |
    RuleUser {
    ruleUniq :: (Module,Int),
    ruleFreeTVars :: [(Name,Kind)]
    }


-- CTFun f => \g . \y -> f (g y)
data CoerceTerm = CTId | CTAp [Type] | CTAbs [Tyvar] | CTFun CoerceTerm | CTCompose CoerceTerm CoerceTerm

instance Show CoerceTerm where
    showsPrec _ CTId = showString "id"
    showsPrec n (CTAp ts) = ptrans (n > 10) parens $ char '@' <+> hsep (map (parens . prettyPrintType) ts)
    showsPrec n (CTAbs ts) = ptrans (n > 10) parens $ char '\\' <+> hsep (map pprint ts)
    showsPrec n (CTFun ct) = ptrans (n > 10) parens $ text "->" <+> showsPrec 11 ct
    showsPrec n (CTCompose ct1 ct2) = ptrans (n > 10) parens $ (showsPrec 11 ct1) <+> char '.' <+> (showsPrec 11 ct2)


ptrans b f = if b then f else id

instance Monoid CoerceTerm where
    mempty = CTId
    mappend = composeCoerce

ctFun CTId = CTId
ctFun x = CTFun x
ctAbs [] = CTId
ctAbs xs = CTAbs xs
ctAp [] = CTId
ctAp xs = CTAp xs
ctId = CTId

composeCoerce :: CoerceTerm -> CoerceTerm -> CoerceTerm
--composeCoerce (CTFun a) (CTFun b) = ctFun (a `composeCoerce` b)
composeCoerce CTId x = x
composeCoerce x CTId = x
--composeCoerce (CTAbs ts) (CTAbs ts') = CTAbs (ts ++ ts')
--composeCoerce (CTAp ts) (CTAp ts') = CTAp (ts ++ ts')
--composeCoerce (CTAbs ts) (CTAp ts') = f ts ts' where
--    f (t:ts) (TVar t':ts') | t == t' = f ts ts'
--    f [] [] = CTId
--    f _ _ = CTCompose (CTAbs ts) (CTAp ts')
composeCoerce x y = CTCompose x y


instance UnVar Type => UnVar CoerceTerm where
    unVar' opt (CTAp ts) = CTAp `liftM` unVar' opt ts
    unVar' opt (CTFun ct) = CTFun `liftM` unVar' opt ct
    unVar' opt (CTCompose c1 c2) = liftM2 CTCompose (unVar' opt c1) (unVar' opt c2)
    unVar' _ x = return x




