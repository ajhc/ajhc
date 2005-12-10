module FrontEnd.Tc.Monad(
    addPreds,
    addToCollectedEnv,
    freshInstance,
    freshSigma,
    getKindEnv,
    inst,
    localEnv,
    lookupName,
    newBox,
    newTVar,
    runTc,
    Tc(),
    TcInfo(..),
    tcInfoEmpty,
    TypeEnv(),
    unify,
    varBind,
    skolomize,
    boxySpec,
    boxyInstantiate,
    unifyList,
    generalize,
    withContext
    ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans
import Control.Monad.Error
import Data.IORef
import Data.Monoid
import List
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJ(Doc)


import Atom
import Class(ClassHierarchy)
import Diagnostic
import Doc.PPrint
import FrontEnd.KindInfer
import FrontEnd.SrcLoc(bogusASrcLoc)
import FrontEnd.Tc.Type
import GenUtil
import Name.Name
import Options
import Warning

type TypeEnv = Map.Map Name Sigma

-- read only environment, set up before type checking.
data TcEnv = TcEnv {
    tcInfo              :: TcInfo,
    tcDiagnostics       :: [Diagnostic],   -- list of information that might help diagnosis
    tcVarnum            :: IORef Int,
    -- Used by new typechecker only
    tcCollectedEnv      :: IORef (Map.Map Name Sigma),
    tcCurrentEnv        :: Map.Map Name Sigma,
    tcOptions           :: Opt  -- module specific options
    }
   {-! derive: update !-}

newtype Tc a = Tc (ReaderT TcEnv (WriterT [Pred] IO) a)
    deriving(MonadFix,MonadIO,MonadReader TcEnv,MonadWriter [Pred],Functor)

-- | information that is passed into the type checker.
data TcInfo = TcInfo {
    tcInfoEnv :: TypeEnv, -- initial typeenv, data constructors, and previously infered types
    tcInfoSigEnv :: TypeEnv, -- type signatures used for binding analysis
    tcInfoModName :: String,
    tcInfoKindInfo :: KindEnv,
    tcInfoClassHierarchy :: ClassHierarchy
    }

-- | run a computation with a local environment
localEnv :: TypeEnv -> Tc a -> Tc a
localEnv te = local (tcCurrentEnv_u (te `Map.union`))

-- | add to the collected environment which will be used to annotate uses of variables with their instantiated types.
-- should contain @-aliases for each use of a polymorphic variable or pattern match.

addToCollectedEnv :: TypeEnv -> Tc ()
addToCollectedEnv te = do
    v <- asks tcCollectedEnv
    liftIO $ modifyIORef v (te `Map.union`)


runTc :: (MonadIO m,OptionMonad m) => TcInfo -> Tc a -> m a
runTc tcInfo  (Tc tim) = do
    opt <- getOptions
    liftIO $ do
    vn <- newIORef 0
    ce <- newIORef mempty
    (a,out) <- runWriterT $ runReaderT tim TcEnv {
        tcCollectedEnv = ce,
        tcCurrentEnv = tcInfoEnv tcInfo,
        tcVarnum = vn,
        tcDiagnostics = [Msg Nothing $ "Compilation of module: " ++ tcInfoModName tcInfo],
        tcInfo = tcInfo,
        tcOptions = opt
        }
    return a

instance OptionMonad Tc where
    getOptions = asks tcOptions


-- | given a diagnostic and a computation to take place inside the TI-monad,
--   run the computation but during it have the diagnostic at the top of the
--   stack

withContext :: Diagnostic -> Tc a -> Tc a
withContext diagnostic comp = do
    local (tcDiagnostics_u (diagnostic:)) comp


getErrorContext :: Tc [Diagnostic]
getErrorContext = asks tcDiagnostics

getClassHierarchy  :: Tc ClassHierarchy
getClassHierarchy = asks (tcInfoClassHierarchy . tcInfo)

getKindEnv :: Tc KindEnv
getKindEnv = asks (tcInfoKindInfo . tcInfo)

getSigEnv :: Tc TypeEnv
getSigEnv = asks (tcInfoSigEnv . tcInfo)

getModName :: Tc String
getModName = asks ( tcInfoModName . tcInfo)



dConScheme :: Name -> Tc Sigma
dConScheme conName = do
    env <- asks tcCurrentEnv
    case Map.lookup conName env of
        Just s -> return s
        Nothing -> error $ "dConScheme: constructor not found: " ++ show conName ++
                              "\nin this environment:\n" ++ show env


unify      :: Tau -> Tau -> Tc ()
unify t1 t2 = do
    t1' <- findType t1
    t2' <- findType t2
    b <- mgu t1' t2'
    case b of
        Nothing -> return ()
        Just err -> do
                  diagnosis <- getErrorContext
                  typeError (Unification $ "attempted to unify " ++
                                           pretty t1' ++
                                           " with " ++
                                           pretty t2' ++ "\n" ++ err)
                            diagnosis

unifyList :: [Type] -> Tc ()
unifyList (t1:t2:ts) = unify t1 t2 >> unifyList (t2:ts)
unifyList _ = return ()


-- | returns a new box and a function to read said box.

newBox :: Kind -> Tc (Tc Type,Type)
newBox k = do
    u <- newUniq
    r <- liftIO $ newIORef (error "empty box")
    return (liftIO $ readIORef r >>= flattenMetaVars, TBox k u r)

mgu     :: (MonadIO m) => Type -> Type -> m (Maybe String)

mgu x y = do
    r <- runErrorT (mgu'' x y)
    case r of
        Right _ -> return Nothing
        Left (err::String) -> return (Just err)
mgu'' x y = do
    x' <- findType x
    y' <- findType y
    mgu' x' y'
mgu' (TAp l r) (TAp l' r')
   = do s1 <- mgu'' l l'
        s2 <- mgu'' r r'
        return ()
mgu' (TArrow l r) (TArrow l' r')
   = do s1 <- mgu'' l l'
        s2 <- mgu'' r r'
        return ()
mgu' (TVar u) t | isMetaTV u  = varBind u t
mgu' t (TVar u) | isMetaTV u  = varBind u t
mgu' (TVar a) (TVar b) | a == b = return ()
mgu' c1@(TCon tc1) c2@(TCon tc2)
           | tc1==tc2 = return ()
           | otherwise = fail $ "mgu: Constructors don't match:" ++ show (c1,c2)
mgu' TForAll {} _ = error "attempt to unify TForall"
mgu' _ TForAll {} = error "attempt to unify TForall"
mgu' _ TBox {} = error "attempt to unify TBox"
mgu' TBox {} _ = error "attempt to unify TBox"
mgu' t1 t2  = fail $ "mgu: types do not unify:" ++ show (t1,t2)

{-

unify      :: Type -> Type -> TI ()
unify t1 t2 = do
    t1' <- findType t1
    t2' <- findType t2
    b <- mgu t1' t2'
    case b of
        Just u -> return () -- extSubst u
        Nothing -> do
                  diagnosis <- getErrorContext
                  typeError (Unification $ "attempted to unify " ++
                                           pretty t1' ++
                                           " with " ++
                                           pretty t2')
                            diagnosis

unifyList :: [Type] -> TI ()
unifyList [] = return ()
unifyList [_] = return ()
unifyList (t1:t2:ts) = do
       unify t1 t2
       unifyList (t2:ts)

-}


lookupName :: Name -> Tc Sigma
lookupName n = do
    env <- asks tcCurrentEnv
    case Map.lookup n env of
        Just x -> return x
        Nothing -> fail $ "Could not find var in tcEnv:" ++ show (nameType n,n)


newTVar    :: Kind -> Tc Type
newTVar k   = do
    te <- ask
    n <- newUniq
    r <- liftIO $ newIORef Nothing
    let ident = toName TypeVal ('v':show n)
        v = tyvar ident k (Just r)
    return $ TVar v


class Instantiate a where
    inst:: Map.Map Atom Type -> a -> a


instance Instantiate Type where
    inst ts (TAp l r)     = TAp (inst ts l) (inst ts r)
    inst ts (TArrow l r)  = TArrow (inst ts l) (inst ts r)
    inst ts t@TGen {}     = error $ "inst TGen " ++ show (ts,t)
    inst  _ t@TCon {}     = t
    inst ts (TVar tv )
        | Nothing == tyvarRef tv  = t'  where Just t' = Map.lookup (tyvarAtom tv) ts
    inst  _ t@TVar {}     = t
    inst _ TBox {}        = error "instantiating something with a box"
    inst ts (TForAll as qt) = TForAll as (inst (foldr Map.delete ts (map tyvarAtom as)) qt)

    --inst ts t@(TGen n tv) | Just t <- Map.lookup (tyvarAtom tv) ts = t
    --                      | otherwise = error $ "inst TGen " ++ show (ts,t)

instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)

instance Instantiate t => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t

instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c (inst ts t)


freshInst :: Sigma -> Tc (Qual Type)
freshInst (TForAll as qt) = do
        ts <- mapM newTVar (map tyvarKind as)
        return (inst (Map.fromList $ zip (map tyvarAtom as) ts) qt)
freshInst x = return ([] :=> x)

freshInstance :: Sigma -> Tc Rho
freshInstance (TForAll as qt) = do
    ts <- mapM newTVar (map tyvarKind as)
    let (ps :=> t) = (inst (Map.fromList $ zip (map tyvarAtom as) ts) qt)
    addPreds ps
    return t
freshInstance x = return x

addPreds :: [Pred] -> Tc ()
addPreds ps = Tc $ tell ps

newVar :: Kind -> Tc Tyvar
newVar k = do
    te <- ask
    n <- newUniq
    let ident = toName TypeVal (tcInfoModName $ tcInfo te,'v':show n)
        v = tyvar ident k Nothing
    return v

-- rename the bound variables of a sigma, just in case.
freshSigma :: Sigma -> Tc Sigma
freshSigma (TForAll [] ([] :=> t)) = return t
freshSigma (TForAll vs qt) = do
    nvs <- mapM (newVar . tyvarKind) vs
    return (TForAll nvs $ inst (Map.fromList $ zip (map tyvarAtom vs) (map TVar nvs)) qt)
freshSigma x = return x

-- | replace bound variables with arbitrary new ones and drop the binding
-- TODO predicates?

skolomize :: Sigma' -> Tc ([SkolemTV],Rho')
skolomize s@TForAll {} = freshSigma s >>= \x -> case x of
    TForAll as (_ :=> r) -> return (as,r)
    r -> return ([],r)
skolomize s = return ([],s)

boxyInstantiate :: Sigma -> Tc Rho'
boxyInstantiate (TForAll as qt) = do
        bs <- mapM (newBox . tyvarKind) as
        let mp = Map.fromList $ zip (map tyvarAtom as) (snds bs)
            (ps :=> r) = inst mp qt
        addPreds ps
        return r
boxyInstantiate x = return x

boxySpec :: Sigma -> Tc ([(BoundTV,[Sigma'])],Rho')
boxySpec (TForAll as qt@(ps :=> t)) = do
    let f (TVar t) vs | t `elem` vs = do
            (_,b) <- lift (newBox $ tyvarKind t)
            tell [(t,b)]
            return b
        f e@TCon {} _ = return e
        f (TAp a b) vs = liftM2 TAp (f a vs) (f b vs)
        f (TArrow a b) vs = liftM2 TArrow (f a vs) (f b vs)
        f (TForAll as (ps :=> t)) vs = do
            t' <- f t (vs List.\\ as)
            return (TForAll as (ps :=> t'))
        f t _ = return t
        -- f t _ = error $ "boxySpec: " ++ show t
    (t',vs) <- runWriterT (f t as)
    addPreds $ inst (Map.fromList [ (tyvarAtom bt,s) | (bt,s) <- vs ]) ps
    return (sortGroupUnderFG fst snd vs,t')


generalize :: Rho -> Tc Sigma
generalize r = do
    let mtvs = freeMetaVars r
    nvs <- mapM (newVar . tyvarKind) [ t | t <- mtvs]
    sequence_ [ varBind mv (TVar v) | v <- nvs |  mv <- mtvs ]
    r <- flattenMetaVars r
    return $ TForAll nvs ([] :=> r)

varBind :: (MonadIO m) => Tyvar -> Type -> m ()
varBind u t | not (isMetaTV u) = error "varBind: not metatv"
            | t == TVar u   = return ()
            | u `elem` freeMetaVars t = fail "varBind: occurs check fails"
            | kind u == kind t, Just r <- tyvarRef u = do
                x <- liftIO $ readIORef r
                case x of
                    Just r -> fail $ "varBind: bining unfree: " ++ show (u,t,r)
                    Nothing -> liftIO $ writeIORef r (Just t)
            | otherwise        = error $ "varBind: kinds do not match:" ++ show (u,t)

----------------------------------------
-- Declaration of instances, boilerplate
----------------------------------------

pretty  :: PPrint Doc a => a -> String
pretty x  = show (pprint x :: Doc)

instance Monad Tc where
    return a = Tc $ return a
    Tc comp >>= fun = Tc $ do x <- comp; case fun x of Tc m -> m
    Tc a >> Tc b = Tc $ a >> b
    fail s = Tc $ do
        st <- ask
        liftIO $ processIOErrors
        liftIO $ fail s -- typeError (Failure s) (tcDiagnostics st)

instance MonadWarn Tc where
    addWarning w = liftIO $ processErrors [w]

instance MonadSrcLoc Tc where
    getSrcLoc = do
        xs <- asks tcDiagnostics
        case xs of
            (Msg (Just sl) _:_) -> return sl
            _ -> return bogusASrcLoc

instance UniqueProducer Tc where
    newUniq = do
        v <- asks tcVarnum
        n <- liftIO $ do
            n <- readIORef v
            writeIORef v $! n + 1
            return n
        return n

tcInfoEmpty = TcInfo {
    tcInfoEnv = mempty,
    tcInfoModName = "(unknown)",
    tcInfoKindInfo = mempty,
    tcInfoClassHierarchy = mempty,
    tcInfoSigEnv = mempty
}


