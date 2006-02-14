module FrontEnd.Tc.Monad(
    addPreds,
    addToCollectedEnv,
    getCollectedEnv,
    boxyInstantiate,
    boxySpec,
    freshInstance,
    toSigma,
    freshSigma,
    getClassHierarchy,
    getKindEnv,
    getSigEnv,
    getModName,
    localEnv,
    withMetaVars,
    quantify,
    lookupName,
    newBox,
    unBox,
    newMetaVar,
    newVar,
    runTc,
    skolomize,
    Tc(),
    TcInfo(..),
    tcInfoEmpty,
    TypeEnv(),
    inst,
    unificationError,
    freeMetaVarsEnv,
    varBind,
    withContext
    ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Error
import Data.IORef
import Data.Monoid
import Data.FunctorM
import List
import Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.PrettyPrint.HughesPJ(Doc)


import Atom
import Class(ClassHierarchy,simplify)
import Diagnostic
import Doc.DocLike
import Doc.PPrint
import FrontEnd.KindInfer
import FrontEnd.SrcLoc(bogusASrcLoc)
import FrontEnd.Tc.Type
import Options
import qualified FlagDump as FD
import GenUtil
import Name.Name
import Options
import Support.CanType
import Util.Inst
import Warning

type TypeEnv = Map.Map Name Sigma

-- read only environment, set up before type checking.
data TcEnv = TcEnv {
    tcInfo              :: TcInfo,
    tcDiagnostics       :: [Diagnostic],   -- list of information that might help diagnosis
    tcVarnum            :: IORef Int,
    tcCollectedEnv      :: IORef (Map.Map Name Sigma),
    tcCurrentEnv        :: Map.Map Name Sigma,
    tcCurrentScope      :: Set.Set MetaVar,
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
localEnv te | isGood = local (tcCurrentEnv_u (te `Map.union`)) where
    isGood = not $ any isBoxy (Map.elems te)
localEnv te = fail $ "localEnv error!\n" ++ show te

-- | run a computation with a local environment
--localScopeEnv :: [Tyvar] -> Tc a -> Tc a
--localScopeEnv te = local (tcCurrentScope_u (te `Set.union`))

-- | add to the collected environment which will be used to annotate uses of variables with their instantiated types.
-- should contain @-aliases for each use of a polymorphic variable or pattern match.

addToCollectedEnv :: TypeEnv -> Tc ()
addToCollectedEnv te = do
    v <- asks tcCollectedEnv
    liftIO $ modifyIORef v (te `Map.union`)

getCollectedEnv :: Tc TypeEnv
getCollectedEnv = do
    v <- asks tcCollectedEnv
    r <- liftIO $ readIORef v
    r <- fmapM flattenType r
    return r


runTc :: (MonadIO m,OptionMonad m) => TcInfo -> Tc a -> m a
runTc tcInfo  (Tc tim) = do
    opt <- getOptions
    liftIO $ do
    vn <- newIORef 0
    ce <- newIORef mempty
    (a,out) <- runWriterT $ runReaderT tim TcEnv {
        tcCollectedEnv = ce,
        tcCurrentEnv = tcInfoEnv tcInfo `mappend` tcInfoSigEnv tcInfo,
        tcVarnum = vn,
        tcDiagnostics = [Msg Nothing $ "Compilation of module: " ++ tcInfoModName tcInfo],
        tcInfo = tcInfo,
        tcCurrentScope = mempty,
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



-- | returns a new box and a function to read said box.

newBox :: Kind -> Tc Type
newBox k = newMetaVar Sigma k

throwError s t1 t2 = do
    diagnosis <- getErrorContext
    typeError (Unification $ "attempted to unify " ++ prettyPrintType t1 ++ " with " ++ prettyPrintType t2) diagnosis


unificationError t1 t2 = do
    diagnosis <- getErrorContext
    typeError (Unification $ "attempted to unify " ++ prettyPrintType t1 ++ " with " ++ prettyPrintType t2) diagnosis


lookupName :: Name -> Tc Sigma
lookupName n = do
    env <- asks tcCurrentEnv
    case Map.lookup n env of
        Just x -> freshSigma x
        Nothing -> fail $ "Could not find var in tcEnv:" ++ show (nameType n,n)


newMetaVar :: MetaVarType -> Kind -> Tc Type
newMetaVar t k = do
    te <- ask
    n <- newUniq
    r <- liftIO $ newIORef Nothing
    return $ TMetaVar MetaVar { metaUniq = n, metaKind = k, metaRef = r, metaType = t }


class Instantiate a where
    inst:: Map.Map Int Type -> Map.Map Atom Type -> a -> a

instance Instantiate Type where
    inst mm ts (TAp l r)     = TAp (inst mm ts l) (inst mm ts r)
    inst mm ts (TArrow l r)  = TArrow (inst mm ts l) (inst mm ts r)
    inst mm  _ t@TCon {}     = t
    inst mm ts (TVar tv )
        | Nothing == tyvarRef tv  = case Map.lookup (tyvarAtom tv) ts of
            Just t'  -> t'
            Nothing -> (TVar tv)
    inst mm ts (TForAll as qt) = TForAll as (inst mm (foldr Map.delete ts (map tyvarAtom as)) qt)
    inst mm ts (TMetaVar mv) | Just t <- Map.lookup (metaUniq mv) mm  = t
    inst mm ts (TMetaVar mv) = TMetaVar mv
    inst mm _ t = error $ "inst: " ++ show t


instance Instantiate a => Instantiate [a] where
  inst mm ts = map (inst mm ts)

instance Instantiate t => Instantiate (Qual t) where
  inst mm ts (ps :=> t) = inst mm ts ps :=> inst mm ts t

instance Instantiate Pred where
  inst mm ts (IsIn c t) = IsIn c (inst mm ts t)


freshInstance :: MetaVarType -> Sigma -> Tc Rho
freshInstance typ (TForAll as qt) = do
    ts <- mapM (newMetaVar typ) (map tyvarKind as)
    let (ps :=> t) = (inst mempty (Map.fromList $ zip (map tyvarAtom as) ts) qt)
    addPreds ps
    return t
freshInstance _ x = return x

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
    return (TForAll nvs $ inst mempty (Map.fromList $ zip (map tyvarAtom vs) (map TVar nvs)) qt)
freshSigma x = return x

toSigma :: Sigma -> Sigma
toSigma t@TForAll {} = t
toSigma t = TForAll [] ([] :=> t)

-- | replace bound variables with arbitrary new ones and drop the binding
-- TODO predicates?

skolomize :: Sigma' -> Tc ([SkolemTV],Preds,Rho')
skolomize (TForAll vs (ps :=> rho)) = return (vs,ps,rho)
--freshSigma s >>= \x -> case x of
--    TForAll as (_ :=> r) -> return (as,r)
--    r -> return ([],r)
skolomize s = return ([],[],s)

boxyInstantiate :: Sigma -> Tc Rho'
boxyInstantiate = freshInstance Sigma

boxySpec :: Sigma -> Tc ([(BoundTV,[Sigma'])],Rho')
boxySpec (TForAll as qt@(ps :=> t)) = do
    let f (TVar t) vs | t `elem` vs = do
            b <- lift (newBox $ tyvarKind t)
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
    addPreds $ inst mempty (Map.fromList [ (tyvarAtom bt,s) | (bt,s) <- vs ]) ps
    return (sortGroupUnderFG fst snd vs,t')




freeMetaVarsEnv :: Tc (Set.Set MetaVar)
freeMetaVarsEnv = do
    env <- asks tcCurrentEnv
    xs <- flip mapM (Map.elems env)  $ \ x -> do
        x <- flattenType x
        return $ freeMetaVars x
    return (Set.fromList $ concat xs)

quantify :: [MetaVar] -> [Pred] -> Rho -> Tc Sigma
quantify vs ps r | not $ any isBoxyMetaVar vs = do
    r <- flattenType r
    nvs <- mapM (newVar . metaKind) vs
    sequence_ [ varBind mv (TVar v) | v <- nvs |  mv <- vs ]
    (ps :=> r) <- flattenType (ps :=> r)
    ch <- getClassHierarchy
    return $ TForAll nvs (Class.simplify ch ps :=> r)


-- this removes all boxes, replacing them with tau vars
unBox ::  Type -> Tc Type
unBox tv = ft' tv where
    ft (TAp x y) = liftM2 TAp (ft' x) (ft' y)
    ft (TArrow x y) = liftM2 TArrow (ft' x) (ft' y)
    ft t@TCon {} = return t
    ft (TForAll vs (ps :=> t)) = do
        when (any isMetaTV vs) $ error "metatv in forall binding"
        ps' <- sequence [ ft' t >>= return . IsIn c | ~(IsIn c t) <- ps ]
        t' <- ft' t
        return $ TForAll vs (ps' :=> t')
    ft t@(TMetaVar mv)
        | isBoxyMetaVar mv = do
            tmv <- newMetaVar Tau (getType mv)
            varBind mv tmv
            return tmv
        | otherwise =  return t
    ft t | ~(Just tv) <- extractTyVar t  = return (TVar tv)
    ft' t = findType t >>= ft


-- Bind mv to type, first filling in any boxes in type with tau vars
varBind :: MetaVar -> Type -> Tc ()
varBind u t
    | getType u /= getType t = error $ "varBind: kinds do not match:" ++ show (u,t)
    | otherwise = do
        tt <- unBox t
        --(t,be,_) <- unbox t
        --when be $ error $ "binding boxy: " ++ tupled [pprint u,prettyPrintType t]
        when (u `elem` freeMetaVars tt) $ unificationError (TMetaVar u) tt -- occurs check
        let r = metaRef u
        x <- liftIO $ readIORef r
        case x of
            Just r -> fail $ "varBind: binding unfree: " ++ tupled [pprint u,prettyPrintType tt,prettyPrintType r]
            Nothing -> liftIO $ do
                when (dump FD.BoxySteps) $ putStrLn $ "varBind: " ++ pprint u <+> prettyPrintType t
                writeIORef r (Just tt)




zonkBox :: MetaVar -> Tc Type
zonkBox mv | isBoxyMetaVar mv = findType (TMetaVar mv)
zonkBox mv = fail $ "zonkBox: nonboxy" ++ show mv

readFilledBox :: MetaVar -> Tc Type
readFilledBox mv | isBoxyMetaVar mv = zonkBox mv >>= \v -> case v of
    TMetaVar mv' | mv == mv' -> fail $ "readFilledBox: " ++ show mv
    t -> return t
readFilledBox mv = error $ "readFilledBox: nonboxy" ++ show mv

elimBox :: MetaVar -> Tc Type
elimBox mv | isBoxyMetaVar mv = do
    t <- readMetaVar mv
    case t of
        Just t -> return t
        Nothing -> newMetaVar Tau (getType mv)

elimBox mv = error $ "elimBox: nonboxy" ++ show mv



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
        Left x <- typeError (Failure s) (tcDiagnostics st)
        liftIO $ fail x

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


withMetaVars :: MetaVar -> [Kind] -> ([Sigma] -> Sigma) -> ([Sigma'] -> Tc a) -> Tc a
withMetaVars mv ks sfunc bsfunc | isBoxyMetaVar mv = do
    boxes <- mapM newBox ks
    res <- bsfunc boxes
    tys <- mapM readFilledBox [ mv | ~(TMetaVar mv) <- boxes]
    varBind mv (sfunc tys)
    return res
withMetaVars mv ks sfunc bsfunc  = do
    taus <- mapM (newMetaVar Tau) ks
    varBind mv (sfunc taus)
    bsfunc taus

