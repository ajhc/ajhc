module FrontEnd.Tc.Monad(
    TypeEnv(),
    Tc(),
    localEnv,
    addToCollectedEnv,
    tcInfoEmpty,
    runTc,
    lookupName,
    freshInst,
    unify,
    TcInfo(..)
    ) where

import Control.Monad.Reader
import Control.Monad.Trans
import Data.IORef
import Data.Monoid
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJ(Doc)


import Atom
import Class(ClassHierarchy)
import Diagnostic
import Doc.PPrint
import FrontEnd.KindInfer
import GenUtil
import FrontEnd.SrcLoc(bogusASrcLoc)
import Type(mgu)
import Name.Name
import Options(Opt,options)
import Representation
import Warning

type TypeEnv = Map.Map Name Sigma

-- read only environment, set up before type checking.
data TcEnv = TcEnv {
    tcInfo              :: TcInfo,
    tcDiagnostics       :: [Diagnostic],   -- list of information that might help diagnosis
    tcVarnum            :: IORef Int,
    -- Used by new typechecker only
    tcCollectedEnv      :: IORef (Map.Map Name Sigma),
    tcCurrentEnv        :: Map.Map Name Sigma
    }
   {-! derive: update !-}

newtype Tc a = Tc (ReaderT TcEnv IO a)
    deriving(MonadFix,MonadIO,MonadReader TcEnv,Functor)

-- | information that is passed into the type checker.
data TcInfo = TcInfo {
    tcInfoEnv :: TypeEnv, -- initial typeenv, data constructors, and previously infered types
    tcInfoSigEnv :: TypeEnv, -- type signatures used for binding analysis
    tcInfoModName :: String,
    tcInfoKindInfo :: KindEnv,
    tcInfoClassHierarchy :: ClassHierarchy,
    tcInfoOptions :: Opt  -- module specific options
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


runTc :: TcInfo -> Tc a -> IO a
runTc tcInfo  (Tc tim) = do
    vn <- newIORef 0
    ce <- newIORef mempty
    runReaderT tim TcEnv {
        tcCollectedEnv = ce,
        tcCurrentEnv = tcInfoEnv tcInfo,
        tcVarnum = vn,
        tcDiagnostics = [Msg Nothing $ "Compilation of module: " ++ tcInfoModName tcInfo],
        tcInfo = tcInfo
        }


{-
runTI :: Map.Map Name Scheme-> ClassHierarchy -> KindEnv -> SigEnv -> Module -> TI a -> IO a
runTI env' ch' kt' st' mod' (TI tim) = do
    vn <- newIORef 0
    runReaderT tim tcenv {  tcVarnum = vn } where
    tcenv = TcEnv {
        tcClassHierarchy = ch',
        tcKinds = kt',
        tcModuleName = mod',
        tcSigs = st',
        tcVarnum = undefined,
        tcDConsEnv = env',
        tcDiagnostics = [Msg Nothing $ "Compilation of module: " ++ fromModule mod']
        }
-}

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
    env <- asks ( tcInfoEnv . tcInfo)
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
        Just u -> return () -- extSubst u
        Nothing -> do
                  diagnosis <- getErrorContext
                  typeError (Unification $ "attempted to unify " ++
                                           pretty t1' ++
                                           " with " ++
                                           pretty t2')
                            diagnosis

unifyList :: [Type] -> Tc ()
unifyList (t1:t2:ts) = unify t1 t2 >> unifyList (t2:ts)
unifyList _ = return ()

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
    env <- asks (tcInfoEnv . tcInfo)
    case Map.lookup n env of
        Just x -> return x
        Nothing -> fail $ "Could not find var in tcEnv:" ++ show (nameType n,n)


newTVar    :: Kind -> Tc Type
newTVar k   = do
    te <- ask
    n <- newUniq
    r <- liftIO $ newIORef Nothing
    let ident = toName TypeVal (tcInfoModName $ tcInfo te,'v':show n)
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


{-
freshInst :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do
        ts <- mapM newTVar ks
        let v = (inst ts qt)
        return (v)
-}



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
        liftIO $ typeError (Failure s) (tcDiagnostics st)

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
    tcInfoSigEnv = mempty,
    tcInfoOptions = options
}
