{-# OPTIONS -fglasgow-exts #-}

{-------------------------------------------------------------------------------

        Copyright:              Mark Jones and The Hatchet Team
                                (see file Contributors)

        Module:                 TIMonad

        Description:            A monad to support type inference, in
                                particular for threading the type environment
                                through the type inference code.

        Primary Authors:        Mark Jones, Bernie Pope and Bryn Humberstone

        Notes:                  See the file License for license information

                                Large parts of this module were derived from
                                the work of Mark Jones' "Typing Haskell in
                                Haskell", (http://www.cse.ogi.edu/~mpj/thih/)

-------------------------------------------------------------------------------}

module TIMonad (TI,
                inst,
                runTI,
                withContext,
                getClassHierarchy,
                getSigEnv,
                getKindEnv,
                unify,
                freshInst,
                dConScheme,
                unifyList,
                getModName,
                newTVar
                ) where


import Control.Monad.Reader
import Control.Monad.Trans
import Data.IORef
import Monad
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJ(render,Doc())

import Class(ClassHierarchy())
import Diagnostic
import Doc.PPrint(PPrint(..))
import FrontEnd.KindInfer(KindEnv())
import FrontEnd.SrcLoc
import FrontEnd.Utils()
import HsSyn
import Name.Name
import Representation
import Type(Instantiate (..), mgu)
import TypeSigs(SigEnv)
import Warning
import Options

--------------------------------------------------------------------------------



-- read only environment, set up before type checking.
data TcEnv = TcEnv {
      tcClassHierarchy    :: ClassHierarchy,
      tcKinds             :: KindEnv,
      tcModuleName        :: Module,
      tcDiagnostics       :: [Diagnostic],   -- list of information that might help diagnosis
      tcVarnum            :: IORef Int,
      tcDConsEnv          :: Map.Map Name Scheme,
      tcSigs              :: SigEnv,
      tcOptions           :: Opt
    }
   {-! derive: update !-}



newtype TI a = TI (ReaderT TcEnv IO a)
    deriving(MonadIO,MonadReader TcEnv,Functor)

instance Monad TI where
    return a = TI $ return a
    TI comp >>= fun = TI $ do x <- comp; case fun x of TI m -> m
    TI a >> TI b = TI $ a >> b
    fail s = TI $ do
        st <- ask
        liftIO $ printIOErrors
        liftIO $ typeError (Failure s) (tcDiagnostics st)

instance MonadWarn TI where
    addWarning w = liftIO $ processErrors [w]

instance MonadSrcLoc TI where
    getSrcLoc = do
        xs <- asks tcDiagnostics
        case xs of
            (Msg (Just sl) _:_) -> return sl
            _ -> return bogusASrcLoc

instance OptionMonad TI where
    getOptions = asks tcOptions

runTI :: Opt -> Map.Map Name Scheme-> ClassHierarchy -> KindEnv -> SigEnv -> Module -> TI a -> IO a
runTI opt  env' ch' kt' st' mod' (TI tim) = do
    vn <- newIORef 0
    runReaderT tim tcenv {  tcVarnum = vn } where
    tcenv = TcEnv {
        tcClassHierarchy = ch',
        tcKinds = kt',
        tcModuleName = mod',
        tcSigs = st',
        tcVarnum = undefined,
        tcDConsEnv = env',
        tcOptions = opt,
        tcDiagnostics = [Msg Nothing $ "Compilation of module: " ++ fromModule mod']
        }



{- given a diagnostic and a computation to take place inside the TI-monad,
   run the computation but during it have the diagnostic at the top of the
   stack -}
{-# INLINE withContext #-}
{-# INLINE tcDiagnostics_u #-}
withContext :: Diagnostic -> TI a -> TI a
withContext diagnostic comp = do
    local (tcDiagnostics_u (diagnostic:)) comp


getErrorContext :: TI [Diagnostic]
getErrorContext = asks tcDiagnostics



getDConsTypeEnv :: TI (Map.Map Name Scheme)
getDConsTypeEnv = asks tcDConsEnv

getClassHierarchy  :: TI ClassHierarchy
getClassHierarchy = asks tcClassHierarchy

getKindEnv :: TI (KindEnv)
getKindEnv = asks tcKinds

getSigEnv :: TI SigEnv
getSigEnv = asks tcSigs

getModName :: TI Module
getModName = asks tcModuleName



dConScheme :: Name -> TI Scheme
dConScheme conName
   = do
        env <- getDConsTypeEnv
        case Map.lookup conName env of
           Nothing -> error $ "dConScheme: constructor not found: " ++ show conName ++
                              "\nin this environment:\n" ++ show env
           Just s -> return s

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


extSubst   :: Subst -> TI ()
extSubst s = sequence_ [ do y' <- findType y ; liftIO $ writeIORef r (Just y') | (Tyvar { tyvarRef = ~(Just r)} ,y) <- Map.toList s]

newTVar    :: Kind -> TI Type
newTVar k   = do
    te <- ask
    n <- liftIO $ readIORef (tcVarnum te)
    r <- liftIO $ newIORef Nothing
    --let ident = Qual (tcModuleName te) $ HsIdent $ "v" ++ show n
    let ident = toName TypeVal (show $ tcModuleName te,'v':show n)
        v = tyvar ident k (Just r)
    liftIO $ writeIORef (tcVarnum te) $! n + 1
    return $ TVar v


freshInst :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do
        ts <- mapM newTVar ks
        let v = (inst ts qt)
        return (v)


pretty  :: PPrint Doc a => a -> String
pretty   = render . pprint

