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
                getErrorContext,
                withContext,
                getClassHierarchy,
                getKindEnv,
                getSigEnv,
                unify,
                freshInst,
                dConScheme,
                unifyList,
                getModName,
                newTVar) where


import Control.Monad.Trans
import Data.IORef
import Monad
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJ(render,Doc)

import Class                 (ClassHierarchy)
import Diagnostic
import Doc.PPrint(pprint,PPrint)
import HsSyn
import FrontEnd.KindInfer             (KindEnv)
import Representation
import TypeSigs              (SigEnv)
import Type                  (Instantiate (..), mgu)
import Utils()
import Warning

--------------------------------------------------------------------------------



-- read only environment, set up before type checking.
data TcEnv = TcEnv {
      tcClassHierarchy    :: ClassHierarchy,
      tcKinds             :: KindEnv,
      tcModuleName        :: Module,
      tcDiagnostics       :: [Diagnostic],   -- list of information that might help diagnosis
      tcVarnum            :: IORef Int,
      -- tcSubst             :: IORef Subst,
      tcDConsEnv          :: Map.Map HsName Scheme,
      tcSigs              :: SigEnv
    }
   {-! derive: update !-}


newtype TI a = TI (TcEnv -> IO a)

instance MonadIO TI where
    liftIO x = TI (\_ -> x)

--instance MonadReader TcEnv TI  where
{-# INLINE ask #-}
{-# INLINE local #-}
{-# INLINE asks #-}

ask = TI (\t -> return t)
local f (TI c) = TI (\t -> c (f t))
asks f = liftM f ask

instance Monad TI where
    {-# INLINE return #-}
    {-# INLINE (>>=) #-}
    {-# INLINE (>>) #-}
    return a = TI (\_ -> return a)
    TI comp >>= fun = TI (\t -> comp t >>= \x -> case fun x of
        TI r -> r t)
    TI a >> TI b = TI (\t -> a t >> b t)
    fail s = TI $ \st -> do
        processIOErrors
        typeError (Failure s) (tcDiagnostics st)

instance Functor TI where
    fmap = liftM

runTI     :: Map.Map HsName Scheme-> ClassHierarchy -> KindEnv -> SigEnv -> Module -> TI a -> IO a
runTI env' ch' kt' st' mod' (TI c) = do
    vn <- newIORef 0
    -- sub <- newIORef nullSubst
    c tcenv {  tcVarnum = vn } where
    tcenv = TcEnv {
        tcClassHierarchy = ch',
        tcKinds = kt',
        tcModuleName = mod',
        tcSigs = st',
        tcVarnum = undefined,
        -- tcSubst = undefined,
        tcDConsEnv = env',
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


--getSubst :: TI Subst
--getSubst = TI $ \t -> readIORef (tcSubst t) -- gets subst

getDConsTypeEnv :: TI (Map.Map HsName Scheme)
getDConsTypeEnv = TI $ \t -> return (tcDConsEnv t) -- gets env

getClassHierarchy  :: TI ClassHierarchy
getClassHierarchy = asks tcClassHierarchy

getKindEnv :: TI (KindEnv)
getKindEnv = asks tcKinds

getSigEnv :: TI SigEnv
getSigEnv = asks tcSigs

getModName :: TI Module
getModName = asks tcModuleName



dConScheme :: HsName -> TI Scheme
dConScheme conName
   = do
        env <- getDConsTypeEnv
        case Map.lookup conName env of
           Nothing
            --  | Just n <- fromTupname conName -> return (toTuple n)
            | otherwise -> error $ "dConScheme: constructor not found: " ++ show conName ++
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

{-
--s <- getSubst
--let t1' = apply s t1
--    t2' = apply s t2

                 case mgu t1' t2' of
                   Just u  -> extSubst u
                   Nothing -> do
                              diagnosis <- getErrorContext
                              typeError (Unification $ "attempted to unify " ++
                                                       pretty t1' ++
                                                       " with " ++
                                                       pretty t2')
                                        diagnosis
-}

unifyList :: [Type] -> TI ()
unifyList [] = return ()
unifyList [_] = return ()
unifyList (t1:t2:ts) = do
       unify t1 t2
       unifyList (t2:ts)


extSubst   :: Subst -> TI ()
--extSubst s' = TI (\t -> modifyIORef (tcSubst t) (s' @@))
extSubst s = sequence_ [ do y' <- findType y ; liftIO $ writeIORef r (Just y') | (Tyvar { tyvarRef = ~(Just r)} ,y) <- Map.toList s]

newTVar    :: Kind -> TI Type
newTVar k   = TI $ \te -> do
                n <- readIORef (tcVarnum te)
                r <- newIORef Nothing
                let ident = Qual (tcModuleName te) $ HsIdent $ "v" ++ show n
                    v = tyvar ident k (Just r)
                writeIORef (tcVarnum te) $! n + 1
                return $ TVar v



freshInst :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do
        ts <- mapM newTVar ks
        let v = (inst ts qt)
        return (v)


pretty  :: PPrint Doc a => a -> String
pretty   = render . pprint
