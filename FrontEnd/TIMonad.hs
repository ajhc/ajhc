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
                --pushErrorContext, 
                withContext,
                --popErrorContext,
                -- DCAssumpTable,
                getSubst,
                getClassHierarchy,
                getKindEnv,
                getSigEnv,
                unify,
                freshInst,
                dConScheme,
                unifyList,
                getModName,
                newTVar) where

import Atom
import Class                 (ClassHierarchy)
import Control.Monad.State hiding(State(..))
import Data.IORef
import Diagnostic
import HsSyn    
import KindInfer             (KindEnv)
import Monad
import PPrint                (pretty)
import qualified Data.Map as Map
import Representation
import TypeSigs              (SigEnv)
import Type                  ((@@), Types (..), Instantiate (..), nullSubst, mgu)
import Utils
import VConsts
import Warning

--------------------------------------------------------------------------------



-- read only environment, set up before type checking.
data TcEnv = TcEnv {
      tcClassHierarchy    :: ClassHierarchy,
      tcKinds             :: KindEnv,
      tcModuleName        :: Module,
      tcDiagnostics       :: [Diagnostic],   -- list of information that might help diagnosis
      tcVarnum            :: IORef Int,
      tcSubst             :: IORef Subst,
      tcDConsEnv          :: Map.Map HsName Scheme,
      tcSigs              :: SigEnv
    } 
   {-! derive: update !-}


--newtype TI a = TI (TcEnv -> State -> (# a, State #))
newtype TI a = TI (TcEnv -> IO a)

--instance MonadState State TI where 
--    {-# INLINE get #-}
--    {-# INLINE put #-}
--    get = TI (\_ s -> (# s,s #))
--    put s = TI (\_ _ -> (# (),s #))
--    get = TI (\s -> readIORef (tcState s))
--    put s = TI (\v -> writeIORef (tcState v) $! s)

--instance MonadReader TcEnv TI  where
{-# INLINE ask #-}
{-# INLINE local #-}
{-# INLINE asks #-}

ask = TI (\t -> return t)
local f (TI c) = TI (\t -> c (f t))
asks f = liftM f ask 

-- dcat == data constructor assump table

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
        -- fail s

--    return a
--        = TI (\_ state -> (# a, state #))    -- maintain state and return value
--    TI comp >>= fun
--        = TI (\e state -> case comp e state of  
--            (# result, newState #) -> case fun result of  
--                TI comp' -> comp' e newState)

--    TI comp >>= fun
--        = TI (\state -> let (result, newState) = comp state
--                            TI comp' = fun result
--                        in
--                        if inerror newState then (undefined, newState)
--                                            else comp' newState)
-- we only continue with the calculations if there isn't an error

instance Functor TI where
    fmap = liftM

runTI     :: Map.Map HsName Scheme-> ClassHierarchy -> KindEnv -> SigEnv -> Module -> TI a -> IO a
runTI env' ch' kt' st' mod' (TI c) = do
    vn <- newIORef 0
    sub <- newIORef nullSubst
    c tcenv {  tcVarnum = vn,  tcSubst = sub } where
    tcenv = TcEnv {
        tcClassHierarchy = ch',
        tcKinds = kt',
        tcModuleName = mod', 
        tcSigs = st',
        tcVarnum = undefined,
        tcSubst = undefined,
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


getSubst :: TI Subst
getSubst = TI $ \t -> readIORef (tcSubst t) -- gets subst

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
unify t1 t2 = do s <- getSubst
                 let t1' = apply s t1
                     t2' = apply s t2
                 case mgu t1' t2' of
                   Just u  -> extSubst u
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

{-
trim       :: [Tyvar] -> TI ()
trim vs     = TI (\state ->
                     let s' = [(v,t) | (v,t) <- toListFM (subst state), v `elem` vs]
                         force = length (tv (map snd s'))
                     in force `seq` ((), state {subst = listToFM s'})
                 )
-}

extSubst   :: Subst -> TI ()
--extSubst s' = TI (\_ state -> (# (), state {subst = s'@@(subst state)} #))
extSubst s' = TI (\t -> modifyIORef (tcSubst t) (s' @@))

newTVar    :: Kind -> TI Type
newTVar k   = TI $ \te -> do 
                n <- readIORef (tcVarnum te)
                let ident = Qual (tcModuleName te) $ HsIdent $ "v" ++ show n
                    v = Tyvar (Atom.fromString $ fromHsName ident) ident k
                writeIORef (tcVarnum te) $! n + 1
                return $ TVar v
                 
--newTVar k   = TI (\te state -> 
--                   let n = varnum state
--                       ident = Qual (tcModuleName te) $ HsIdent $ "v" ++ show n
--                       v = Tyvar (Atom.fromString $ fromHsName ident) ident k
--                   in  (# TVar v, state{varnum = n+1} #)
--                 )

{-
freshInt :: TI Int
freshInt = TI (\state -> 
                   let n = varnum state
                   in  (n, state{varnum = n+1})
                 )
-}


    

freshInst :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do 
        ts <- mapM newTVar ks
        let v = (inst ts qt)
        return (v)


