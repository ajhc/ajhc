module Util.VarName(
    VarNameT(),
    VarName(),
    runVarNameT,
    runVarName,
    newName,
    lookupName,
    newLookupName) where

import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Map as Map

newtype VarNameT nc ni no m a = VarName (StateT (Map.Map ni no, Map.Map nc Int) m a)
    deriving(Monad, MonadTrans, Functor, MonadFix, MonadPlus, MonadIO)

type VarName ni no a = VarNameT () ni no Identity a


runVarNameT :: Monad m => VarNameT nc ni no m a -> m a
runVarNameT  (VarName sm) = evalStateT sm (Map.empty, Map.empty)

runVarName ::  VarName ni no a -> a
runVarName v = runIdentity $ runVarNameT v


newName :: (Ord ni, Ord nc,Monad m) => [no] -> nc -> ni -> VarNameT nc ni no m no
newName ns nc ni = VarName $ do
    (nim,ncm) <- get
    let no = ns!!i
        Just i = fmap (subtract 1) $ Map.lookup nc ncm'
        ncm' = Map.insertWith (+) nc 1 ncm
    put (Map.insert ni no nim, ncm')
    return no

lookupName :: (Ord ni, Monad m,Show ni) => ni -> VarNameT nc ni no m no
lookupName t = VarName $ do
    (nim,_) <- get
    case Map.lookup t nim of
        Just x -> return x
        Nothing -> fail $ "lookupName not found: " ++ show t


newLookupName :: (Ord ni, Ord nc,Monad m) => [no] -> nc -> ni -> VarNameT nc ni no m no
newLookupName ns nc ni = VarName $ do
    (nim,ncm) <- get
    case Map.lookup ni nim of
        Just x -> return x
        Nothing -> do
            let no = ns!!i
                Just i = fmap (subtract 1) $ Map.lookup nc ncm'
                ncm' = Map.insertWith (+) nc 1 ncm
            put (Map.insert ni no nim, ncm')
            return no



