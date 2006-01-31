-- | examine all uses of types in a program to determine which ones are
-- actually needed in the method generation

module E.TypeAnalysis(typeAnalyze, pruneE) where

import Control.Monad.Reader
import Control.Monad.Identity
import Data.Monoid
import Data.FunctorM
import qualified Data.Set as Set
import qualified Data.Map as Map

import E.Annotate
import E.E hiding(isBottom)
import E.Inline(emapE',emapE_)
import E.Program
import E.TypeCheck
import Fixer.Fixer
import Fixer.VMap
import GenUtil
import Name.Name
import Name.Names
import qualified Info.Info as Info
import Support.CanType


type Typ = VMap Name
type Env = (Value (Set.Set TVr),Map.Map Id [Value Typ])

extractValMap :: [(TVr,E)] -> Map.Map Id [Value Typ]
extractValMap ds = Map.fromList [ (tvrIdent t,f e []) | (t,e) <- ds] where
    f (ELam tvr e) rs | sortStarLike (getType tvr) = f e (runIdentity (Info.lookup $ tvrInfo tvr):rs)
    f _ rs = reverse rs

-- all variables _must_ be unique before running this
typeAnalyze :: Program -> IO Program
typeAnalyze prog = do
    fixer <- newFixer
    usedVals <- newValue fixer Set.empty
    let lambind _ nfo = do
            x <- newValue fixer ( bottom :: Typ)
            return $ Info.insert x nfo
        lamread _ nfo = do
            rv <- readValue (runIdentity $ Info.lookup nfo)
            return (Info.insert (rv :: Typ) $ Info.delete (undefined :: Value Typ) nfo)
        lamdel _ nfo = return (Info.delete (undefined :: Value Typ) nfo)
    ds <- annotateDs mempty lambind (\_ -> return) (\_ -> return) (programDs prog)
    calcDs (usedVals,extractValMap ds) ds
    mapM_ (calcE (usedVals,extractValMap ds) . EVar ) (progEntryPoints prog)
    calcFixpoint "type analysis" fixer
    ds <- annotateDs mempty (\_ -> return) (\_ -> return) lamread ds
    ds <- annotateDs mempty lamdel (\_ -> return) (\_ -> return) ds
    return $ programSetDs ds prog

calcDs ::  Env -> [(TVr,E)] -> IO ()
calcDs env@(usedVals,_) ds = do
    mapM_ d ds
    flip mapM_ ds $ \ (v,e) -> do
        addRule $ conditionalRule (v `Set.member`) usedVals (ioToRule $ calcE env e)
     where
        --mapM_ d ds >> mapM_ (calcE env) (snds ds) where
    d (t,e) | not (sortStarLike (getType t)) = return ()
    d (t,e) | Just v <- getValue e = do
        let Just t' = Info.lookup (tvrInfo t)
        addRule $ t' `isSuperSetOf` v
    d (t, ELit (LitCons n xs _)) = do
        let Just t' = Info.lookup (tvrInfo t)
            v = vmapSingleton n
        addRule $ t' `isSuperSetOf` (value v)
        xs' <- mapM getValue xs
        flip mapM_ (zip xs' [0.. ])  $ \ (v,i) -> do
            addRule $ modifiedSuperSetOf t' v (vmapArgSingleton n i)
    d (t,e) | (EVar v,as) <- fromAp e = do
        let Just t' = Info.lookup (tvrInfo t)
            Just v' = Info.lookup (tvrInfo v)
        as' <- mapM getValue as
        addRule $ dynamicRule v' $ \ v -> mconcat $ flip map (vmapHeads v) $ \ h ->
            mconcat $ t' `isSuperSetOf` value (vmapSingleton h) : (flip map (zip as' [0.. ])  $ \ (a,i) -> modifiedSuperSetOf t' a $ \ v -> vmapArgSingleton h i v)
    d (t,e) = fail $ "calcDs: " ++ show (t,e)

-- TODO - make default case conditional
calcAlt env v (Alt (LitCons n xs _) e) = do
    addRule $ conditionalRule (\ (VMap _ vs) -> n `Set.member` vs) v $ ioToRule $ do
        calcE env e
        flip mapM_ (zip [0..] xs) $ \ (i,t) -> do
            let Just t' = Info.lookup (tvrInfo t)
            addRule $ modifiedSuperSetOf t' v (vmapArg n i)


calcE :: Env -> E -> IO ()
calcE (usedVals,env) (ELetRec ds e) = calcDs nenv ds >> calcE nenv e where
    nenv = (usedVals,extractValMap ds `Map.union` env)
calcE env e | (e',(_:_)) <- fromLam e = calcE env e'
calcE env ec@ECase {} | sortStarLike (getType $ eCaseScrutinee ec) = do
    calcE env (eCaseScrutinee ec)
    fmapM_ (calcE env) (eCaseDefault ec)
    v <- getValue (eCaseScrutinee ec)
    mapM_ (calcAlt env v) (eCaseAlts ec)
calcE env ec@ECase {} = do
    calcE env (eCaseScrutinee ec)
    mapM_ (calcE env) (caseBodies ec)
calcE env e@ELit {} = tagE env e
calcE env e@EPrim {} = tagE env e
calcE _ EError {} = return ()
calcE _ ESort {} = return ()
calcE _ Unknown = return ()
calcE env e | (EVar v,as@(_:_)) <- fromAp e, Just ts <- Map.lookup (tvrIdent v) (snd env) = do
    tagE env e
    flip mapM_ (zip as ts) $ \ (a,t) -> do
        when (sortStarLike (getType a)) $ do
            a' <- getValue a
            addRule $ t `isSuperSetOf` a'
calcE env e@EVar {} = tagE env e
calcE env e@EAp {} = tagE env e
calcE _ e = fail $ "odd calcE: " ++ show e

tagE (usedVals,_) (EVar v) = addRule $ usedVals `isSuperSetOf` value (Set.singleton v)
tagE env e  = emapE_ (tagE env) e

getValue (EVar v)
    | Just x <- Info.lookup (tvrInfo v) = return x
    | otherwise = fail $ "getValue: no varinfo: " ++ show v
getValue e | Just c <- typConstant e = return $ value c
getValue e = fail $ "getValue: " ++ show e

typConstant :: Monad m => E -> m Typ
typConstant (EPi TVr { tvrType = a} b) = do
    ab <- mapM typConstant [a,b]
    return $ vmapValue tc_Arrow ab
typConstant (ELit (LitCons n xs _)) = do
    xs' <- mapM typConstant xs
    return $ vmapValue n xs'
typConstant e = fail $ "typConstant: " ++ show e


-- pruning the unused branches of typecase statements


pruneE :: E -> IO E
pruneE e = return $ runIdentity (prune e)  where
    prune ec@ECase { eCaseScrutinee = EVar v } | sortStarLike (getType v), Just (VMap _ ns) <- Info.lookup (tvrInfo v) = do
        ec' <- pruneCase ec ns
        emapE' prune ec'
    prune e = emapE' prune e

pruneCase :: Monad m => E -> Set.Set Name -> m E
pruneCase ec ns = return $ if null (caseBodies nec) then err else nec where
    err = EError "pruneCase: all alternatives pruned" (getType ec)
    nec = ec { eCaseAlts = f [] $ eCaseAlts ec, eCaseDefault = cd (eCaseDefault ec)}
    f xs [] = reverse xs
    f xs (alt@(Alt (LitCons n _ _) _):rs) | not (n `Set.member` ns) = f xs rs
    f xs (alt:rs) = f (alt:xs) rs
    cd (Just d) | or [ n `notElem` as | n <- Set.toList ns ] = Just d
    cd Nothing = Nothing
    -- The reason we do this is because for a typecase, we need a valid default in order to get the most general type
    cd (Just d) = Just $ EError "pruneCase: default pruned" (getType d)
    as = [ n | LitCons n _ _ <- casePats ec ]


{-

specializeProgram :: Program -> Program
specializeProgram prog = ans where
    entries = Set.fromList $ progEntryPoints -- must not be specialized
    ans = runReader (programMapDs f prog) (cenv $ programDs prog)
    f (t,e) = do
        env <- ask
        ne <- case Map.lookup t env of
            Just (cd,_) -> cd e
            Nothing -> return e
        ne' <- de e
        return (t,ne')
    cenv ds = undefined
    de = undefined

-}




