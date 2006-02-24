-- | examine all uses of types in a program to determine which ones are
-- actually needed in the method generation

module E.TypeAnalysis(typeAnalyze, Typ(), pruneE) where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity
import Data.Monoid
import Data.FunctorM
import qualified Data.Set as Set
import qualified Data.Map as Map

import DataConstructors
import Doc.PPrint
import E.Annotate
import E.E hiding(isBottom)
import E.Inline(emapE',emapE_)
import E.Program
import E.Rules
import E.TypeCheck
import E.Values
import Fixer.Fixer
import Fixer.VMap
import GenUtil
import Info.Info(infoMapM,infoMap)
import Info.Types
import Name.Name
import Name.Names
import qualified Info.Info as Info
import Stats
import Support.CanType


type Typ = VMap () Name
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
            return $ Info.insert x (Info.delete (undefined :: Typ) nfo)
        lamread _ nfo | Just v <- Info.lookup nfo = do
            rv <- readValue v
            return (Info.insert (rv :: Typ) $ Info.delete (undefined :: Value Typ) nfo)
        lamread _ nfo = return nfo
        lamdel _ nfo = return (Info.delete (undefined :: Value Typ) nfo)
    prog <- annotateProgram mempty lambind (\_ -> return) (\_ -> return) prog
    let ds = programDs prog
        env = (usedVals,extractValMap ds)
        entries = progEntryPoints prog ++ [ t | (t,_) <- ds, getProperty prop_INSTANCE t]
    calcDs env ds
    mapM_ (calcE env . EVar ) entries
    mapM_ (sillyEntry env) entries
    calcFixpoint "type analysis" fixer
    prog <- annotateProgram mempty (\_ -> return) (\_ -> return) lamread prog
    let (prog',stats) = runStatM $ specializeProgram prog
    prog <- annotateProgram mempty lamdel (\_ -> return) (\_ -> return) prog'
    printStat "TypeAnalysis" stats

    return prog

sillyEntry :: Env -> TVr -> IO ()
sillyEntry env t = mapM_ (addRule . (`isSuperSetOf` value (vmapPlaceholder ()))) args where
    args = lookupArgs t env

lookupArgs t (_,tm) = maybe [] id (Map.lookup (tvrIdent t) tm)

calcDs ::  Env -> [(TVr,E)] -> IO ()
calcDs env@(usedVals,_) ds = do
    mapM_ d ds
    flip mapM_ ds $ \ (v,e) -> do
        addRule $ conditionalRule (v `Set.member`) usedVals (ioToRule $ calcE env e)
     where
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
    d (t, EPi TVr { tvrType = a} b) = do
        let Just t' = Info.lookup (tvrInfo t)
            v = vmapSingleton tc_Arrow
        addRule $ t' `isSuperSetOf` (value v)
        xs' <- mapM getValue [a,b]
        flip mapM_ (zip xs' [0.. ])  $ \ (v,i) -> do
            addRule $ modifiedSuperSetOf t' v (vmapArgSingleton tc_Arrow i)
    d (t,e) | (EVar v,as) <- fromAp e = do
        let Just t' = Info.lookup (tvrInfo t)
            Just v' = Info.lookup (tvrInfo v)
        as' <- mapM getValue as
        addRule $ dynamicRule v' $ \ v -> mconcat $ (t' `isSuperSetOf` value (vmapDropArgs v)):case vmapHeads v of
                Just vs -> concat $ flip map vs $ \h -> (flip map (zip as' [0.. ])  $ \ (a,i) -> modifiedSuperSetOf t' a $ \ v -> vmapArgSingleton h i v)
                Nothing -> flip map (zip as' [0.. ])  $ \ (_,i) -> isSuperSetOf t' (value $ vmapProxyIndirect i v)
    d (t,e) = fail $ "calcDs: " ++ show (t,e)

-- TODO - make default case conditional
calcAlt env v (Alt (LitCons n xs _) e) = do
    addRule $ conditionalRule (n `vmapMember`) v $ ioToRule $ do
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
    when (length as < length ts) $ fail "calcE: unsaturated call to function"
    flip mapM_ (zip as ts) $ \ (a,t) -> do
        when (sortStarLike (getType a)) $ do
            a' <- getValue a
            addRule $ t `isSuperSetOf` a'
calcE env e@EVar {} = tagE env e
calcE env e@EAp {} = tagE env e
calcE env e@EPi {} = tagE env e
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
    prune ec@ECase { eCaseScrutinee = EVar v } | sortStarLike (getType v), Just vm <- Info.lookup (tvrInfo v) = do
        ec' <- pruneCase ec vm
        emapE' prune ec'
    prune e = emapE' prune e

pruneCase :: (Monad m) => E -> VMap () Name -> m E
pruneCase ec ns = return $ if null (caseBodies nec) then err else nec where
    err = EError "pruneCase: all alternatives pruned" (getType ec)
    nec = ec { eCaseAlts = f [] $ eCaseAlts ec, eCaseDefault = cd (eCaseDefault ec)}
    f xs [] = reverse xs
    f xs (alt@(Alt (LitCons n _ _) _):rs) | not (n `vmapMember` ns) = f xs rs
    f xs (alt:rs) = f (alt:xs) rs
    cd (Just d) | Just nns <- vmapHeads ns, or [ n `notElem` as | n <- nns ] = Just d
                | Nothing <- vmapHeads ns = Just d
    cd Nothing = Nothing
    -- The reason we do this is because for a typecase, we need a valid default in order to get the most general type
    cd (Just d) = Just $ EError "pruneCase: default pruned" (getType d)
    as = [ n | LitCons n _ _ <- casePats ec ]



getTyp :: Monad m => E -> DataTable -> Typ -> m E
getTyp kind dataTable vm = f kind vm where
    f kind vm | Just [] <- vmapHeads vm = return $ tAbsurd kind
    f kind vm | Just [h] <- vmapHeads vm = do
        let ss = slotTypes dataTable h kind
            as = [ (s,vmapArg h i vm) | (s,i) <- zip ss [0..]]
        as' <- mapM (uncurry f) as
        return $ ELit (LitCons h as' kind)
    f _ _  = fail "getTyp: not constant type"

specializeProgram :: (MonadStats m) => Program -> m Program
specializeProgram prog = do
    (nds,_) <- specializeDs (progDataTable prog) mempty (programDs prog)
    return $ programSetDs nds prog


specializeDef _dataTable (t,e) | getProperty prop_PLACEHOLDER t = return (t,e)
specializeDef dataTable (tvr,e) = ans where
    sub = substLet  [ (t,v) | (t,Just v) <- sts ]
    sts = map spec ts
    spec t | Just nt <- Info.lookup (tvrInfo t) >>= getTyp (getType t) dataTable, sortStarLike (getType t) = (t,Just nt)
    spec t = (t,Nothing)
    (fe,ts) = fromLam e
    ne = sub $ foldr ELam fe [ t | (t,Nothing) <- sts]
    ans = do
        sequence_ [ mtick ("Specialize.body.{" ++ pprint tvr ++ "}.{" ++ pprint t ++ "}.{" ++ pprint v) | (t,Just v) <- sts ]
        let vs = [ (n,v) | ((_,Just v),n) <- zip sts naturals ]
            sd = not $ null vs
        when sd $ tell (Map.singleton tvr (fsts vs))
        return (if sd then tvr { tvrType = getType ne, tvrInfo = infoMap (dropArguments vs) (tvrInfo tvr) } else tvr,ne)


specBody :: MonadStats m => DataTable -> Map.Map TVr [Int] -> E -> m E
specBody dataTable env e | (EVar h,as) <- fromAp e, Just os <- Map.lookup h env = do
    mtick $ "Specialize.use.{" ++ pprint h ++ "}"
    return $ foldl EAp (EVar h) [ a | (a,i) <- zip as naturals, i `notElem` os ]
specBody dataTable env (ELetRec ds e) = do
    (nds,nenv) <- specializeDs dataTable env ds
    e <- specBody dataTable nenv e
    return $ ELetRec nds e
specBody dataTable env e = emapE' (specBody dataTable env) e

--specializeDs :: MonadStats m => DataTable -> Map.Map TVr [Int] -> [(TVr,E)] -> m ([(TVr,E)]
specializeDs dataTable env ds = do
    (ds,nenv) <- runWriterT $ mapM (specializeDef dataTable) ds
    -- ds <- sequence [ specBody dataTable (nenv `mappend` env) e >>= return . (,) t | (t,e) <- ds]
    let f (t,e) = do
            e <- sb e
            nfo <- infoMapM (mapABodiesArgs sb) (tvrInfo t)
            return (t { tvrInfo = nfo }, e)
        sb = specBody dataTable (nenv `mappend` env)
    ds <- mapM f ds
    return (ds,nenv `mappend` env)




