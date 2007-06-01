module Grin.FromE(compile) where

import Char
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Graph(stronglyConnComp, SCC(..))
import Data.IORef
import Data.Monoid
import List
import Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

import Atom
import C.Arch
import C.FFI hiding(Primitive)
import C.Prims
import Control.Monad.Identity
import DataConstructors
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.E
import E.FreeVars
import E.Program
import E.TypeCheck
import E.Values
import GenUtil
import Grin.Grin
import Grin.Noodle
import Grin.Show
import Grin.Val
import Info.Types
import Name.Id
import Name.Name
import Name.Names
import Options
import PackedString
import Stats(mtick)
import Support.CanType
import Support.FreeVars
import Support.Tuple
import Util.Graph as G
import Util.Once
import Util.SetLike
import Util.UniqueMonad()
import qualified C.FFI as FFI
import qualified C.Op as Op
import qualified FlagDump as FD
import qualified Info.Info as Info
import qualified Stats



{- | Tags
 'f' - normal function
 'F' - postponed function
 'P' - partial application of function
 'C' - data constructor
 'T' - type constructor
 'Y' - partial application of type constructor (think, broken T)
 'b' - built in funttion
 'B' - postponed built in function (built in functions may not be partially applied)
 '@' - very special function or tag
-}

-------------------
-- Compile E -> Exp
-------------------


unboxedMap :: [(Name,Ty)]
unboxedMap = [
    (tc_World__,TyUnit),
    (tc_Ref__,TyPtr tyINode),
    (tc_Array__,TyPtr tyINode),
    (tc_MutArray__,TyPtr tyINode)
    ]

newtype C a = C (ReaderT LEnv IO a)
    deriving(Monad,MonadReader LEnv,UniqueProducer,Functor,MonadIO,Stats.MonadStats)

runC :: LEnv -> C a -> IO a
runC lenv (C x) = runReaderT x lenv

data LEnv = LEnv {
    evaledMap :: IdMap Val,
    lfuncMap  :: IdMap (Atom,Int,[Ty])
}

data CEnv = CEnv {
    scMap :: IdMap (Atom,[Ty],[Ty]),
    ccafMap :: IdMap Val,
    tyEnv :: IORef TyEnv,
    funcBaps :: IORef [(Atom,Lam)],
    errorOnce :: OnceMap ([Ty],String) Atom,
    dataTable :: DataTable,
    counter :: IORef Int
}

dumpTyEnv (TyEnv tt) = mapM_ putStrLn $ sort [ fromAtom n <+> hsep (map show as) <+> "::" <+> show t <> f z <> g th|  (n,TyTy { tySlots = as, tyReturn = t, tySiblings = z, tyThunk = th}) <- Map.toList tt] where
    f Nothing = mempty
    f (Just v) = text " " <> tshow v
    g TyNotThunk = mempty
    g x = text " " <> tshow x

tagArrow = convertName tc_Arrow


flattenScc xs = concatMap f xs where
    f (AcyclicSCC x) = [x]
    f (CyclicSCC xs) = xs



scTag n
    | Just nm <- fromId (tvrIdent n) = toAtom ('f':show nm)
    | otherwise = toAtom ('f':show (tvrIdent n))
cafNum n = V $ - atomIndex (partialTag (scTag n) 0)

toEntry (n,as,e) = f (scTag n) where
        f x = (x,map (toType (TyPtr TyNode) . tvrType )  as,toTypes TyNode (getType (e::E) :: E))

rawNameToTy :: Monad m => Name -> m Ty
rawNameToTy n | RawType <- nameType n = return $ stringNameToTy (show n)
              | otherwise = fail "rawNameToTy: not primitive type"

stringNameToTy :: String -> Ty
stringNameToTy n = TyPrim (archOpTy archInfo n)

toType :: Ty -> E -> Ty
toType node = toty . followAliases mempty where
--    toty (ELit LitCons { litName = n, litArgs = es, litType = ty }) |  ty == eHash, TypeConstructor <- nameType n, Just _ <- fromUnboxedNameTuple n = (tuple (keepIts $ map (toType (TyPtr TyNode) ) es))
    --toty (ELit LitCons { litName = n, litArgs = [], litType = ty }) |  ty == eHash, RawType <- nameType n = (Ty $ toAtom (show n))
    toty (ELit LitCons { litName = n, litArgs = [], litType = ty }) |  ty == eHash, Just t <- rawNameToTy n = t
    toty e@(ELit LitCons { litName = n, litType = ty }) |  ty == eHash = case lookup n unboxedMap of
        Just x -> x
        Nothing -> error $ "Grin.FromE.toType: " ++ show e
    toty _ = node

toTypes :: Ty -> E -> [Ty]
toTypes node = toty . followAliases mempty where
    toty (ELit LitCons { litName = n, litArgs = es, litType = ty }) |  ty == eHash, TypeConstructor <- nameType n, Just _ <- fromUnboxedNameTuple n = ((keepIts $ map (toType (TyPtr TyNode) ) es))
    toty (ELit LitCons { litName = n, litArgs = [], litType = ty }) |  ty == eHash, Just t <- rawNameToTy n = [t]
    toty e@(ELit LitCons { litName = n, litType = ty }) |  ty == eHash = case lookup n unboxedMap of
        Just TyUnit -> []
        Just x -> [x]
        Nothing -> error $ "Grin.FromE.toType: " ++ show e
    toty _ = [node]

toTyTy (as,r) = tyTy { tySlots = as, tyReturn = r }


{-# NOINLINE compile #-}
compile :: Program -> IO Grin
compile prog@Program { progDataTable = dataTable, progMainEntry = mainEntry, progEntryPoints = entries } = do
    tyEnv <- liftIO $ newIORef initTyEnv
    funcBaps <- liftIO $ newIORef []
    counter <- liftIO $ newIORef 100000  -- TODO real number
    let (cc,reqcc,rcafs) = constantCaf prog
        funcMain = toAtom "b_main"
    wdump FD.Progress $ do
        putErrLn $ "Found" <+> tshow (length cc) <+> "CAFs to convert to constants," <+> tshow (length reqcc) <+> "of which are recursive."
        putErrLn "Recursive"
        putDocMLn putStr $ vcat [ pprint v  | v <- reqcc ]
        putErrLn "Constant"
        putDocMLn putStr $ vcat [ pprint v <+> pprint n <+> pprint e | (v,n,e) <- cc ]
        putErrLn "CAFS"
        putDocMLn putStr $ vcat [ pprint v <+> pprint n <+> pprint e | (v,n,e) <- rcafs ]
    errorOnce <- newOnceMap
    let doCompile = compile' cenv
        lenv = LEnv { evaledMap = mempty, lfuncMap = mempty }
        cenv = CEnv {
            funcBaps = funcBaps,
            tyEnv = tyEnv,
            scMap = scMap,
            counter = counter,
            dataTable = dataTable,
            errorOnce = errorOnce,
            ccafMap = fromList $ [(tvrIdent v,e) |(v,_,e) <- cc ]  ++ [ (tvrIdent v,Var vv (TyPtr TyNode)) | (v,vv,_) <- rcafs]
            }
    ds <- runC lenv $ mapM doCompile [ c | c@(v,_,_) <- progCombinators prog, v `notElem` [x | (x,_,_) <- cc]]
    wdump FD.Progress $ do
        os <- onceMapToList errorOnce
        mapM_ print os
    let tf a = a:tagToFunction a
    ds <- return $ flattenScc $ stronglyConnComp [ (a,x, concatMap tf (freeVars z)) | a@(x,(_ :-> z)) <- ds]

    -- FFI
    let tvrAtom t  = liftM convertName (fromId $ tvrIdent t)
    let ef x = do n <- tvrAtom x
                  return (n, [] :-> discardResult (App (scTag x) [] []))
        ep x = do putStrLn ("EP FOR "++show x)
                  n <- tvrAtom x
                  case Info.lookup (tvrInfo x) of
                    Just l -> return [(n, l)]
                    Nothing -> return []
    -- efv <- mapM ef entries -- FIXME
    efv <- return []
    epv <- liftM concat $ mapM ep entries
    enames <- mapM tvrAtom entries


    TyEnv endTyEnv <- readIORef tyEnv
    -- FIXME correct types.
    let newTyEnv = TyEnv $ Map.fromList (Map.toList endTyEnv ++ [(funcMain, toTyTy ([],[]))] ++ [(en, toTyTy ([],[])) | en <- enames])
    wdump FD.Tags $ do
        dumpTyEnv newTyEnv
    fbaps <- readIORef funcBaps
    let cafs = [ (x,y) | (_,x,y) <- rcafs ]
        initCafs = sequenceG_ [ Update (Var v (TyPtr TyNode)) node | (v,node) <- cafs ]
        ds' = ds ++ fbaps
        a @>> b = a :>>= ([] :-> b)
        sequenceG_ [] = Return []
        sequenceG_ (x:xs) = foldl (@>>) x xs
    let grin = setGrinFunctions theFuncs emptyGrin {
            grinEntryPoints = Map.insert funcMain (FfiExport "_amain" Safe CCall) $ Map.fromList epv,
            grinPhase = PhaseInit,
            grinTypeEnv = newTyEnv,
            grinCafs = [ (x,NodeC tagHole []) | (x,_) <- cafs]
            }
        theFuncs = (funcMain ,[] :-> initCafs :>>= [] :->  discardResult (App (scTag mainEntry) [] [])) : efv ++ ds'
    return grin
    where
    scMap = fromList [ (tvrIdent t,toEntry x) |  x@(t,_,_) <- progCombinators prog]
    initTyEnv = mappend primTyEnv $ TyEnv $ Map.fromList $ concat [ makePartials (a,b,c) | (_,(a,b,c)) <-  massocs scMap] ++ concat [con x| x <- Map.elems $ constructorMap dataTable, conType x /= eHash]
    con c | (EPi (TVr { tvrType = a }) b,_) <- fromLam $ conExpr c = return $ (tagArrow,toTyTy ([TyPtr TyNode, TyPtr TyNode],[TyNode]))
    con c | keepCon = return $ (n,TyTy { tyThunk = TyNotThunk, tySlots = keepIts as, tyReturn = [TyNode], tySiblings = fmap (map convertName) sibs}) where
        n | sortKindLike (conType c) = convertName (conName c)
          | otherwise = convertName (conName c)
        as = [ toType (TyPtr TyNode) s |  s <- conSlots c]
        keepCon = isNothing (conVirtual c) || TypeConstructor == nameType (conName c)
        sibs = getSiblings dataTable (conName c)
    con _ = fail "not needed"

discardResult exp = exp :>>= map (Var v0) (getType exp) :-> Return []


shouldKeep :: E -> Bool
shouldKeep e = TyUnit /= toType TyNode e

class Keepable a where
    keepIt :: a -> Bool

--instance Keepable E where
--    keepIt = shouldKeep
instance Keepable Ty where
    keepIt t = t /= TyUnit
instance Keepable Val where
    keepIt t = getType t /= TyUnit

keepIts xs = filter keepIt xs

tySusp fn ts = (partialTag fn 0,(toTyTy (keepIts ts,[TyNode])) { tyThunk = TySusp fn })

makePartials (fn,ts,rt) | 'f':_ <- show fn = (fn,toTyTy (keepIts ts,rt)):f undefined 0 (reverse ts) where
    f _ 0 ts = tySusp fn (reverse ts):f fn 1 ts
    f nfn n (t:ts) = (mfn,(toTyTy (reverse $ keepIts ts,[TyNode])) { tyThunk = TyPApp (if keepIt t then Just t else Nothing) nfn }):f mfn (n + 1) ts  where
        mfn = partialTag fn n
    f _ _ [] = []
--    ans = (fn,toTyTy (keepIts ts,rt)):[(partialTag fn i,toTyTy (keepIts $ reverse $ drop i $ reverse ts ,TyNode)) |  i <- [0.. length ts] ]
makePartials x = error "makePartials"

primTyEnv = TyEnv . Map.map toTyTy $ Map.fromList $ [
    (tagArrow,([TyPtr TyNode, TyPtr TyNode],[TyNode])),
    (funcEval, ([TyPtr TyNode],[TyNode])),
    (tagHole, ([],[TyNode]))
    ]


-- | constant CAF analysis
-- In grin, partial applications are constant data, rather than functions. Since
-- many cafs consist of constant applications, we preprocess them into values
-- beforehand. This also catches recursive constant toplevel bindings.
--
-- takes a program and returns (cafs which are actually constants,which are recursive,rest of cafs)

constantCaf :: Program -> ([(TVr,Var,Val)],[Var],[(TVr,Var,Val)])
constantCaf Program { progDataTable = dataTable, progCombinators = ds } = ans where
    -- All CAFS
    ecafs = [ (v,e) | (v,[],e) <- ds ]
    -- just CAFS that can be converted to constants need dependency analysis
    (lbs',cafs) = G.findLoopBreakers (const 0) (const True) $ G.newGraph (filter (canidate . snd) ecafs) (tvrIdent . fst) (freeVars . snd)
    lbs = Set.fromList $ fsts lbs'
    canidate (ELit _) = True
    canidate (EPi _ _) = True
    canidate e | (EVar x,as) <- fromAp e, Just vs <- mlookup x res, vs > length as = True
    canidate _ = False
    ans = ([ (v,cafNum v,conv e) | (v,e) <- cafs ],[ cafNum v | (v,_) <- cafs, v `Set.member` lbs ], [(v,cafNum v, NodeC (partialTag n 0) []) | (v,e) <- ecafs, not (canidate e), let n = scTag v ])
    res = Map.fromList [ (v,length vs) | (v,vs,_) <- ds]
    coMap = Map.fromList [  (v,ce)| (v,_,ce) <- fst3 ans]
    conv :: E -> Val
    conv e | Just [v] <- literal e = v
    conv (ELit lc@LitCons { litName = n, litArgs = es }) | Just nn <- getName lc = (Const (NodeC nn (keepIts $ map conv es)))
    conv (EPi (TVr { tvrIdent = 0, tvrType =  a}) b)  =  Const $ NodeC tagArrow [conv a,conv b]
    conv (EVar v) | v `Set.member` lbs = Var (cafNum v) (TyPtr TyNode)
    conv e | (EVar x,as) <- fromAp e, Just vs <- mlookup x res, vs > length as = Const (NodeC (partialTag (scTag x) (vs - length as)) (keepIts $ map conv as))
    conv (EVar v) | Just ce <- mlookup v coMap = ce
    conv (EVar v) = Var (cafNum v) (TyPtr TyNode)
    conv x = error $ "conv: " ++ show x
    getName = getName' dataTable

    fst3 (x,_,_) = x

getName' :: (Show a,Monad m) => DataTable -> Lit a E -> m Atom
getName' dataTable v@LitCons { litName = n, litArgs = es }
    | Just _ <- fromUnboxedNameTuple n = fail $ "unboxed tuples don't have names silly"
    | conAlias cons /= NotAlias = error $ "Alias still exists: " ++ show v
    | length es == nargs  = do
        return cn
    | nameType n == TypeConstructor && length es < nargs = do
        return ((partialTag cn (nargs - length es)))
    | otherwise = error $ "Strange name: " ++ show v ++ show nargs ++ show cons
    where
    cn = convertName n
    cons = runIdentity $ getConstructor n dataTable
    nargs = length (conSlots cons)

instance ToVal TVr where
    toVal TVr { tvrType = ty, tvrIdent = num } = case toType (TyPtr TyNode) ty of
--        TyTup [] -> Tup []
        ty -> Var (V num) ty


doApply x y ty | not (keepIt y) = App funcApply [x] ty
doApply x y ty = App funcApply [x,y] ty

evalVar :: [Ty] -> TVr -> C Exp
evalVar fty tvr  = do
    em <- asks evaledMap
    case mlookup (tvrIdent tvr) em of
        Just v -> do
            mtick "Grin.FromE.strict-evaled"
            return (Return [v])
--        Nothing | not isFGrin, Just CaseDefault <- Info.lookup (tvrInfo tvr) -> do
--            mtick "Grin.FromE.strict-casedefault"
--            return (Fetch (toVal tvr))
        Nothing | getProperty prop_WHNF tvr -> do
            mtick "Grin.FromE.strict-propevaled"
            return (Fetch (toVal tvr))
        Nothing -> return $ App funcEval [toVal tvr] fty

compile' ::  CEnv -> (TVr,[TVr],E) -> C (Atom,Lam)
compile' cenv (tvr,as,e) = ans where
    ans = do
        --putStrLn $ "Compiling: " ++ show nn
        x <- cr e
        let (nn,_,_) = runIdentity $ mlookup (tvrIdent tvr) (scMap cenv)
        return (nn,((keepIts $ map toVal as) :-> x))
    funcName = maybe (show $ tvrIdent tvr) show (fromId (tvrIdent tvr))
    cc, ce, cr :: E -> C Exp
    cr x = ce x

    -- | ce evaluates something in strict context returning the evaluated result of its argument.
    ce (ELetRec ds e) = doLet ds (ce e)
    ce (EError s e) = return (Error s (toTypes TyNode e))
    ce (EVar tvr) | isUnboxed (getType tvr) = do
        return (Return $ keepIts [toVal tvr])
    ce (EVar tvr) | not $ isLifted (EVar tvr)  = do
        mtick "Grin.FromE.strict-unlifted"
        return (Fetch (toVal tvr))
    ce e | (EVar tvr,as) <- fromAp e = do
        as <- return $ args as
        lfunc <- asks lfuncMap
        let fty = toTypes TyNode (getType e)
        case mlookup (tvrIdent tvr) (ccafMap cenv) of
            Just (Const c) -> app fty (Return [c]) as
            Just x@Var {} -> app fty (gEval x) as
            Nothing | Just (v,n,rt) <- mlookup (tvrIdent tvr) lfunc -> do
                    let (x,y) = splitAt n as
                    app fty (App v (keepIts x) rt) y
            Nothing -> case mlookup (tvrIdent tvr) (scMap cenv) of
                Just (v,as',es)
                    | length as >= length as' -> do
                        let (x,y) = splitAt (length as') as
                        app fty (App v (keepIts x) es) y
                    | otherwise -> do
                        let pt = partialTag v (length as' - length as)
                        return $ Return [NodeC pt (keepIts as)]
                Nothing | not (isLifted $ EVar tvr) -> do
                    mtick "Grin.FromE.app-unlifted"
                    app fty (Fetch $ toVal tvr) as
                Nothing -> do
                    case as of
                        [] -> evalVar fty tvr
                        _ -> do
                            ee <- evalVar [TyNode] tvr
                            app fty ee as
    ce e | Just z <- literal e = return (Return z)
    ce e | Just (Const z) <- constant e = return (Return $ keepIts [z])
    ce e | Just z <- constant e = return (gEval z)
    ce e | Just z <- con e = return (Return z)


    ce (EPrim ap@(APrim (PrimPrim prim) _) as _) = f (unpackPS prim) as where

        -- holes - are these still useful?
--        f "newHole__" [_] = do
--            let var = Var v2 (TyPtr TyNode)
--            return $ Store (NodeC (toAtom "@hole") []) :>>= var :-> Return (tuple [var])
--        f "fillHole__" [r,v,_] = do
--            let var = Var v2 TyNode
--                [r',v'] = args [r,v]
--            return $ gEval v' :>>= n1 :-> Update r' n1

        -- artificial dependencies
        f "newWorld__" [_] = do
            return $ Return []
        f "dependingOn" [e,_] = ce e


        -- references
        f "newRef__" [v,_] = do
            let [v'] = args [v]
            return $ Store v'
        f "readRef__" [r,_] = do
            let [r'] = args [r]
            return $ Fetch r'
        f "writeRef__" [r,v,_] = do
            let [r',v'] = args [r,v]
            return $ Update r' v'

        -- arrays
        f "newMutArray__" [v,def,_] = do
            let [v',def'] = args [v,def]
            return $ Alloc { expValue = def', expCount = v', expRegion = region_heap, expInfo = mempty }
        f "newBlankMutArray__" [v,_] = do
            let [v'] = args [v]
            return $ Alloc { expValue = ValUnknown (TyPtr TyNode), expCount = v', expRegion = region_heap, expInfo = mempty }
        f "readArray__" [r,o,_] = do
            let [r',o'] = args [r,o]
            return $ Fetch (Index r' o')
        f "indexArray__" [r,o] = do
            let [r',o'] = args [r,o]
            return $ Fetch (Index r' o')
        f "writeArray__" [r,o,v,_] = do
            let [r',o',v'] = args [r,o,v]
            return $ Update (Index r' o') v'

        f ft [v,_]  | ft `elem` ["unsafeFreezeArray__", "unsafeThawArray__"] = do
            let [v'] = args [v]
            return $ Return [v']
        f p xs = fail $ "Grin.FromE - Unknown primitive: " ++ show (p,xs)


    -- other primitives
    ce (EPrim ap@(APrim p _) xs ty) = do
        let prim = ap
            xs' = keepIts $ args xs
            ty' = toTypes TyNode ty

        case p of
            Func True fn as "void" -> return $ Prim ap  xs' ty'
            Func True fn as r -> do
                return $ Prim ap xs' ty'
            Func False _ as r | Just _ <- fromRawType ty ->  do
                return $ Prim ap xs' ty'
            Peek pt' | [addr] <- xs -> do
                return $ Prim ap (args [addr]) ty'
            Peek pt' -> do
                let [_,addr] = xs
                return $ Prim ap (args [addr]) ty'
            Poke pt' ->  do
                let [_,addr,val] = xs
                return $  Prim ap (args [addr,val]) []
            Op (Op.BinOp _ a1 a2) rt -> do
                return $ Prim ap (args xs) ty'
            Op (Op.UnOp _ a1) rt -> do
                return $ Prim ap (args xs) ty'
            Op (Op.ConvOp _ a1) rt -> do
                return $ Prim ap (args xs) ty'
    --        Operator n as r | Just _ <- fromRawType ty -> do
    --            let p = prim { primType = ((map (Ty . toAtom) as),Ty (toAtom r)) }
    --            return $ Prim p (args xs)
            other -> fail $ "ce unknown primitive: " ++ show other

    -- case statements
    ce ECase { eCaseScrutinee = e, eCaseAlts = [Alt LitCons { litName = n, litArgs = xs } wh] } | Just _ <- fromUnboxedNameTuple n, DataConstructor <- nameType n  = do
        e <- ce e
        wh <- ce wh
        return $ e :>>= (keepIts $ map toVal xs) :-> wh
    ce ECase { eCaseScrutinee = e, eCaseAlts = [], eCaseDefault = (Just r)} | not (shouldKeep (getType e)) = do
        e <- ce e
        r <- ce r
        return $ e :>>= [] :-> r
    ce ECase { eCaseScrutinee = e, eCaseBind = b, eCaseAlts = as, eCaseDefault = d } | (ELit LitCons { litName = n, litArgs = [] }) <- followAliases (dataTable cenv) (getType e), Just ty <- rawNameToTy n = do
            v <- if tvrIdent b == 0 then newPrimVar ty else return $ toVal b
            e <- ce e
            as' <- mapM cp'' as
            def <- createDef d (return (toVal b))
            return $
                e :>>= [v] :-> Case v (as' ++ def)
    ce ECase { eCaseScrutinee = scrut, eCaseBind = b, eCaseAlts = as, eCaseDefault = d }  = do
        v <- newNodeVar
        e <- ce scrut
        case (b,scrut) of
            (TVr { tvrIdent = 0 },EVar etvr) -> localEvaled [etvr] v $ do
                    as <- mapM cp as
                    def <- createDef d newNodeVar
                    return $ e :>>= [v] :-> Case v (as ++ def)
            (_,EVar etvr) -> localEvaled [etvr,b] v $ do
                    as <- mapM cp as
                    def <- createDef d newNodeVar
                    return $ e :>>= [v] :-> Return [toVal etvr] :>>= [toVal b] :-> Case v (as ++ def)
            (TVr { tvrIdent = 0 },_) -> do
                as <- mapM cp as
                def <- createDef d newNodeVar
                return $ e :>>= [v] :-> Case v (as ++ def)
            (_,_) -> localEvaled [b] v $ do
                    as <- mapM cp as
                    def <- createDef d newNodeVar
                    return $ e :>>= [v] :-> Store v :>>= [toVal b] :-> Case v (as ++ def)
    ce e = error $ "ce: " ++ render (pprint (funcName,e))

    localEvaled vs v action = local (\lenv -> lenv { evaledMap = nm `mappend` evaledMap lenv }) action where
        nm = fromList [ (tvrIdent x, v) | x <- vs, tvrIdent x /= 0 ]

    localFuncs vs action = local (\lenv -> lenv { lfuncMap = fromList vs `mappend` lfuncMap lenv }) action

    createDef Nothing _ = return []
    createDef (Just e) nnv = do
        nv <- nnv
        x <- ce e
        return [[nv] :-> x]
    cp (Alt lc@LitCons { litName = n, litArgs = es } e) = do
        x <- ce e
        nn <- getName lc
        return ([NodeC nn (keepIts $ map toVal es)] :-> x)
    cp x = error $ "cp: " ++ show (funcName,x)
    cp'' (Alt (LitInt i (ELit LitCons { litName = nn, litArgs = [] })) e) = do
        x <- ce e
        ty <- rawNameToTy nn
        return ([Lit i ty] :-> x)

    getName x = getName' (dataTable cenv) x

    app :: [Ty] -> Exp -> [Val] -> C Exp
    app _ e [] = return e
    app ty e [a] | not (keepIt a) = do
        v <- newNodeVar
        return (e :>>= [v] :-> App funcApply [v] ty)
    app ty e [a] = do
        v <- newNodeVar
        return (e :>>= [v] :-> doApply v a ty)
    app ty e (a:as) | not (keepIt a) = do
        v <- newNodeVar
        app ty (e :>>= [v] :-> App funcApply [v] [TyNode]) as
    app ty e (a:as) = do
        v <- newNodeVar
        app ty (e :>>= [v] :-> doApply v a [TyNode]) as

--    app' (Const (NodeC t cs)) (a:as) | tagIsPartialAp t = do
--        let Just (n,frs) = tagUnfunction t
--            lazy = do
--                mtick "Grin.FromE.lazy-app-const"
--                app' (Const (NodeC (partialTag frs (n - 1)) (cs ++ [a]))) as
--        case a of
--            Const {} -> lazy
--            Lit {} -> lazy
--            Var (V n) _ | n < 0 -> lazy
--            _ -> do
--                mtick "Grin.FromE.lazy-app-store"
--                tpv <- newNodePtrVar
--                x <- app' tpv as
--                return $ Store  (NodeC (partialTag frs (n - 1)) (cs ++ [a])) :>>= tpv :-> x
    app' e [] = return $ Return [e]
    app' e as = do
        mtick "Grin.FromE.lazy-app-bap"
        V vn <- newVar
        let t  = toAtom $ "Bap_" ++ show (length as) ++ "_" ++ funcName ++ "_" ++ show vn
            tl = toAtom $ "bap_" ++ show (length as) ++ "_" ++  funcName ++ "_" ++ show vn
            targs = [Var v ty | v <- [v1..] | ty <- (TyPtr TyNode:map getType as)]
            s = Store (NodeC t (keepIts $ e:as))
        d <- app [TyNode] (gEval p1) (tail targs)
        liftIO $ addNewFunction cenv (tl,(keepIts targs) :-> d)
        return s
    addNewFunction cenv tl@(n,args :-> body) = do
        liftIO $ modifyIORef (funcBaps cenv) (tl:)
        let addt (TyEnv mp) =  TyEnv $ Map.insert sfn sft (Map.insert n (toTyTy (args',getType body)) mp)
            (sfn,sft) = tySusp n args'
            args' = map getType args
        liftIO $ modifyIORef (tyEnv cenv) addt

    -- | cc evaluates something in lazy context, returning a pointer to a node which when evaluated will produce the strict result.
    -- it is an invarient that evaling (cc e) produces the same value as (ce e)
    cc (EPrim (APrim (PrimPrim don) _) [e,_] _) | don == packString "dependingOn" = cc e
    cc e | Just _ <- literal e = error "unboxed literal in lazy context"
    cc e | Just z <- constant e = return (Return $ keepIts [z])
    cc e | Just [z] <- con e = return (Store z)
    cc (EError s e) = do
        let ty = toTypes TyNode e
        a <- liftIO $ runOnceMap (errorOnce cenv) (ty,s) $ do
            u <- newUniq
            let t  = toAtom $ "Berr_" ++ show u
                tl = toAtom $ "berr_" ++ show u
            addNewFunction cenv (tl,[] :-> Error s ty)
            return t
        return $ Return [Const (NodeC a [])]
    cc (ELetRec ds e) = doLet ds (cc e)
    cc e | (EVar v,as@(_:_)) <- fromAp e = do
        as <- return $ args as
        case mlookup (tvrIdent v) (scMap cenv) of
            Just (_,[],_) | Just x <- constant (EVar v) -> app' x as
            Just (v,as',es)
                | length as > length as' -> do
                    let (x,y) = splitAt (length as') as
                    let s = Store (NodeC (partialTag v 0) (keepIts x))
                    nv <- newNodePtrVar
                    z <- app' nv y
                    return $ s :>>= [nv] :-> z
--                | length as < length as', all valIsConstant as -> do
--                    let pt = partialTag v (length as' - length as)
--                    mtick "Grin.FromE.partial-constant"
--                    return $ Return (Const (NodeC pt as))
                | length as < length as' -> do
                    let pt = partialTag v (length as' - length as)
                    as <- return $ keepIts as
                    return $ if all valIsConstant as
                      then Return [Const (NodeC pt as)]
                      else Store (NodeC pt as)
                | otherwise -> do -- length as == length as'
                    return $ Store (NodeC (tagFlipFunction v) (keepIts as))
            Nothing -> app' (toVal v) as
    cc (EVar v) = do
        return $ Return [toVal v]
    cc e = return $ error ("cc: " ++ show e)


    doLet ds e = f (decomposeDs ds) e where
        f [] x = x
        f (Left te@(_,ELam {}):ds) x = f (Right [te]:ds) x
        f (Left (t,e):ds) x | not (isLifted (EVar t)) = do
            mtick "Grin.FromE.let-unlifted"
            e <- ce e
            z <- newNodeVar
            v <- localEvaled [t] z $ f ds x
            return $ (e :>>= [z] :-> Store z) :>>= [toVal t] :-> v
        f (Left (t,e):ds) x = do
            e <- cc e
            v <- f ds x
            return $ e :>>= [toVal t] :-> v
        f (Right bs:ds) x | any (isELam . snd) bs = do
            let g (t,e@ELam {}) = do
                    let (a,as) = fromLam e
                        (nn,_,_) = toEntry (t,[],getType t)
                    x <- ce a
                    return $ [createFuncDef True nn ((keepIts $ map toVal as) :-> x)]
                g' (t,e@ELam {}) =
                    let (a,as) = fromLam e
                        (nn,_,_) = toEntry (t,[],getType t)
                    in (tvrIdent t,(nn,length as,toTypes TyNode (getType a)))
            localFuncs (map g' bs) $ do
                v <- f ds x
                defs <- mapM g bs
                return $ grinLet (concat defs) v


        f (Right bs:ds) x = do
            let u [] ss dus = return (\y -> ss (dus y))
                u ((tvr,e):rs) ss dus = do
                    v <- newNodePtrVar
                    v' <- newNodeVar
                    e <- cc e
                    let (du,t,ts) = doUpdate (toVal tvr) e
                    u rs (\y -> Store (NodeC t (map ValUnknown ts)) :>>= [toVal tvr] :-> ss y) (\y -> du :>>= [] :-> dus y)
            rr <- u bs id id
            v <- f ds x
            return (rr v)

    -- This avoids a blind update on recursive thunks
    doUpdate vr (Store n@(NodeC t ts)) = (Update vr n,t,map getType ts)
    doUpdate vr (x :>>= v :-> e) = let (du,t,ts) = doUpdate vr e in (x :>>= v :-> du,t,ts)
    doUpdate vr x = error $ "doUpdate: " ++ show x
    args es = map f es where
        f x | Just [z] <- literal x = z
        f x | Just z <- constant x =  z
        f (EVar tvr) = toVal tvr
        f x = error $ "invalid argument: " ++ show x



    -- | Takes an E and returns something constant which is either a pointer to
    -- a constant heap location only pointing to global values or constants.
    -- this includes a CAF which may be evaluated, a literal, a saturated
    -- application of constant values to a supercombinator, or a constructor
    -- containing constant values. constant is sort of a misnomer here when
    -- runtime behavior is considered, it means a compile time constant, the
    -- CAFs may be updated with evaluated values.

    constant :: Monad m =>  E -> m Val
    constant (EVar tvr) | Just c <- mlookup (tvrIdent tvr) (ccafMap cenv) = return c
                        | Just (v,as,_) <- mlookup (tvrIdent tvr) (scMap cenv)
                         , t <- partialTag v (length as), tagIsWHNF t = return $ Const $ NodeC t []
    --                        False -> return $ Var (V $ - atomIndex t) (TyPtr TyNode)
    constant e | Just [l] <- literal e = return l
    constant (ELit lc@LitCons { litName = n, litArgs = es }) | Just es <- mapM constant es, Just nn <- getName lc = (return $ Const (NodeC nn (keepIts es)))
    constant (EPi (TVr { tvrIdent = 0, tvrType = a}) b) | Just a <- constant a, Just b <- constant b = return $ Const $ NodeC tagArrow [a,b]
    constant _ = fail "not a constant term"

    -- | convert a constructor into a Val, arguments may depend on local vars.
    con :: Monad m => E -> m [Val]
    con (EPi (TVr {tvrIdent =  0, tvrType = x}) y) = do
        return $  [NodeC tagArrow (args [x,y])]
    con v@(ELit LitCons { litName = n, litArgs = es })
        | conAlias cons /= NotAlias = error $ "Alias still exists: " ++ show v
        | Just v <- fromUnboxedNameTuple n, DataConstructor <- nameType n = do
            return ((keepIts $ args es))
        | length es == nargs  = do
            return [NodeC cn (keepIts $ args es)]
        | nameType n == TypeConstructor && length es < nargs = do
            return [NodeC (partialTag cn (nargs - length es)) $ keepIts (args es)]
        where
        cn = convertName n
        cons = runIdentity $ getConstructor n (dataTable cenv)
        nargs = length (conSlots cons)

    con _ = fail "not constructor"


    scInfo tvr | Just n <- mlookup (tvrIdent tvr) (scMap cenv) = return n
    scInfo tvr = fail $ "not a supercombinator:" <+> show tvr
    newNodeVar =  fmap (\x -> Var x TyNode) newVar
    newPrimVar ty =  fmap (\x -> Var x ty) newVar
    newNodePtrVar =  fmap (\x -> Var x (TyPtr TyNode)) newVar
    newVar = do
        i <- liftIO $ readIORef (counter cenv)
        liftIO $ (writeIORef (counter cenv) $! (i + 2))
        return $! V i

fromRawType (ELit LitCons { litName = tname, litArgs = [] }) | Just r <- rawNameToTy tname = return r
fromRawType _ = fail "not a raw type"

-- | converts an unboxed literal
literal :: Monad m =>  E -> m [Val]
literal (ELit LitCons { litName = n, litArgs = xs })  |  Just xs <- mapM literal xs, Just _ <- fromUnboxedNameTuple n = return (keepIts $ concat xs)
literal (ELit (LitInt i ty)) | Just ptype <- fromRawType ty = return $ [Lit i ptype]
literal (EPrim aprim@(APrim p _) xs ty) | Just ptype <- fromRawType ty, primIsConstant p = do
    xs <- mapM literal xs
    return $ [ValPrim aprim (concat xs) ptype]
literal _ = fail "not a literal term"

