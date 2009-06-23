module E.FromHs(
    convertDecls,
    convertRules,
    createInstanceRules,
    procAllSpecs,
    getMainFunction
    ) where

import Char
import Control.Monad.Identity
import Control.Monad.RWS
import qualified Data.Traversable as T
import Data.Monoid
import List(isPrefixOf,nub)
import Prelude
import qualified Data.Map as Map
import qualified Text.PrettyPrint.HughesPJ as PPrint

import C.FFI
import C.Prims as CP
import StringTable.Atom
import DataConstructors
import Doc.DocLike
import Doc.PPrint
import E.E
import E.Eta
import E.Eval(eval)
import E.LetFloat(atomizeAp)
import E.PrimOpt
import E.Rules
import E.Show(render)
import E.Subst
import E.Traverse
import E.TypeCheck
import E.Values
import FrontEnd.Class
import FrontEnd.Rename(unRename)
import FrontEnd.SrcLoc
import FrontEnd.Syn.Traverse(getNamesFromHsPat)
import FrontEnd.Tc.Main(isTypePlaceholder)
import FrontEnd.Tc.Module(TiData(..))
import FrontEnd.Tc.Type hiding(Rule(..))
import FrontEnd.Warning
import FrontEnd.HsSyn as HS
import Info.Types
import Name.Name as Name
import Name.Names
import Name.Id
import Name.VConsts
import Options
import PackedString
import PrimitiveOperators
import Support.CanType
import Support.FreeVars
import Util.Gen
import Util.NameMonad
import Util.SetLike
import qualified FlagOpts as FO
import qualified FrontEnd.Tc.Type as T(Rule(..))
import qualified FrontEnd.Tc.Type as Type
import qualified Info.Info as Info

ump sl e = EError (show sl ++ ": Unmatched pattern") e


createIf e a b = do
    [tv] <- newVars [Unknown]
    return $ createIfv tv e a b

createIfv v e a b = res where
    tv = v { tvrType = tBoolzh }
    ic = eCase (EVar tv) [Alt lTruezh a, Alt lFalsezh b] Unknown
    res = eCase e [Alt (litCons { litName = dc_Boolzh, litArgs = [tv], litType = tBool }) ic] Unknown

ifzh e a b = eCase e [Alt lTruezh a, Alt lFalsezh b] Unknown

newVars :: UniqueProducer m => [E] -> m [TVr]
newVars xs = f xs [] where
    f [] xs = return $ reverse xs
    f (x:xs) ys = do
        s <- newUniq
        f xs (tVr (anonymous s) x:ys)

tipe t = f t where
    f (TAp (TAp (TCon arr) a1) a2) | tyconName arr == tc_Arrow = f (TArrow a1 a2)
    f (TAp t1 t2) = eAp (f t1) (f t2)
    f (TArrow t1 t2) =  EPi (tVr emptyId (f t1)) (f t2)
    f (TCon (Tycon n k)) | Just n' <- Map.lookup n primitiveAliases = ELit litCons { litName = n', litType = kind k }
    f (TCon (Tycon n k)) =  ELit litCons { litName = n, litType = kind k }
    f (TVar tv) = EVar (cvar [] tv)
    f (TMetaVar mv) = cmvar mv
    f (TForAll vs (ps :=> t)) = foldr EPi (f t) (map (cvar $ freeVars ps) vs)
    f (TExists xs (_ :=> t)) = let
        xs' = map (kind . tyvarKind) xs
        in ELit litCons { litName = unboxedNameTuple TypeConstructor (length xs' + 1), litArgs = f t:xs', litType = eHash }
    f TAssoc {} = error "E.FromHs.tipe TAssoc"
    cvar fvs tv@Tyvar { tyvarName = n, tyvarKind = k }
        | tv `elem` fvs = setProperty prop_SCRUTINIZED (tVr (lt n) (kind k))
        | otherwise = tVr (lt n) (kind k)
    cmvar MetaVar { metaKind = k } = tAbsurd (kind k)
    lt n | nameType n == TypeVal = toId n  -- verifies namespace
         | otherwise = error "E.FromHs.lt"

kind (KBase KUTuple) = eHash
kind (KBase KHash) = eHash
kind (KBase Star) = eStar
kind (KBase KQuest) = eStar      -- XXX why do these still exist?
kind (KBase KQuestQuest) = eStar
kind (Kfun k1 k2) = EPi (tVr emptyId (kind k1)) (kind k2)
kind (KVar _) = error "Kind variable still existing."
kind _ = error "E.FromHs.kind: unknown"





fromTyvar (Tyvar n k) = tVr (toId n) (kind k)

fromSigma (TForAll vs (_ :=> t)) = (map fromTyvar vs, tipe t)
fromSigma t = ([], tipe t)

monadicLookup k m = case Map.lookup k m of
    Just x  -> return x
    Nothing -> fail "key not found"

convertValue n = do
    assumps <- asks ceAssumps
    dataTable <- asks ceDataTable
    t <- monadicLookup n assumps
    let ty = removeNewtypes dataTable (tipe t)
    cc <- asks ceCoerce
    lm <- case Map.lookup n cc of
        Nothing -> do
            let (vs,_) = fromSigma t
            return (flip (foldr eLam) vs)
        Just CTId -> do return id
        Just ~(CTAbs ts) -> do return $ \e -> foldr eLam e (map fromTyvar ts)
    return (tVr (toId n) ty,ty,lm)




--convertType t = do
--    dataTable <- asks ceDataTable
--    return $ removeNewtypes dataTable (tipe t)


matchesConv ms = map v ms where
    v (HsMatch _ _ ps rhs wh) = (ps,rhs,wh)

altConv as = map v as where
    v (HsAlt _ p rhs wh) = ([p],rhs,wh)

argTypes e = span (sortSortLike . getType) (map tvrType xs) where
    (_,xs) = fromPi e
argTypes' :: E -> ([E],E)
argTypes' e = let (x,y) = fromPi e in (map tvrType y,x)


getMainFunction :: Monad m => DataTable -> Name -> (Map.Map Name (TVr,E)) -> m (TVr,E)
getMainFunction dataTable name ds = do
  mt <- case Map.lookup name ds of
    Just x -> return x
    Nothing -> fail $ "Could not find main function: " ++ show name
  let funcs = runIdentity $ T.mapM (\n -> return . EVar . fst $ runEither (show n) $ monadicLookup n ds) sFuncNames
  nameToEntryPoint dataTable (fst mt) (toName Name.Val "theMain") Nothing funcs

nameToEntryPoint :: Monad m => DataTable -> TVr -> Name -> Maybe FfiExport -> FuncNames E -> m (TVr,E)
nameToEntryPoint dataTable main cname ffi ds = ans where
    ans = do
        let runMain      = func_runMain ds
            runExpr      = func_runExpr ds
            runNoWrapper = func_runNoWrapper ds
            runRaw       = func_runRaw ds
        let e = case extractIO (getType maine) of
                Just x | not (fopts FO.Wrapper) -> EAp (EAp runNoWrapper x) maine
                Just x ->  EAp (EAp runMain  x ) maine
                Nothing | fopts FO.Raw -> EAp (EAp runRaw ty) maine
                Nothing ->  EAp (EAp runExpr ty) maine
            ne = ELam worldVar (EAp e (EVar worldVar))
            worldVar = tvr { tvrIdent = va1, tvrType = tWorld__ }
            theMainTvr =  tVr (toId cname) (infertype dataTable ne)
            tvm@(TVr { tvrType =  ty}) =  main
            maine = foldl EAp (EVar tvm) [ tAbsurd k |  TVr { tvrType = k } <- xs, sortKindLike k ]
            (_,xs) = fromPi ty
        return (tvrInfo_u (case ffi of Just ffi -> Info.insert ffi; Nothing -> id) $ setProperty prop_EXPORTED theMainTvr,ne)

-- | create a RULE for each instance attached to the class methods.
-- These rules allow early specialization of monomorphic code, and are
-- eventually used in E.TypeAnalysis.expandPlaceholder to fill out
-- the generic class method bodies.

{-# NOINLINE createInstanceRules #-}
createInstanceRules :: Monad m => DataTable -> ClassHierarchy -> [(TVr,E)] -> m Rules
createInstanceRules dataTable classHierarchy funcs = return $ fromRules ans where
    ans = concatMap cClass (classRecords classHierarchy)
    cClass classRecord =  concat [ method classRecord n mve | (n,TForAll _ (_ :=> t)) <- classAssumps classRecord, mve <- findName n ]

    method classRecord methodName (methodVar,_) = as where
        ty = tvrType methodVar
        defaultName = defaultInstanceName methodName

        as = [ rule  t | Inst { instHead = _ :=> IsIn _ t }  <- snub (classInsts classRecord) ]
        rule t = makeRule ("Rule.{" ++ show name ++ "}") (Module (show name),0) RuleSpecialization ruleFvs methodVar (vp:map EVar args) (removeNewtypes dataTable body)  where
            ruleFvs = [ t | ~(EVar t) <- vs] ++ args
            (vp,vs) = valToPat' (removeNewtypes dataTable $ tipe t)
            name = instanceName methodName (getTypeCons t)
            bodyt = foldl eAp ty (vp:map EVar args)
            body = case findName name of
                Just (n,_) -> runIdentity $ do actuallySpecializeE (EVar n) bodyt
                Nothing -> case findName defaultName of
                    Just (deftvr,_) | otherwise -> runIdentity $ do actuallySpecializeE (EVar deftvr) bodyt
                    Nothing -> EError ( show methodName ++ ": undefined at type " ++  PPrint.render (pprint t)) bodyt
                    --Just (deftvr,_) -> eLet tv vp $ runIdentity $ do actuallySpecializeE (EVar deftvr) (foldl eAp ty $ EVar tv:map EVar args) where -- foldl EAp (EAp (EVar deftvr) (EVar tv)) (map EVar args) where
                    --    tv = tvr { tvrIdent = head [ n | n <- newIds (freeVars vp `mappend` fromList (map tvrIdent args))], tvrType = getType vp }
                    --Just (deftvr,_) | null vs -> foldl EAp (EAp (EVar deftvr) vp) (map EVar args)

        -- this assumes the class argument is always the first type parameter
        (_,_:args') = fromPi ty
        (args,_) = span (sortKindLike . tvrType)  args'

        someIds = newIds (fromList $ map tvrIdent args')
        valToPat' (ELit LitCons { litAliasFor = af,  litName = x, litArgs = ts, litType = t }) = ans where
            ans = (ELit litCons { litAliasFor = af, litName = x, litArgs = ts', litType = t },ts')
            ts' = [ EVar (tVr j (getType z)) | z <- ts | j <- someIds]
        valToPat' (EPi tv@TVr { tvrType =  a} b)  = (EPi tvr { tvrType =  a'} b',[a',b']) where
            a' = EVar (tVr ja (getType a))
            b' = EVar (tVr jb (getType b))
            (ja:jb:_) = someIds
        valToPat' x = error $ "FromHs.valToPat': " ++ show x

    funcsMap = Map.fromList [ (n,(v,e)) | (v,e) <- funcs, let Just n = fromId (tvrIdent v) ]
    findName name = case Map.lookup name funcsMap of
        Nothing -> fail $ "Cannot find: " ++ show name
        Just n -> return n

getTypeCons (TCon (Tycon n _)) = n
getTypeCons (TAp a _) = getTypeCons a
getTypeCons (TArrow {}) = tc_Arrow
getTypeCons x = error $ "getTypeCons: " ++ show x



unbox :: DataTable -> E -> Id -> (E -> E) -> E
unbox dataTable e _vn wtd | getType (getType e) == eHash = wtd e
unbox dataTable e vn wtd = eCase e [Alt (litCons { litName = cna, litArgs = [tvra], litType = te }) (wtd (EVar tvra))] Unknown where
    te = getType e
    tvra = tVr vn sta
    Just (ExtTypeBoxed cna sta _) = lookupExtTypeInfo dataTable te

createFunc :: UniqueProducer m => DataTable -> [E] -> ([TVr] -> (E -> E,E)) -> m E
createFunc dataTable es ee = do
    xs <- flip mapM es $ \te -> do
        eti <- lookupExtTypeInfo dataTable te
        [n] <- newVars [te]
        case eti of
            ExtTypeVoid -> fail "createFunc: attempt to pass a void argument"
            ExtTypeBoxed cn sta _ -> do
                [n'] <- newVars [sta]
                return (n,n',Just cn)
            ExtTypeRaw _ -> do
                return (n,n,Nothing)
    let tvrs' = [ n' | (_,n',_) <- xs ]
        tvrs = [ t | (t,_,_) <- xs]
        (me,innerE) = ee tvrs'
        eee = me $ foldr esr innerE xs
        esr (tvr,tvr',Just cn) e = eCase (EVar tvr) [Alt (litCons { litName = cn, litArgs = [tvr'], litType = tvrType tvr }) e] Unknown
        esr (_,_,Nothing) e = e
    return $ foldr ELam eee tvrs

instance GenName String where
   genNames i = map (('x':) . show) [i..]

{-# NOINLINE convertRules #-}
convertRules :: Module -> TiData -> ClassHierarchy -> Map.Map Name Type -> DataTable -> [HsDecl] -> IO Rules
convertRules mod tiData classHierarchy assumps dataTable hsDecls = ans where
    ans = do
        rawRules <- concatMapM g hsDecls
        return $ fromRules [ makeRule n (mod,i) (if catalyst then RuleCatalyst else RuleUser) vs head args e2 | (catalyst,n,vs,e1,e2) <- rawRules, let (EVar head,args) = fromAp e1 | i <- [1..] ]
    g (HsPragmaRules rs) = mapM f rs
    g _ = return []
    f pr = do
        let ce = convertE tiData classHierarchy assumps dataTable (hsRuleSrcLoc pr)
        e1 <- ce (hsRuleLeftExpr pr)
        e2 <- ce (hsRuleRightExpr pr)
        (ts,cs) <- runNameMT $ do
            ts <- flip mapM (filter (sortKindLike . getType) $ freeVars e1) $ \tvr -> do
                --return (tvrIdent tvr,tvr)
                nn <- newNameFrom (map (:'\'':[]) ['a' ..])
                return (tvrIdent tvr,tvr { tvrIdent = toId (toName TypeVal nn) })
            cs <- flip mapM [toTVr assumps dataTable (toName Val v) | (v,_) <- hsRuleFreeVars pr ] $ \tvr -> do
                let ur = show $ unRename $ nameName (toUnqualified $ runIdentity $ fromId (tvrIdent tvr))
                nn <- newNameFrom (ur:map (\v -> ur ++ show v) [1 ::Int ..])
                return (tvrIdent tvr,tvr { tvrIdent = toId (toName Val nn) })
            return (ts,cs)
        let smt = substMap $ fromList [ (x,EVar y)| (x,y) <- ts ]
            sma = substMap $ fromList [ (x,EVar y)| (x,y) <- cs' ]
            cs' =  [ (x,(tvrType_u smt y))| (x,y) <- cs ]
            e2' = deNewtype dataTable $ smt $ sma e2
        --e2 <- atomizeAp False dataTable Stats.theStats mainModule e2'
        let e2 = atomizeAp mempty False dataTable e2'
        return (hsRuleIsMeta pr,hsRuleString pr,( snds (cs' ++ ts) ),eval $ smt $ sma e1,e2)

convertE :: Monad m => TiData -> ClassHierarchy -> Map.Map Name Type -> DataTable -> SrcLoc -> HsExp -> m E
convertE tiData classHierarchy assumps dataTable srcLoc exp = do
    [(_,_,e)] <- convertDecls tiData mempty classHierarchy assumps dataTable [HsPatBind srcLoc (HsPVar sillyName') (HsUnGuardedRhs exp) []]
    return e

v_silly = toName Val ("Jhc@","silly")
sillyName' = nameName v_silly

data CeEnv = CeEnv {
    ceAssumps :: Map.Map Name Type,
    ceCoerce :: Map.Map Name CoerceTerm,
    ceFuncs  :: FuncNames E,
    ceProps  :: IdMap Properties,
    ceSrcLoc :: SrcLoc,
    ceDataTable :: DataTable
    }

newtype Ce t a = Ce (RWST CeEnv [Warning] Int t a)
    deriving(Monad,Functor,MonadTrans,MonadIO,MonadReader CeEnv,MonadState Int)

instance Monad t => MonadWarn (Ce t) where
    addWarning w = Ce $ tell [w]

instance Monad t => MonadSrcLoc (Ce t) where
    getSrcLoc = asks ceSrcLoc

instance Monad t => MonadSetSrcLoc (Ce t) where
    withSrcLoc sl = local (\ce -> ce { ceSrcLoc = sl })

instance Monad m => UniqueProducer (Ce m) where
    newUniq = do
        i <- get
        put $! (i + 1)
        return i

instance Monad m => DataTableMonad (Ce m) where
    getDataTable = asks ceDataTable

applyCoersion :: Monad m => CoerceTerm -> E -> Ce m E
applyCoersion CTId e = return e
applyCoersion ct e = etaReduce `liftM` f ct e where
    f CTId e = return e
    f (CTAp ts) e = return $ foldl eAp e (map tipe ts)
    f (CTAbs ts) e = return $ foldr eLam e (map fromTyvar ts)
    f (CTCompose ct1 ct2) e = f ct1 =<< (f ct2 e)
    f (CTFun CTId) e = return e
    f (CTFun ct) e = do
        let EPi TVr { tvrType = ty } _ = getType e
        [y] <- newVars [ty]
        fgy <- f ct (EAp e (EVar y))
        return (eLam y fgy)

-- | return primitive instances associated with class given as argument
primitiveInstances :: Name -> [(Name,TVr,E)]
primitiveInstances name = [(n,setProperties [prop_INSTANCE,prop_INLINE] $ tVr (toId n) (getType v),v) | (cn,n,v) <- constantMethods, cn == name]

{-# NOINLINE convertDecls #-}
convertDecls :: Monad m => TiData -> IdMap Properties -> ClassHierarchy -> Map.Map Name Type -> DataTable -> [HsDecl] -> m [(Name,TVr,E)]
convertDecls tiData props classHierarchy assumps dataTable hsDecls = liftM fst $ evalRWST ans ceEnv 2 where
    ceEnv = CeEnv {
        ceCoerce = tiCoerce tiData,
        ceAssumps = assumps,
        ceFuncs = funcs,
        ceProps = props,
        ceSrcLoc = bogusASrcLoc,
        ceDataTable = dataTable
        }
    Identity funcs = T.mapM (return . EVar . toTVr assumps dataTable) sFuncNames
    Ce ans = do
        nds <- mapM cDecl hsDecls
        return (map anninst $ concat nds)
    doNegate e = eAp (eAp (func_negate funcs) (getType e)) e
    anninst (a,b,c)
        | "Instance@" `isPrefixOf` show a = (a,setProperty prop_INSTANCE b, deNewtype dataTable c)
        | otherwise = (a,b, deNewtype dataTable c)

    marshallToC :: UniqueProducer m => DataTable -> E -> E -> m E
    marshallToC dataTable e te = do
        eti <- lookupExtTypeInfo dataTable te
        case eti of
            ExtTypeBoxed cna sta _ -> do
                [tvra] <- newVars [sta]
                return $ eCase e
                               [Alt (litCons { litName = cna, litArgs = [tvra], litType = te })
                                    (EVar tvra)]
                               Unknown
            ExtTypeRaw _ -> return e
            ExtTypeVoid -> fail "marshallToC: trying to marshall void"

    marshallFromC :: Monad m => DataTable -> E -> E -> m E
    marshallFromC dataTable ce te = do
        eti <- lookupExtTypeInfo dataTable te
        case eti of
            ExtTypeBoxed cna _ _ -> return $ ELit (litCons { litName = cna, litArgs = [ce], litType = te })
            ExtTypeRaw _ -> return ce
            ExtTypeVoid -> fail "marshallFromC: trying to marshall void"

    -- first argument builds the actual call primitive, given
    -- (a) the C argtypes
    -- (b) the C return type
    -- (c) whether it's IO-like or not
    -- (d) the real return type
    -- (e) the arguments themselves
    -- ccallHelper returns a function expression to perform the call, when given the arguments
    ccallHelper :: Monad m => ([ExtType] -> ExtType -> Bool -> [E] -> E -> E) -> E -> Ce m E
    ccallHelper myPrim ty = do
        let (ts,rt) = argTypes' ty
            (isIO,rt') =  extractIO' rt
        es <- newVars [ t |  t <- ts, not (sortKindLike t) ]
        pt <- lookupExtTypeInfo dataTable rt'
        cts <- mapM (lookupExtTypeInfo dataTable) (filter (not . sortKindLike) ts)
        [tvrWorld, tvrWorld2] <- newVars [tWorld__,tWorld__]
        let cFun = createFunc dataTable (map tvrType es)
            prim = myPrim (map extTypeInfoExtType cts) (extTypeInfoExtType pt)
        case (isIO,pt) of
            (True,ExtTypeVoid) -> cFun $ \rs -> (,) (ELam tvrWorld) $
                        eStrictLet tvrWorld2
                                   (prim True
                                         (EVar tvrWorld
                                          :[EVar t | t <- rs ])
                                         tWorld__)
                                   (eJustIO (EVar tvrWorld2) vUnit)
            (False,ExtTypeVoid) -> fail "pure foreign function must return a valid value"
            (_,ExtTypeBoxed cn rtt' _) -> do
                [rtVar,rtVar'] <- newVars [rt',rtt']
                let rttIO' = ltTuple' [tWorld__, rtt']
                case isIO of
                    False -> cFun $ \rs -> (,) id $
                                 eStrictLet rtVar'
                                           (prim False
                                                 [ EVar t | t <- rs ]
                                                 rtt')
                                           (ELit $ litCons { litName = cn, litArgs = [EVar rtVar'], litType = rt' })
                    True -> cFun $ \rs -> (,) (ELam tvrWorld) $
                                eCaseTup' (prim True
                                                (EVar tvrWorld:[EVar t | t <- rs ])
                                                rttIO')
                                          [tvrWorld2,rtVar']
                                          (eLet rtVar
                                                (ELit $ litCons { litName = cn, litArgs = [EVar rtVar'], litType = rt' })
                                                (eJustIO (EVar tvrWorld2) (EVar rtVar)))
            (_,ExtTypeRaw  _) -> do
                [rtVar] <- newVars [rt']
                let rttIO' = ltTuple' [tWorld__, rt']
                case isIO of
                    False -> cFun $ \rs -> (,) id $ (prim False [ EVar t | t <- rs ] rt') 
                    True -> cFun $ \rs -> (,) (ELam tvrWorld) $
                                eCaseTup' (prim True (EVar tvrWorld:[EVar t | t <- rs ]) rttIO')
                                          [tvrWorld2,rtVar]
                                          (eJustIO (EVar tvrWorld2) (EVar rtVar))

    cDecl :: Monad m => HsDecl -> Ce m [(Name,TVr,E)]
    cDecl (HsForeignDecl _ (FfiSpec (Import cn req) _ Primitive) n _) = do
        let name      = toName Name.Val n
        (var,ty,lamt) <- convertValue name
        let (ts,rt)   = argTypes' ty
            prim      = APrim (PrimPrim $ toAtom cn) req
        es <- newVars [ t |  t <- ts, not (sortKindLike t) ]
        let result    = foldr ($) (processPrimPrim dataTable $ EPrim prim [ EVar e | e <- es, not (tvrType e == tUnit)] rt) (map ELam es)
        return [(name,setProperty prop_INLINE var,lamt result)]
    cDecl (HsForeignDecl _ (FfiSpec (ImportAddr rcn req) _ _) n _) = do
        let name       = toName Name.Val n
        (var,ty,lamt)  <- convertValue name
        let (_ts,rt)   = argTypes' ty
            expr x     = return [(name,setProperty prop_INLINE var,lamt x)]
            prim       = APrim (AddrOf $ packString rcn) req
        -- this needs to be a boxed value since we can't have top-level
        -- unboxed values yet.
        eti <- lookupExtTypeInfo dataTable rt
        case eti of
            ExtTypeBoxed cn st _ -> do
                [uvar] <- newVars [st]
                expr $ eStrictLet uvar (EPrim prim [] st) (ELit (litCons { litName = cn, litArgs = [EVar uvar], litType = rt }))
            _ -> fail "foreign import of address must be of a boxed type"

    cDecl (HsForeignDecl _ (FfiSpec (Import rcn req) _ CCall) n _) = do
        let name = toName Name.Val n
        (var,ty,lamt) <- convertValue name
        result <- ccallHelper
                     (\cts crt io args rt ->
                      EPrim (APrim (Func io (packString rcn) cts crt) req) args rt)
                     ty
        return [(name,setProperty prop_INLINE var,lamt result)]
    cDecl (HsForeignDecl _ (FfiSpec Dynamic _ CCall) n _) = do
        -- XXX ensure that the type is of form FunPtr /ft/ -> /ft/
        let name = toName Name.Val n
        (var,ty,lamt) <- convertValue name
        --let ((fptrTy:_), _) = argTypes' ty
        --    fty = discardArgs 1 ty

        result <- ccallHelper
                     (\cts crt io args rt ->
                      EPrim (APrim (IFunc io (tail cts) crt) (Requires [] [])) args rt)
                     ty
        return [(name,setProperty prop_INLINE var,lamt result)]

    cDecl (HsForeignDecl _ (FfiSpec (Import rcn _) _ DotNet) n _) = do
        (var,ty,lamt) <- convertValue (toName Name.Val n)
        let (ts,rt) = argTypes' ty
            (isIO,rt') = extractIO' rt
        es <- newVars [ t |  t <- ts, not (sortKindLike t) ]
        pt <- lookupExtTypeInfo dataTable rt'
        [tvrWorld, tvrWorld2] <- newVars [tWorld__,tWorld__]
        dnet <- parseDotNetFFI rcn
        let cFun = createFunc dataTable (map tvrType es)
            prim rs rtt = EPrim (APrim dnet { primIOLike = isIO } mempty)
        result <- case (isIO,pt) of
            (True,ExtTypeVoid) -> cFun $ \rs -> (,) (ELam tvrWorld) $
                        eStrictLet tvrWorld2 (prim rs "void" (EVar tvrWorld:[EVar t | t <- rs ]) tWorld__) (eJustIO (EVar tvrWorld2) vUnit)
            (False,ExtTypeVoid) -> fail "pure foreign function must return a valid value"
            _ -> do
                ExtTypeBoxed cn rtt' rtt <- lookupExtTypeInfo dataTable rt'
                [rtVar,rtVar'] <- newVars [rt',rtt']
                let _rttIO = ltTuple [tWorld__, rt']
                    rttIO' = ltTuple' [tWorld__, rtt']
                case isIO of
                    False -> cFun $ \rs -> (,) id $ eStrictLet rtVar' (prim rs rtt [ EVar t | t <- rs ] rtt') (ELit $ litCons { litName = cn, litArgs = [EVar rtVar'], litType = rt' })
                    True -> cFun $ \rs -> (,) (ELam tvrWorld) $
                                eCaseTup' (prim rs rtt (EVar tvrWorld:[EVar t | t <- rs ]) rttIO')  [tvrWorld2,rtVar'] (eLet rtVar (ELit $ litCons { litName = cn, litArgs = [EVar rtVar'], litType = rt' }) (eJustIO (EVar tvrWorld2) (EVar rtVar)))
        return [(toName Name.Val n,var,lamt result)]

    cDecl x@HsForeignDecl {} = fail ("Unsupported foreign declaration: "++ show x)

    cDecl (HsForeignExport _ ffi@FfiExport { ffiExportCName = ecn } n _) = do
        let name = ffiExportName ffi
        fn <- convertVar name
        tn <- convertVar (toName Name.Val n)

        (var,ty,lamt) <- convertValue name
        let (argTys,retTy') = argTypes' ty
            (isIO,retTy) = extractIO' retTy'

        --retCTy <- if retTy == tUnit
         --         then return unboxedTyUnit
         --         else liftM (\(_, _, x) -> rawType x) $ lookupCType' dataTable retTy

        aets <- forM argTys $ \ty -> do
            eti <- lookupExtTypeInfo dataTable ty
            ty' <- case eti of
                ExtTypeVoid -> fail "attempt to foreign export function with void argument"
                ExtTypeRaw _ -> do return ty
                ExtTypeBoxed _ ty' _  -> do return ty'
            [v] <- newVars [ty']
            e <- marshallFromC dataTable (EVar v) ty
            return (e,v,ty')


        let argEs   = [ e | (e,_,_) <- aets ]
            argTvrs = [ v | (_,v,_) <- aets ]
            argCTys = [ t | (_,_,t) <- aets ]
--        argCTys <- mapM (liftM (\(_,st,_) -> st) . lookupCType' dataTable) argTys

 --       argTvrs <- newVars argCTys
--        argEs <- sequence [(marshallFromC dataTable (EVar v) et) | v <- argTvrs | et <- argTys]

        fe <- actuallySpecializeE (EVar tn) ty
        let inner = foldl EAp fe argEs

        retE <- case isIO of
                  False -> marshallToC dataTable inner retTy
                  True -> do [world_, world__, ret] <- newVars [tWorld__, tWorld__, retTy]
                             retMarshall <- if retTy == tUnit
                                            then return (ELit (unboxedTuple []))
                                            else marshallToC dataTable (EVar ret) retTy
                             return (eLam world_ (eCaseTup' (eAp inner (EVar world_))
                                                            [world__, ret]
                                                            (ELit (unboxedTuple [EVar world__, retMarshall]))))

        let retCTy' = typeInfer dataTable retE

        -- trace ("retE: "++pprint retE) $ return ()

        let result = foldr ELam retE argTvrs

        realRetCTy:realArgCTys <- mapM (\x -> extTypeInfoExtType `liftM`  lookupExtTypeInfo dataTable x) (retTy:argTys)

        return [(name,
                 tvrInfo_u (Info.insert ffi { ffiExportArgTypes = realArgCTys, ffiExportRetType = realRetCTy } )
                           (fmap (const (foldr tFunc retCTy' argCTys)) $
                              setProperty prop_EXPORTED fn),
                 result)]

    cDecl (HsPatBind sl (HsPVar n) (HsUnGuardedRhs exp) []) | n == sillyName' = do
        e <- cExpr exp
        return [(v_silly,tvr,e)]
    cDecl (HsPatBind sl p rhs wh) | (HsPVar n) <- p = do
        let name = toName Name.Val n
        (var,ty,lamt) <- convertValue name
        rhs <- cRhs sl rhs
        lv <- hsLetE wh rhs
        return [(name,var,lamt lv)]
    cDecl (HsPatBind sl p rhs wh) | (HsPVar n) <- p = do
        let name = toName Name.Val n
        (var,ty,lamt) <- convertValue name
        rhs <- cRhs sl rhs
        lv <- hsLetE wh rhs
        return [(name,var,lamt lv)]

    cDecl (HsPatBind sl p rhs wh) | (HsPVar n) <- p = do
        let name = toName Name.Val n
        (var,ty,lamt) <- convertValue name
        rhs <- cRhs sl rhs
        lv <- hsLetE wh rhs
        return [(name,var,lamt lv)]
    cDecl (HsFunBind [(HsMatch sl n ps rhs wh)]) | all isHsPVar ps = do
        let name = toName Name.Val n
        (var,ty,lamt) <- convertValue name
        rhs <- cRhs sl rhs
        lv <- hsLetE wh rhs
        lps <- lp ps lv
        return [(name,var,lamt lps )]
    cDecl (HsFunBind ms@((HsMatch sl n ps _ _):_)) = do
        let name = toName Name.Val n
        (var,t,lamt) <- convertValue name
        let (targs,eargs) = argTypes t
            numberPatterns = length ps
        bs' <- newVars (take numberPatterns eargs)
        let bs  = map EVar bs'
            rt = discardArgs (length targs + numberPatterns) t
            z e = foldr eLam e bs'
        ms <- cMatchs bs (matchesConv ms) (ump sl rt)
        return [(name,var,lamt $ z ms )]
    cDecl HsNewTypeDecl {  hsDeclName = dname, hsDeclArgs = dargs, hsDeclCon = dcon, hsDeclDerives = derives } = return $ makeDerives dname dargs [dcon] (map (toName ClassName) derives)
    cDecl HsDataDecl {  hsDeclName = dname, hsDeclArgs = dargs, hsDeclCons = dcons, hsDeclDerives = derives } = return $ makeDerives dname dargs dcons (map (toName ClassName) derives)
    cDecl cd@(HsClassDecl {}) = cClassDecl cd
    cDecl _ = return []
    makeDerives dname dargs dcons derives  = concatMap f derives where
        f n | n == class_Bounded, all (null . hsConDeclArgs) dcons  = []
        f _ = []
    cExpr :: Monad m => HsExp -> Ce m E
    cExpr (HsAsPat n' (HsCon n)) = return $ constructionExpression dataTable (toName DataConstructor n) rt where
        t' = getAssump n'
        (_,rt) = argTypes' (tipe t')
    cExpr (HsLit (HsStringPrim s)) = return $ EPrim (APrim (PrimString (packString s)) mempty) [] r_bits_ptr_
    cExpr (HsLit (HsString s)) = return $ E.Values.toE s
    cExpr (HsAsPat n' (HsLit (HsIntPrim i))) = ans where
        t' = getAssump n'
        ans = return $ ELit (LitInt (fromIntegral i) (tipe t'))
    cExpr (HsAsPat n' (HsLit (HsInt i))) = ans where
        t' = getAssump n'
        ty = tipe t'
        -- XXX this can allow us to create integer literals out of things that
        -- arn't in Num if we arn't careful
        ans = case lookupExtTypeInfo dataTable ty of
            Just (ExtTypeBoxed cn st _) -> return $ ELit (litCons { litName = cn, litArgs = [ELit (LitInt (fromIntegral i) st)], litType = ty })
            _ -> return $ intConvert' funcs ty i
            --Just (cn,st,it) ->
    --cExpr (HsLit (HsInt i)) = return $ intConvert i
    cExpr (HsLit (HsChar ch)) = return $ toE ch
    cExpr (HsLit (HsFrac i))  = return $ toE i
    cExpr (HsLambda sl ps e) | all isHsPVar ps = do
        e <- cExpr e
        lp ps e
    cExpr (HsInfixApp e1 v e2) = do
        v <- cExpr v
        e1 <- cExpr e1
        e2 <- cExpr e2
        return $ eAp (eAp v e1) e2
    cExpr (HsLeftSection op e) = liftM2 eAp (cExpr op) (cExpr e)
    cExpr (HsApp (HsRightSection e op) e') = do
        op <- cExpr op
        e' <- cExpr e'
        e <- cExpr e
        return $ eAp (eAp op e') e
    cExpr (HsRightSection e op) = do
        cop <- cExpr op
        ce <- cExpr e
        let (_,TVr { tvrType = ty}:_) = fromPi (getType cop)
        [var] <- newVars [ty]
        return $ eLam var (eAp (eAp cop (EVar var)) ce)
    cExpr (HsApp e1 e2) = liftM2 eAp (cExpr e1) (cExpr e2)
    cExpr (HsParen e) = cExpr e
    cExpr (HsExpTypeSig _ e _) = cExpr e
    cExpr (HsNegApp e) = liftM doNegate (cExpr e)
    cExpr (HsLet dl e) = hsLet dl e
    cExpr (HsIf e a b) = join $ liftM3 createIf (cExpr e) (cExpr a) (cExpr b)
    cExpr (HsCase _ []) = error "empty case"
    cExpr (HsAsPat n HsError { hsExpString = msg }) = do
        ty <- convertTyp (toName Name.Val n)
        return $ EError msg ty
    cExpr (HsAsPat n hs@(HsCase e alts)) = do
        ty <- convertTyp (toName Name.Val n)
        scrut <- cExpr e
        cMatchs [scrut] (altConv alts) (EError ("No Match in Case expression at " ++ show (srcLoc hs))  ty)
    cExpr (HsTuple es) = liftM eTuple (mapM cExpr es)
    cExpr (HsUnboxedTuple es) = liftM eTuple' (mapM cExpr es)
    cExpr (HsAsPat n (HsList xs)) = do
        ty <- convertTyp (toName Name.Val n)
        let cl (x:xs) = liftM2 eCons (cExpr x) (cl xs)
            cl [] = return $ eNil ty
        cl xs
    cExpr (HsVar n) = do
        t <- convertVar (toName Name.Val n)
        return (EVar t)
    cExpr (HsAsPat n' e) = do
        e <- cExpr e
        cc <- asks ceCoerce
        case Map.lookup (toName Val n') cc of
            Nothing -> return e
            Just c -> applyCoersion c e
    cExpr e = fail ("Cannot convert: " ++ show e)
    hsLetE [] e = return  e
    hsLetE dl e = do
        nds <- mconcatMapM cDecl dl
        return $ eLetRec [ (b,c) | (_,b,c) <- nds] e
    hsLet dl e = do
        e <- cExpr e
        hsLetE dl e

    cMatchs :: Monad m => [E] -> [([HsPat],HsRhs,[HsDecl])] -> E -> Ce m E
    cMatchs bs ms els = do
        pg <- processGuards ms
        convertMatches bs pg els

    cGuard (HsUnGuardedRhs e) = liftM const $ cExpr e
    cGuard (HsGuardedRhss (HsGuardedRhs _ g e:gs)) = do
        g <- cExpr g
        e <- cExpr e
        fg <- cGuard (HsGuardedRhss gs)
        [nv] <- newVars [Unknown]
        return (\els -> createIfv nv g e (fg els))
    cGuard (HsGuardedRhss []) = return id

    getAssump n  = case Map.lookup (toName Name.Val n) assumps of
        Just z -> z
        Nothing -> error $ "Lookup failed: " ++ (show n)
    lp  [] e = return e
    lp  (HsPVar n:ps) e = do
        v <- convertVar (toName Name.Val n)
        eLam v `liftM` lp ps e
    lp  p e  =  error $ "unsupported pattern:" <+> tshow p  <+> tshow e
    cRhs sl (HsUnGuardedRhs e) = cExpr e
    cRhs sl (HsGuardedRhss []) = error "HsGuardedRhss: empty"
    cRhs sl (HsGuardedRhss gs@(HsGuardedRhs _ _ e:_)) = f gs where
        f (HsGuardedRhs _ g e:gs) = join $ liftM3 createIf (cExpr g) (cExpr e) (f gs)
        f [] = do
            e <- cExpr e
            return $ ump sl $ getType e
    processGuards xs = flip mapM xs $ \ (ps,e,wh) -> do
        cg <- cGuard e
        nds <- mconcatMapM cDecl wh
        let elet = eLetRec [ (b,c) | (_,b,c) <- nds]
        return (ps,elet . cg )

    cClassDecl (HsClassDecl _ (HsQualType _ (HsTyApp (HsTyCon name) _)) decls) = do
        props <- asks ceProps
        let cr = findClassRecord classHierarchy className
            className = (toName ClassName name)
            cClass classRecord =  [ f n (toId n) (removeNewtypes dataTable $ tipe t) | (n,t) <- classAssumps classRecord ] where
                f n i t = (n,setProperties [prop_METHOD,prop_PLACEHOLDER] $ tVr i t, foldr ELam (EPrim (primPrim ("Placeholder: " ++ show n)) [] ft) args)  where
                    (ft',as) = fromPi t
                    (args,rargs) = case mlookup i props of
                        Just p | getProperty prop_NOETA p -> span (sortKindLike . getType) as
                        _ -> (as,[])
                    ft = foldr EPi ft' rargs
        return (cClass cr ++ primitiveInstances className)
    cClassDecl _ = error "cClassDecl"

convertVar n = do
    (t,_,_) <- convertValue n
    return t
convertTyp n = do
    (_,t,_) <- convertValue n
    return t


toTVr assumps dataTable n = tVr (toId n) typeOfName where
    typeOfName = case Map.lookup n assumps of
        Just z -> removeNewtypes dataTable (tipe z)
        Nothing -> error $ "convertVal.Lookup failed: " ++ (show n)



integer_cutoff = 500000000

intConvert i | abs i > integer_cutoff  =  ELit (litCons { litName = dc_Integer, litArgs = [ELit $ LitInt (fromInteger i) r_bits_max_], litType = tInteger })
intConvert i =  ELit (litCons { litName = dc_Int, litArgs = [ELit $ LitInt (fromInteger i) r_bits32], litType = tInt })

intConvert' funcs typ i = EAp (EAp fun typ) (ELit (litCons { litName = con, litArgs = [ELit $ LitInt (fromInteger i) rawtyp], litType = ltype }))  where
    (con,ltype,fun,rawtyp) = case abs i > integer_cutoff of
        True -> (dc_Integer,tInteger,f_fromInteger,r_bits_max_)
        False -> (dc_Int,tInt,f_fromInt,r_bits32)
    f_fromInt = func_fromInt funcs
    f_fromInteger = func_fromInteger funcs

litconvert (HsChar i) t | t == tChar =  LitInt (fromIntegral $ ord i) tCharzh
litconvert (HsCharPrim i) t | t == tCharzh =  LitInt (fromIntegral $ ord i) tCharzh
litconvert (HsIntPrim i) t  =  LitInt (fromIntegral $  i) t
litconvert e t = error $ "litconvert: shouldn't happen: " ++ show (e,t)


fromHsPLitInt (HsPLit l@(HsInt _)) = return l
fromHsPLitInt (HsPLit l@(HsFrac _)) = return l
fromHsPLitInt x = fail $ "fromHsPLitInt: " ++ show x

{-
patVar ::
    Monad m
    => HsPat -- ^ the pattern
    -> E     -- ^ the type of the expression
    -> Ce m (HsPat,TVr)  -- ^ a new pattern and a binding variable
patVar HsPWildCard t = return (HsPWildCard,tvr { tvrType = t })
patVar (HsPVar n) t | isTypePlaceholder n = return (HsPWildCard,tvr { tvrType = t })
patVar (HsPAsPat n p) t | not (isTypePlaceholder n) = do
    nn <- convertVar (toName Name.Val n)
    return (p,nn)
patVar (HsPAsPat n p) t | isTypePlaceholder n = patVar p t
patVar p t = do
    [nv] <- newVars [t]
    return (p,nv)
    -}


tidyPat ::
    Monad m
    => HsPat
    -> E
    -> Ce m (HsPat,E -> E)
tidyPat p b = f p where
    f HsPWildCard = return (HsPWildCard,id)
    f (HsPVar n) | isTypePlaceholder n = return (HsPWildCard,id)
    f (HsPAsPat n p) | isTypePlaceholder n = f p
    f (HsPTypeSig _ p _) = f p
    f p@HsPLit {} = return (p,id)
    f (HsPVar n) = do
        v <- convertVar (toName Name.Val n)
        return (HsPWildCard,if EVar v /=  b then eLet v b else id)
    f (HsPAsPat n p) = do
        (p',g') <- f p
        v <- convertVar (toName Name.Val n)
        return (p',(if EVar v /= b then eLet v b else id) . g')
    f pa@(HsPApp n [p]) = do
        dataTable <- getDataTable
        patCons <- getConstructor (toName DataConstructor n) dataTable
        case conAlias patCons of
            ErasedAlias -> f p
            _ -> return (pa,id)
    f p@HsPApp {} = return (p,id)
    f (HsPIrrPat (Located ss p)) = f p >>= \ (p',fe) -> case p' of
        HsPWildCard -> return (p',fe)
        _ -> do
            (lbv,bv) <- varify b
            let f n = do
                v <- convertVar (toName Name.Val n)
                fe <- convertMatches [bv] [([p],const (EVar v))] (EError (show ss ++ ": Irrefutable pattern match failed") (getType v))
                return (v,fe)
            zs <- mapM f (getNamesFromHsPat p)
            return (HsPWildCard,lbv . eLetRec zs)
    f ~(HsPBangPat (Located ss (HsPAsPat v p))) = do
        (p',fe) <- f p
        v <- convertVar (toName Name.Val v)
        return (p',eStrictLet v b . fe)

-- converts a value to an updatable closure if it isn't one already.
varify b@EVar {} = return (id,b)
varify b = do
    [bv] <- newVars [getType b]
    return (eLet bv b,EVar bv)

tidyHeads ::
    Monad m
    => E
    -> [([HsPat],E->E)]  -- [(pats,else -> value)]
    -> Ce m [(HsPat,[HsPat],E->E)]  -- pulls the head off of each pattern, tidying it up perhaps
tidyHeads b ps = mapM f ps where
    f (~(p:ps),fe) = do
        (p',fe') <- tidyPat p b
        return (p',ps,fe' . fe)


convertMatches ::
    Monad m
    => [E]               -- input expressions we are matching against.
    -> [([HsPat],E->E)]  -- [(pats,else -> value)]
    -> E                 -- else, what to do if nothing matches
    -> Ce m E
convertMatches bs ms err = do
    assumps <- asks ceAssumps
    dataTable <- getDataTable
    funcs <- asks ceFuncs
    let fromInt = func_fromInt funcs
        fromInteger = func_fromInteger funcs
        fromRational = func_fromRational funcs
        isJoinPoint (EAp (EVar x) _) | getProperty prop_JOINPOINT x = True
        isJoinPoint _ = False

        match :: Monad m => [E] -> [([HsPat],E->E)] -> E -> Ce m E
        -- when we run out of arguments, we should run out of patterns. simply fold the transformers.
        match  [] ps err = return $ foldr f err ps where f ([],fe) err = fe err
        -- when we are out of patterns, return the error term
        match _ [] err = return err
        match ~(b:bs) ps err = do
            (b',mf) <- if isEVar b   then return (b,id) else do
                [ev] <- newVars [getType b]
                return $ (EVar ev, eLet ev b)
            pps <- tidyHeads b' ps
            let patternGroups = groupUnder (isHsPWildCard . fst3) pps
                f [] err = return err
                f (ps:pss) err = do
                    err' <- f pss err
                    if isEVar err' || isEError err' || isJoinPoint err' then matchGroup b' bs ps err' else do
                        [ev] <- newVars [EPi tvr { tvrType = unboxedTyUnit } $ getType err']
                        let ev' = setProperties [prop_ONESHOT, prop_JOINPOINT] ev
                        nm <- matchGroup b' bs ps (EAp (EVar ev') unboxedUnit)
                        return $ eLetRec [(ev',ELam (setProperty prop_ONESHOT tvr { tvrType = unboxedTyUnit }) err')] nm
            liftM mf $ f patternGroups err
        matchGroup b bs ps err
            | all (isHsPWildCard . fst3) ps = match bs [ (ps,e) | (_,ps,e) <- ps] err
            | Just () <- mapM_ (fromHsPLitInt . fst3) ps = do
                let tb = getType b
                (lbv,bv) <- varify b
                let gps = [ (p,[ (ps,e) |  (_,ps,e) <- xs ]) | (p,xs) <- sortGroupUnderF fst3 ps]
                    eq = EAp (func_equals funcs) tb
                    f els (HsPLit (HsInt i),ps) = do
                        let ip | abs i > integer_cutoff  = (EAp (EAp fromInteger tb) (intConvert i))
                               | otherwise =  (EAp (EAp fromInt tb) (intConvert i))
                        m <- match bs ps err
                        createIf (EAp (EAp eq bv) ip) m els
                    f els ~(HsPLit (HsFrac i),ps) = do
                        let ip = (EAp (EAp fromRational tb) (toE i))
                        m <- match bs ps err
                        createIf (EAp (EAp eq bv) ip) m els
                e <- foldlM f err gps
                return $ lbv e
            | all (isHsPString . fst3) ps = do
                (lbv,bv) <- varify b
                (eqString,_,_) <- convertValue v_eqString
                (eqUnpackedString,_,_) <- convertValue v_eqUnpackedString
                let gps = [ (p,[ (ps,fe) |  (_,ps,fe) <- xs ]) | (p,xs) <- sortGroupUnderF fst3 ps]
                    f els (HsPLit (HsString ""),ps) = do
                        m <- match bs ps err
                        return $ eCase bv [Alt (litCons { litName = dc_EmptyList, litType = tString }) m] els
                    f els ~(HsPLit (HsString s),ps) = do
                        m <- match bs ps err
                        let (s',packed) = packupString s
                        if packed
                            then return $ ifzh (EAp (EAp (EVar eqUnpackedString) s') bv) m els
                            else return $ ifzh (EAp (EAp (EVar eqString) s') bv) m els
                e <- foldlM f err gps
                return $ lbv e
            | all (isHsPLit . fst3) ps = do
                let gps = [ (p,[ (ps,fe) |  (_,ps,fe) <- xs ]) | (p,xs) <- sortGroupUnderF fst3 ps]
                    f (~(HsPLit l),ps) = do
                        m <- match bs ps err
                        return (Alt (litconvert l (getType b)) m)
                as@(_:_) <- mapM f gps
                [TVr { tvrIdent = vr }] <- newVars [Unknown]
                return $ unbox dataTable b vr $ \tvr -> eCase tvr as err
            | Just ps <- mapM pappConvert ps = do
                let gps =  sortGroupUnderF (hsPatName . fst3) ps
                    (Just patCons) = getConstructor (toName DataConstructor $ fst $ head gps) dataTable
                    f (name,ps) = do
                        let spats = hsPatPats $ fst3 (head ps)
                            _nargs = length spats
                        vs <- newVars (slotTypesHs dataTable (toName DataConstructor name) (getType b))
                        ps' <- mapM pp ps
                        m <- match (map EVar vs ++ bs) ps' err
                        deconstructionExpression dataTable (toName DataConstructor name) (getType b) vs m
                    pp (~(HsPApp n ps),rps,e)  = do
                        return $ (ps ++ rps , e)
                as@(_:_) <- mapM f gps
                case conVirtual patCons of
                    Nothing -> return $ eCase b as err
                    Just sibs -> do
                        let (Just Constructor { conChildren = DataNormal [vCons] }) = getConstructor (conInhabits patCons) dataTable
                            (Just Constructor { conOrigSlots = [SlotNormal rtype] }) = getConstructor vCons dataTable
                        [z] <- newVars [rtype]
                        let err' = if length sibs <= length as then Unknown else err
                        return $ eCase b [Alt litCons { litName = vCons, litArgs = [z], litType = getType b } (eCase (EVar z) as err')] Unknown
            | otherwise = error $ "Heterogenious list: " ++ show (map fst3 ps)
        pappConvert (p@HsPApp {},x,y) = return (p,x,y)
        pappConvert (HsPLit (HsString ""),ps,b) = return (HsPApp (nameName $ dc_EmptyList) [],ps,b)
        pappConvert (HsPLit (HsString (c:cs)),ps,b) = return (HsPApp (nameName $ dc_Cons) [HsPLit (HsChar c),HsPLit (HsString cs)],ps,b)
        pappConvert _ = fail "pappConvert"
        isHsPString (HsPLit HsString {}) = True
        isHsPString _ = False
    match bs ms err

packupString :: String -> (E,Bool)
packupString s | all (\c -> c > '\NUL' && c <= '\xff') s = (EPrim (APrim (PrimString (packString s)) mempty) [] r_bits_ptr_,True)
packupString s = (toE s,False)


actuallySpecializeE :: Monad m
    => E   -- ^ the general expression
    -> E   -- ^ the specific type
    -> m E -- ^ the specialized value
actuallySpecializeE ge st = do
    -- trace (pprint (ge, getType ge, st)) $ return ()
    liftM (foldl EAp ge)
          (specializeE (getType ge) st)

specializeE :: Monad m
    => E   -- ^ the general type
    -> E   -- ^ the specific type
    -> m [E]  -- ^ what to apply the general type to to get the specific one
specializeE gt st = do
    let f zs x | Just mm <- match (const Nothing) zs x st = mapM (g mm) (reverse zs) where
            g mm tvr = case lookup tvr mm of
                Just x -> return x
                Nothing -> fail $ "specializeE: variable not bound: " ++ pprint (((gt,st),(mm,tvr)),(zs,x))
        f zs (EPi vbind exp) = f (vbind:zs) exp
        f _ _ = fail $ render (text "specializeE: attempt to specialize types that do not unify:"
                               <$> pprint (gt,st)
                               <$> tshow  gt
                               <$> tshow st)
    f [] gt


procAllSpecs :: Monad m => DataTable -> [Type.Rule] -> [(TVr,E)] -> m ([(TVr,E)],Rules)
procAllSpecs dataTable rs ds = do
    let specMap = Map.fromListWith (++) [ (toId n,[r]) | r@Type.RuleSpec { Type.ruleName = n } <- rs]
        f (t,e) | Just rs <- Map.lookup (tvrIdent t) specMap = do
            hs <- mapM (makeSpec dataTable (t,e)) rs
            return (unzip hs)
        f _ = return mempty
    (nds,rules) <- mconcat `liftM` mapM f ds
    return $ (nds,fromRules rules)


makeSpec :: Monad m => DataTable -> (TVr,E) -> T.Rule -> m ((TVr,E),Rule)
makeSpec dataTable (t,e) T.RuleSpec { T.ruleType = rt, T.ruleUniq = (Module m,ui), T.ruleSuper = ss } = do
    let nt = removeNewtypes dataTable $ tipe rt
    as <- specializeE (getType t) nt
    let ntvr = tvr { tvrIdent = toId newName, tvrType = getType nbody, tvrInfo = setProperties (prop_SPECIALIZATION:sspec) mempty }
        Just nn = fromId (tvrIdent t)
        (ntype,Just m,q) = nameParts nn
        newName = toName ntype (Just $ "Spec@." ++ m ++ "." ++ show ui,'f':m ++ "." ++ q)
        sspec = if ss then [prop_SUPERSPECIALIZE] else []
        ar = makeRule ("Specialize.{" ++ show newName) (Module m,ui) RuleSpecialization bvars t as (foldl eAp (EVar ntvr) (map EVar bvars)) 
        bvars = nub $ freeVars as
        nbody = foldr ELam (foldl EAp e as) bvars
    return ((ntvr,nbody),ar)
makeSpec _ _ _ = fail "E.FromHs.makeSpec: invalid specialization"


deNewtype :: DataTable -> E -> E
deNewtype dataTable e = removeNewtypes dataTable (f e) where
    f ECase { eCaseScrutinee = e, eCaseAlts =  ((Alt (LitCons { litName = n, litArgs = [v], litType = t }) z):_) } | alias == ErasedAlias = f (eLet v e z) where
        Identity Constructor { conAlias = alias } = getConstructor n dataTable
    f ECase { eCaseScrutinee = e, eCaseAlts =  ((Alt (LitCons { litName = n, litArgs = [v], litType = t }) z):_) } | alias == RecursiveAlias = f $ eLet v (prim_unsafeCoerce e (getType v)) z where
        Identity Constructor { conAlias = alias } = getConstructor n dataTable
    f e = runIdentity $ emapE (return . f) e



