module E.FromHs(
    convertDecls,
    convertRules,
    createInstanceRules,
    createMethods,
    makeSpec,
    getMainFunction
    ) where

import Char
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.RWS
import Data.FunctorM
import Data.Generics
import Data.Monoid
import List(isPrefixOf)
import Maybe
import Prelude hiding((&&),(||),not,and,or,any,all,head)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.PrettyPrint.HughesPJ as PPrint

import Atom
import Boolean.Algebra
import Class
import C.Prims as CP
import DataConstructors
import PrimitiveOperators
import Doc.DocLike
import Doc.PPrint
import E.E
import E.Eval(eval)
import E.Eta
import E.LetFloat(atomizeAp)
import E.Program
import E.PrimOpt
import E.Rules
import E.Subst
import E.Traverse
import E.TypeAnalysis
import E.TypeCheck
import E.Values
import Fixer.VMap
import FrontEnd.KindInfer(hoistType)
import FrontEnd.Rename(unRename)
import FrontEnd.SrcLoc
import FrontEnd.Tc.Type hiding(Rule(..), unbox)
import FrontEnd.Tc.Type(prettyPrintType)
import FrontEnd.TiData
import FrontEnd.Utils
import GenUtil
import HsSyn as HS
import Info.Types
import Name.Name as Name
import Name.Names
import Name.VConsts
import Options
import qualified FlagOpts as FO
import qualified FrontEnd.Tc.Monad as TM
import qualified FrontEnd.Tc.Type as T(Rule(..))
import qualified Info.Info as Info
import qualified Stats
import qualified Util.Seq as Seq
import Representation
import Support.CanType
import Support.FreeVars
import Util.Gen
import Util.NameMonad

theMainName = toName Name.Val "theMain"
ump sl e = EError (show sl ++ ": Unmatched pattern") e


head (x:_) = x
head _ = error "FromHsHeadError"

--newVars :: MonadState Int m => [E] -> m [TVr]
newVars xs = f xs [] where
    f [] xs = return $ reverse xs
    f (x:xs) ys = do
        s <- newUniq
        f xs (tVr (2*s) x:ys)


tipe t = f t where
    f (TAp t1 t2) = eAp (f t1) (f t2)
    f (TArrow t1 t2) =  EPi (tVr 0 (f t1)) (f t2)
    f (TCon (Tycon n k)) | n == tc_World__ =  ELit (LitCons rt_Worldzh [] eHash)
    f (TCon (Tycon n k)) =  ELit (LitCons n [] (kind k))
    f (TVar Tyvar { tyvarRef = Just {}, tyvarKind = k}) = tAbsurd (kind k)
    f (TVar tv) = EVar (cvar [] tv)
    f (TMetaVar mv) = cmvar mv
    f (TGen _ (Tyvar _ n k _)) = EVar (tVr (lt n) (kind k))
    f (TForAll vs (ps :=> t)) = foldr EPi (f t) (map (cvar $ freeVars ps) vs)
    f (TExists xs (_ :=> t)) = let
        xs' = map (kind . tyvarKind) xs
        in ELit (LitCons (unboxedNameTuple TypeConstructor (length xs' + 1)) (f t:xs') eHash)
    cvar _ Tyvar { tyvarRef = Just {}, tyvarKind = k}= error $ "tyvar is metaref:" ++ prettyPrintType t
    cvar fvs tv@Tyvar { tyvarName = n, tyvarKind = k }
        | tv `elem` fvs = setProperty prop_SCRUTINIZED (tVr (lt n) (kind k))
        | otherwise = tVr (lt n) (kind k)
    cmvar MetaVar { metaKind = k } = tAbsurd (kind k)
    lt n | nameType n == TypeVal = toId n  -- verifies namespace

kind Star = eStar
kind (Kfun k1 k2) = EPi (tVr 0 (kind k1)) (kind k2)
kind (KVar _) = error "Kind variable still existing."


simplifyDecl (HsPatBind sl (HsPVar n)  rhs wh) = HsFunBind [HsMatch sl n [] rhs wh]
simplifyDecl x = x

simplifyHsPat (HsPInfixApp p1 n p2) = HsPApp n [simplifyHsPat p1, simplifyHsPat p2]
simplifyHsPat (HsPParen p) = simplifyHsPat p
simplifyHsPat (HsPTuple ps) = HsPApp (toTuple (length ps)) (map simplifyHsPat ps)
simplifyHsPat (HsPNeg p)
    | HsPLit (HsInt i) <- p' = HsPLit $ HsInt (negate i)
    | HsPLit (HsFrac i) <- p' = HsPLit $ HsFrac (negate i)
    | otherwise = HsPNeg p'
    where p' = (simplifyHsPat p)
simplifyHsPat (HsPLit (HsString s)) = simplifyHsPat (HsPList (map f s)) where
    f c = HsPLit (HsChar c)
simplifyHsPat (HsPAsPat n p) = HsPAsPat n (simplifyHsPat p)
simplifyHsPat (HsPTypeSig _ p _) = simplifyHsPat p
simplifyHsPat (HsPList ps) = pl ps where
    pl [] = HsPApp (nameName $ dc_EmptyList) []
    pl (p:xs) = HsPApp (nameName $ dc_Cons) [simplifyHsPat p, pl xs]
simplifyHsPat (HsPApp n xs) = HsPApp n (map simplifyHsPat xs)
simplifyHsPat (HsPIrrPat p) = simplifyHsPat p -- TODO irrefutable patterns!
simplifyHsPat p@HsPVar {} = p
simplifyHsPat p@HsPLit {} = p
simplifyHsPat p = error $ "simplifyHsPat: " ++ show p


fromTyvar (Tyvar _ n k _) = tVr (toId n) (kind k)

fromSigma (TForAll vs (_ :=> t)) = (map fromTyvar vs, tipe t)
fromSigma t = ([], tipe t)

convertValue n = do
    assumps <- asks ceAssumps
    t <- Map.lookup n assumps
    let ty = tipe t
    cc <- asks ceCoerce
    lm <- case Map.lookup n cc of
        Nothing -> do
            let (vs,_) = fromSigma t
            return (flip (foldr eLam) vs)
        Just CTId -> do return id
        Just (CTAbs ts) -> do return $ \e -> foldr eLam e (map fromTyvar ts)
    return (tVr (toId n) ty,ty,lm)

lookupCoercion n = do
    assumps <- asks ceAssumps
    cs <- asks ceCoerce
    case Map.lookup n assumps of
        Just x -> return (Left x)
        Nothing -> Right `liftM` Map.lookup n cs


convertVal assumps n = (foldr ePi t vs, flip (foldr eLam) vs) where
    (vs,t) = case Map.lookup n assumps of
        Just z -> fromSigma  z
        Nothing -> error $ "convertVal.Lookup failed: " ++ (show n)

convertOneVal (Forall _ (_ :=> t)) = (mp EPi ts (tipe t)) where
    mp fn (((Tyvar _ n k _)):rs) t = fn (tVr (toId n) (kind k)) (mp fn rs t)
    mp _ [] t = t
    ts = ctgen t

toTVr assumps n = tVr (toId n) (typeOfName n) where
    typeOfName n = fst $ convertVal assumps n

matchesConv ms = map v ms where
    v (HsMatch _ _ ps rhs wh) = (map simplifyHsPat ps,rhs,wh)

altConv as = map v as where
    v (HsAlt _ p rhs wh) = ([simplifyHsPat p],rhs,wh)

argTypes e = span ((== eBox) . getType) (map tvrType xs) where
    (_,xs) = fromPi e
argTypes' :: E -> ([E],E)
argTypes' e = let (x,y) = fromPi e in (map tvrType y,x)


getMainFunction :: Monad m => DataTable -> Name -> (Map.Map Name (TVr,E)) -> m (Name,TVr,E)
getMainFunction dataTable name ds = ans where
    ans = do
        main <- findName name
        runMain <- findName (func_runMain sFuncNames)
        runExpr <- findName (func_runExpr sFuncNames)
        runNoWrapper <- findName (func_runNoWrapper sFuncNames)
        let e = case ioLike (getType maine) of
                Just x | not (fopts FO.Wrapper) -> EAp (EAp (EVar runNoWrapper) x) maine
                Just x ->  EAp (EAp (EVar runMain)  x ) maine
                Nothing ->  EAp (EAp (EVar runExpr) ty) maine
            theMain = (theMainName,setProperty prop_EXPORTED theMainTvr,e)
            theMainTvr =  tVr (toId theMainName) (infertype dataTable e)
            tvm@(TVr { tvrType =  ty}) =  main
            maine = foldl EAp (EVar tvm) [ tAbsurd k |  TVr { tvrType = k } <- xs, sortStarLike k ]
            (ty',xs) = fromPi ty
        return theMain
    ioLike ty = case followAliases dataTable ty of
        ELit (LitCons n [x] _) | n ==  tc_IO -> Just x
        (EPi ioc (EPi tvr (ELit (LitCons n [x] _)))) | n == tc_IOResult -> Just x
        _ -> Nothing
    findName name = case Map.lookup name ds of
        Nothing -> fail $ "Cannot find: " ++ show name
        Just (n,_) -> return n

createInstanceRules :: Monad m => ClassHierarchy -> (Map.Map Name (TVr,E)) -> m Rules
createInstanceRules classHierarchy funcs = return $ fromRules ans where
    ans = concatMap cClass (classRecords classHierarchy)
    cClass classRecord =  concat [ method classRecord n | n :>: Forall _ (_ :=> t) <- classAssumps classRecord ]
    method classRecord methodName | isJust _methodName = as where
        methodVar = tVr (toId methodName) ty
        _methodName@(~(Just (TVr {tvrType = ty},_))) = findName methodName
        defaultName =  (defaultInstanceName methodName)
        valToPat' (ELit (LitCons x ts t)) = ELit $ LitCons x [ EVar (tVr j (getType z)) | z <- ts | j <- [2,4 ..], j `notElem` map tvrIdent args]  t
        valToPat' (EPi (TVr { tvrType =  a}) b)  = ELit $ LitCons tc_Arrow [ EVar (tVr j (getType z)) | z <- [a,b] | j <- [2,4 ..], j `notElem` map tvrIdent args]  eStar
        valToPat' x = error $ "FromHs.valToPat': " ++ show x
        as = [ rule  t | (_ :=> IsIn _ t ) <- snub (classInsts classRecord) ]
        (_ft,_:args') = fromPi ty
        (args,_rargs) = span (sortStarLike . getType)  args'
        rule t = emptyRule { ruleHead = methodVar, ruleArgs = valToPat' (tipe t):map EVar args, ruleBinds = [ t | ~(EVar t) <- vs] ++ args, ruleBody = body, ruleUniq = (Module (show name),0), ruleName = toAtom $ "Rule.{" ++ show name ++ "}"}  where
            name = (instanceName methodName (getTypeCons t))
            vp@(ELit LitCons { litArgs =  vs }) = valToPat' (tipe t)
            body = case findName name of
                Just (n,_) -> foldl EAp (EVar n) (vs ++ map EVar args)
                Nothing -> case findName defaultName of
                    Just (deftvr,_) | null vs -> foldl EAp (EAp (EVar deftvr) vp) (map EVar args)
                    Just (deftvr,_) -> eLet tv vp $ foldl EAp (EAp (EVar deftvr) (EVar tv)) (map EVar args) where
                        tv = tvr { tvrIdent = head [ n | n <- [2,4..], n `notElem` freeVars vp], tvrType = getType vp }
                    Nothing -> foldl EAp (EError ( show methodName ++ ": undefined at type " ++  PPrint.render (pprint t)) (eAp ty (valToPat' (tipe t)))) (map EVar args)
    method _ _ = []
    findName name = case Map.lookup name funcs of
        Nothing -> fail $ "Cannot find: " ++ show name
        Just n -> return n

createMethods :: Monad m => DataTable -> ClassHierarchy -> (Map.Map Name (TVr,E))  -> m [(Name,TVr,E)]
createMethods dataTable classHierarchy funcs = return ans where
    ans = concatMap cClass (classRecords classHierarchy)
    cClass classRecord =  concat [ method classRecord n | n :>: _ <- classAssumps classRecord ]
    method classRecord methodName | isJust _methodTVr = [(methodName ,setProperty prop_METHOD (tVr (toId methodName) ty),v)] where
        theDefault = findName (defaultInstanceName methodName)
        _methodTVr@(~(Just (TVr {tvrType = ty},ELam TVr { tvrInfo = nfo } _))) = findName methodName
        Just (vmap::Typ) = Info.lookup nfo
        (EPi tvr finalType) = ty
        v = eLam tvr (foldr ELam emptyCase { eCaseScrutinee = EVar tvr, eCaseAlts = as, eCaseBind = tvr { tvrIdent = 0 }, eCaseType = foldr EPi ft rargs } args)
        as = concatMap cinst [ t | (_ :=> IsIn _ t ) <- classInsts classRecord]
        (ft,args') = fromPi finalType
        (args,rargs) = span (sortStarLike . getType)  args'
        cinst t | Nothing <- getConstructor x dataTable = fail "skip un-imported primitives"
                | not $ x `vmapMember` vmap = fail "unused instance"
                | Just (tvr,_) <- findName name  = return $ calt (foldl EAp (EVar tvr) (vs ++ map EVar args))
                | Just (deftvr,defe) <- theDefault = return $ calt $ eLet tvr (tipe t) (foldl EAp (EVar deftvr) (EVar tvr:map EVar args))
                | otherwise  = return $ calt $  EError ( show methodName ++ ": undefined at type " ++  PPrint.render (pprint t)) errType
            where
            name = (instanceName methodName (getTypeCons t))
            calt e =  Alt (LitCons x [ case e of EVar tvr -> tvr; _ -> error $ "createMethods: "++ show e | e <- vs ]  ct)  e
            errType = subst tvr (tipe t) finalType
            (x,vs,ct) = case tipe t of
                (ELit (LitCons x' vs' ct')) -> (x',vs',ct')
                (EPi (TVr { tvrType = a}) b) -> (tc_Arrow,[a,b],eStar)
                e -> error $ "FromHs.createMethods: " ++ show e
    method _ _ = []
    findName name = case Map.lookup name funcs of
        Nothing -> fail $ "Cannot find: " ++ show name
        Just n -> return n

methodNames ::  ClassHierarchy ->  [TVr]
methodNames  classHierarchy =  ans where
    ans = concatMap cClass (classRecords classHierarchy)
    cClass classRecord =  [ setProperty prop_METHOD $ tVr (toId n) (convertOneVal t) | n :>: t <- classAssumps classRecord ]

unbox :: DataTable -> E -> Int -> (TVr -> E) -> E
unbox dataTable e vn wtd = eCase e [Alt (LitCons cna [tvra] te) (wtd tvra)] Unknown where
    te = getType e
    tvra = tVr vn sta
    Just (cna,sta,ta) = lookupCType' dataTable te

createFunc :: UniqueProducer m => DataTable -> [E] -> ([(TVr,String)] -> (E -> E,E)) -> m E
createFunc dataTable es ee = do
    xs <- flip mapM es $ \te -> do
        res@(_,sta,rt) <- lookupCType' dataTable te
        [n,n'] <- newVars [te,sta]
        return (n,(n',rt),res)
    let tvrs' = [ n' | (_,n',_) <- xs ]
        tvrs = [ t | (t,_,_) <- xs]
        (me,innerE) = ee tvrs'
        eee = me $ foldr esr innerE xs
        esr (tvr,(tvr',_),(cn,_,_)) e = eCase (EVar tvr) [Alt (LitCons cn [tvr'] te) e] Unknown  where
            te = getType $ EVar tvr
    return $ foldr ELam eee tvrs

instance GenName String where
   genNames i = map (('x':) . show) [i..]

convertRules :: TiData -> ClassHierarchy -> Map.Map Name Type -> DataTable -> [HsDecl] -> IO [(String,[TVr],E,E)]
convertRules tiData classHierarchy assumps dataTable hsDecls = concatMapM f hsDecls where
    f pr@HsPragmaRules {} = do
        let ce = convertE tiData classHierarchy assumps dataTable (hsDeclSrcLoc pr)
        e1 <- ce (hsDeclLeftExpr pr)
        e2 <- ce (hsDeclRightExpr pr)
        (ts,cs) <- runNameMT $ do
            ts <- flip mapM (filter (sortStarLike . getType) $ freeVars e1) $ \tvr -> do
                --return (tvrIdent tvr,tvr)
                nn <- newNameFrom (map (:'\'':[]) ['a' ..])
                return (tvrIdent tvr,tvr { tvrIdent = toId (toName TypeVal nn) })
            cs <- flip mapM [toTVr assumps (toName Val v) | (v,_) <- hsDeclFreeVars pr ] $ \tvr -> do
                let ur = show $ unRename $ nameName (toUnqualified $ runIdentity $ fromId (tvrIdent tvr))
                nn <- newNameFrom (ur:map (\v -> ur ++ show v) [1 ::Int ..])
                return (tvrIdent tvr,tvr { tvrIdent = toId (toName Val nn) })
            return (ts,cs)
        let smt = substMap $ Map.fromList [ (x,EVar y)| (x,y) <- ts ]
            sma = substMap $ Map.fromList [ (x,EVar y)| (x,y) <- cs' ]
            cs' =  [ (x,(tvrType_u smt y))| (x,y) <- cs ]
            e2' = deNewtype dataTable $ smt $ sma e2
        e2 <- atomizeAp False dataTable Stats.theStats mainModule e2'
        return [(hsDeclString pr,( snds (cs' ++ ts) ),eval $ smt $ sma e1,e2)]
    f _ = return []

convertE :: Monad m => TiData -> ClassHierarchy -> Map.Map Name Type -> DataTable -> SrcLoc -> HsExp -> m E
convertE tiData classHierarchy assumps dataTable srcLoc exp = do
    [(_,_,e)] <- convertDecls tiData classHierarchy assumps dataTable [HsPatBind srcLoc (HsPVar sillyName') (HsUnGuardedRhs exp) []]
    return e

sillyName' = nameName v_silly

data CeEnv = CeEnv {
    ceAssumps :: Map.Map Name Type,
    ceCoerce :: Map.Map Name CoerceTerm,
    ceDataTable :: DataTable
    }

newtype Ce t a = Ce (RWST CeEnv () Int t a)
    deriving(Monad,Functor,MonadTrans,MonadIO,MonadReader CeEnv,MonadState Int)

instance Monad m => UniqueProducer (Ce m) where
    newUniq = do
        i <- get
        put $! (i + 1)
        return i

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

convertDecls :: Monad m => TiData -> ClassHierarchy -> Map.Map Name Type -> DataTable -> [HsDecl] -> m [(Name,TVr,E)]
convertDecls tiData classHierarchy assumps dataTable hsDecls = liftM fst $ evalRWST ans ceEnv 2 where
    ceEnv = CeEnv {
        ceCoerce = tiCoerce tiData,
        ceAssumps = assumps,
        ceDataTable = dataTable
        }
    Ce ans = do
        nds <- mapM cDecl hsDecls
        return (map anninst $ concat nds)
    doNegate e = eAp (eAp (func_negate funcs) (getType e)) e
    Identity funcs = fmapM (return . EVar . toTVr assumps) sFuncNames
    anninst (a,b,c)
        | "Instance@" `isPrefixOf` show a = (a,setProperty prop_INSTANCE b, deNewtype dataTable c)
        | otherwise = (a,b, deNewtype dataTable c)
    cDecl :: Monad m => HsDecl -> Ce m [(Name,TVr,E)]
    cDecl (HsForeignDecl _ i@(Import {}) HS.Primitive _ n _) = do
        let name      = toName Name.Val n
        (var,ty,lamt) <- convertValue name
        let (ts,rt)   = argTypes' ty
            toPrim (Import cn is ls) = APrim (PrimPrim cn) (Requires is ls)
        es <- newVars [ t |  t <- ts, not (sortStarLike t) ]
        let result    = foldr ($) (processPrimPrim dataTable $ EPrim (toPrim i) (map EVar es) rt) (map ELam es)
        return [(name,var,lamt result)]
    cDecl (HsForeignDecl _ i@HS.AddrOf {} _ _ n _) = do
        let name       = toName Name.Val n
        (var,ty,lamt) <- convertValue name
        let (ts,rt)    = argTypes' ty
        (cn,st,ct) <- lookupCType' dataTable rt
        [uvar] <- newVars [st]
        let expr x     = return [(name,var,lamt x)]
            prim       = APrim (CP.AddrOf cn) (Requires is ls) where HS.AddrOf cn is ls = i
        expr $ eStrictLet uvar (EPrim prim [] st) (ELit (LitCons cn [EVar uvar] rt))
    cDecl (HsForeignDecl _ i@Import {} HS.CCall _ n _) = do
        let name = toName Name.Val n
        (var,ty,lamt) <- convertValue name
        let (ts,rt) = argTypes' ty
            (isIO,rt') = case  rt of
                ELit (LitCons c [x] _) | c == tc_IO -> (True,x)
                _ -> (False,rt)
        es <- newVars [ t |  t <- ts, not (sortStarLike t) ]
        (_,pt) <- lookupCType dataTable rt'
        [tvrWorld, tvrWorld2] <- newVars [tWorld__,tWorld__]
        let cFun = createFunc dataTable (map tvrType es)
            prim io rs rtt = EPrim (APrim (Func io s (snds rs) rtt) (Requires is ls))
                where Import s is ls = i
        result <- case (isIO,pt) of
            (True,"void") -> cFun $ \rs -> (,) (ELam tvrCont . ELam tvrWorld) $
                        eStrictLet tvrWorld2 (prim True rs "void" (EVar tvrWorld:[EVar t | (t,_) <- rs ]) tWorld__) (eJustIO (EVar tvrWorld2) vUnit)
            (False,"void") -> fail "pure foreign function must return a valid value"
            _ -> do
                (cn,rtt',rtt) <- lookupCType' dataTable rt'
                [rtVar,rtVar'] <- newVars [rt',rtt']
                let rttIO = ltTuple [tWorld__, rt']
                    rttIO' = ltTuple' [tWorld__, rtt']
                case isIO of
                    False -> cFun $ \rs -> (,) id $ eStrictLet rtVar' (prim False rs rtt [ EVar t | (t,_) <- rs ] rtt') (ELit $ LitCons cn [EVar rtVar'] rt')
                    True -> cFun $ \rs -> (,) (ELam tvrCont . ELam tvrWorld) $
                                eCaseTup' (prim True rs rtt (EVar tvrWorld:[EVar t | (t,_) <- rs ]) rttIO')  [tvrWorld2,rtVar'] (eLet rtVar (ELit $ LitCons cn [EVar rtVar'] rt') (eJustIO (EVar tvrWorld2) (EVar rtVar)))
        return [(name,var,lamt result)]

    cDecl x@HsForeignDecl {} = fail ("Unsupported foreign declaration: "++ show x)

    cDecl (HsPatBind sl p (HsUnGuardedRhs exp) []) | (HsPVar n) <- simplifyHsPat p, n == sillyName' = do
        e <- cExpr exp
        return [(v_silly,tvr,e)]

    cDecl (HsPatBind sl p rhs wh) | (HsPVar n) <- simplifyHsPat p = do
        let name = toName Name.Val n
        (var,ty,lamt) <- convertValue name
        rhs <- cRhs sl rhs
        lv <- hsLetE wh rhs
        return [(name,var,lamt lv)]
    cDecl (HsFunBind [(HsMatch sl n ps rhs wh)]) | ps' <- map simplifyHsPat ps, all isHsPVar ps' = do
        let name = toName Name.Val n
        (var,ty,lamt) <- convertValue name
        rhs <- cRhs sl rhs
        lv <- hsLetE wh rhs
        return [(name,var,lamt $ lp  ps' lv)]
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
    cExpr (HsAsPat n' (HsVar n)) = do
        cv <- lookupCoercion (toName Val n')
        let t = getAssump n
        case cv of
            Left t' -> return $ foldl eAp (EVar (tv n)) (map tipe $ specialize t t')
            Right c -> applyCoersion c $ EVar (tv n)
    cExpr (HsAsPat n' (HsCon n)) = return $ constructionExpression dataTable (toName DataConstructor n) rt where
        t' = getAssump n'
        (_,rt) = argTypes' (tipe t')
    cExpr (HsLit (HsString s)) = return $ E.Values.toE s
    cExpr (HsLit (HsInt i)) = return $ intConvert i
    cExpr (HsLit (HsChar ch)) = return $ toE ch
    cExpr (HsLit (HsFrac i))  = return $ toE i
    cExpr (HsLambda sl ps e) | all isHsPVar ps' = do
        e <- cExpr e
        return $ lp ps' e
      where ps' = map simplifyHsPat ps
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
    cExpr (HsIf e a b) = liftM3 eIf (cExpr e) (cExpr a) (cExpr b)
    cExpr (HsCase _ []) = error "empty case"
    cExpr (HsAsPat n hs@(HsCase e alts)) = do
        let ty = cType n
        scrut <- cExpr e
        cMatchs [scrut] (altConv alts) (EError ("No Match in Case expression at " ++ show (srcLoc hs))  ty)
    cExpr (HsTuple es) = liftM eTuple (mapM cExpr es)
    cExpr (HsAsPat n (HsList xs)) = do
        let cl (x:xs) = liftM2 eCons (cExpr x) (cl xs)
            cl [] = return $ eNil (cType n)
        cl xs
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
        return $ ELetRec [ (b,c) | (_,b,c) <- nds] e
    hsLet dl e = do
        e <- cExpr e
        hsLetE dl e

    cMatchs :: Monad m => [E] -> [([HsPat],HsRhs,[HsDecl])] -> E -> Ce m E
    cMatchs bs ms els = do
        pg <- processGuards ms
        convertMatches funcs tv cType bs pg els

    cGuard (HsUnGuardedRhs e) = liftM const $ cExpr e
    cGuard (HsGuardedRhss (HsGuardedRhs _ g e:gs)) = do
        g <- cExpr g
        e <- cExpr e
        fg <- cGuard (HsGuardedRhss gs)
        return (\els -> eIf g e (fg els))
    cGuard (HsGuardedRhss []) = return id

    getAssumpCon n  = case Map.lookup (toName Name.DataConstructor n) assumps of
        Just z -> z
        Nothing -> error $ "Lookup failed: " ++ (show n)
    getAssump n  = case Map.lookup (toName Name.Val n) assumps of
        Just z -> z
        Nothing -> error $ "Lookup failed: " ++ (show n)
    tv n = toTVr assumps (toName Name.Val n)
    lp  [] e = e
    lp  (HsPVar n:ps) e = eLam (tv n) $ lp  ps e
    lp  p e  =  error $ "unsupported pattern:" <+> tshow p  <+> tshow e
    cRhs sl (HsUnGuardedRhs e) = cExpr e
    cRhs sl (HsGuardedRhss []) = error "HsGuardedRhss: empty"
    cRhs sl (HsGuardedRhss gs@(HsGuardedRhs _ _ e:_)) = f gs where
        f (HsGuardedRhs _ g e:gs) = liftM3 eIf (cExpr g) (cExpr e) (f gs)
        f [] = do
            e <- cExpr e
            return $ ump sl $ getType e
    processGuards xs = flip mapM xs $ \ (ps,e,wh) -> do
        cg <- cGuard e
        nds <- mconcatMapM cDecl wh
        let elet = ELetRec [ (b,c) | (_,b,c) <- nds]
        return (map simplifyHsPat ps,elet . cg )
    cType (n::HsName) = fst $ convertVal assumps (toName Name.Val n)

    cClassDecl (HsClassDecl _ (HsQualType _ (HsTyApp (HsTyCon name) _)) decls) = do
        let ds = map simplifyDecl decls
            cr = findClassRecord classHierarchy className
            className = (toName ClassName name)
            method n = do
                let defaultName = defaultInstanceName n
                    (TVr { tvrType = ty}) = tv (nameName n)
                tels <- case [ d | d <- ds, maybeGetDeclName d == Just n] of
                    [] -> return []
                    (d:_) -> cDecl d >>= \ [(_,_,v)] -> return [v]
                return [(defaultName,tVr (toId defaultName) ty,els) | els <- tels ]
            cClass classRecord =  [ f n (toId n) (convertOneVal t) | n :>: t <- classAssumps classRecord ] where
                f n i t = (n,setProperties [prop_METHOD,prop_PLACEHOLDER] $ tVr i t, foldr ELam (EPrim (primPrim ("Placeholder: " ++ show n)) [] ft) args)  where
                    (ft',as) = fromPi t
                    (args,rargs) = span (sortStarLike . getType) as
                    ft = foldr EPi ft' rargs
        --mthds <- mconcatMapM method  [  n | n :>: _ <- classAssumps cr]
        let mthds = []
        return (cClass cr ++ mthds ++ primitiveInstances className)
    cClassDecl _ = error "cClassDecl"

-- | determine what arguments must be passed to something of the first type, to transform it into something of the second type.
specialize :: Type -> Type -> [Type]
specialize (TForAll vs _) (TForAll vs' _) | sameLength vs vs'= []  -- we assume program is typesafe
specialize (TForAll vs (ps :=> t)) b@(TForAll vs' _) | length vs' < length vs = specialize' (TForAll rs (ps :=> TForAll ls ([] :=> t))) b where
    nd = length vs - length vs'
    (rs,ls) = splitAt nd vs
specialize x y = specialize' x y

specialize' g@(TForAll vs (_ :=> t)) s = snds (gg t s)  where
    ps = zip vs [0 :: Int ..]
    gg a b = snubFst $ gg' a b
    gg' (TAp t1 t2) (TAp ta tb) = gg' t1 ta ++ gg' t2 tb
    gg' (TArrow t1 t2) (TArrow ta tb) = gg' t1 ta ++ gg' t2 tb
    gg' (TCon a) (TCon b) = if a /= b then error "constructors don't match." else []
    gg' (TVar a) t | Just n <- lookup a ps = [(n,t)]
    gg' (TVar a) (TVar b) | a == b = []
    gg' (TMetaVar a) (TMetaVar b) | a == b = []
    gg' (TForAll as1 (_ :=> r1)) (TForAll as2 (_ :=> r2)) | sameLength as1 as2 = do
      let r2' = TM.inst mempty (Map.fromList [ (tyvarAtom a2,TVar a1) | a1 <- as1 | a2 <- as2 ]) r2
      gg' r1 r2' -- assume names are unique
    gg' a b = error $ "specialization: " <> parens  (prettyPrintType a) <+> parens (prettyPrintType b) <+> "\nin spec\n" <+> vcat (map parens [prettyPrintType g, prettyPrintType s])
specialize' _g _s = []

ctgen t = map snd $ snubFst $ Seq.toList $ everything (Seq.<>) (mkQ Seq.empty gg) t where
    gg (TGen n g) = Seq.single (n,g)
    gg _ =  Seq.empty

integer_cutoff = 500000000

intConvert i | abs i > integer_cutoff  =  ELit (LitCons dc_Integer [ELit $ LitInt (fromInteger i) (rawType "intmax_t")] tInteger)
intConvert i =  ELit (LitCons dc_Int [ELit $ LitInt (fromInteger i) (rawType "int")] tInt)

litconvert (HsChar i) t | t == tChar =  LitInt (fromIntegral $ ord i) tCharzh
litconvert e t = error $ "litconvert: shouldn't happen: " ++ show (e,t)


fromHsPLitInt (HsPLit l@(HsInt _)) = return l
fromHsPLitInt (HsPLit l@(HsFrac _)) = return l
fromHsPLitInt x = fail $ "fromHsPLitInt: " ++ show x

convertMatches funcs tv cType bs ms err = match bs ms err where
    doNegate e = eAp (eAp (func_negate funcs) (getType e)) e
    fromInt = func_fromInt funcs
    fromInteger = func_fromInteger funcs
    fromRational = func_fromRational funcs
    match :: Monad m => [E] -> [([HsPat],E->E)] -> E -> Ce m E
    match  [] ps err = f ps where
        f (([],e):ps) = do
            r <- f ps
            return (e r)
        f [] = return err
        f _ = error "FromHs.convertMatches.match"
    match _ [] err = return err
    match (b:bs) ps err = f patternGroups err where
        f  [] err = return err
        f (ps:pss) err = do
            err' <- f pss err
            if isEVar err' || isEError err' then
               g ps err'
               else do
                [ev] <- newVars [getType err']
                nm <- g ps (EVar ev)
                return $ eLetRec [(ev,err')] nm
        g ps err
            | all (not . isStrictPat) patternHeads = match bs [(ps',eLetRec (toBinding p) . e)  | (p:ps',e) <- ps] err
            | any (isHsPAsPat || isHsPNeg || isHsPIrrPat) patternHeads = g (map (procAs b) ps) err
            | Just () <- mapM_ fromHsPLitInt patternHeads = do
                let tb = getType b
                [bv] <- newVars [tb]
                let gps = [ (p,[ (ps,e) |  (_:ps,e) <- xs ]) | (p,xs) <- sortGroupUnderF ((\ (x:_) -> x) . fst) ps]
                    eq = EAp (func_equals funcs) tb
                    f els (HsPLit (HsInt i),ps) = do
                        --let ip = (EAp (EAp fromInt tb) (ELit (LitInt (fromIntegral i) tInt)))
                        let ip | abs i > integer_cutoff  = (EAp (EAp fromInteger tb) (intConvert i))
                               | otherwise =  (EAp (EAp fromInt tb) (intConvert i))
                        m <- match bs ps err
                        return $ eIf (EAp (EAp eq (EVar bv)) ip) m els
                    f els (HsPLit (HsFrac i),ps) = do
                        --let ip = (EAp (EAp fromInt tb) (ELit (LitInt (fromIntegral i) tInt)))
                        let ip = (EAp (EAp fromRational tb) (toE i))
                        m <- match bs ps err
                        return $ eIf (EAp (EAp eq (EVar bv)) ip) m els
                e <- foldlM f err gps
                return $ eLetRec [(bv,b)] e
            | all isHsPLit patternHeads = do
                let gps = [ (p,[ (ps,e) |  (_:ps,e) <- xs ]) | (p,xs) <- sortGroupUnderF ((\ (x:_) -> x) . fst) ps]
                    f (HsPLit l,ps) = do
                        m <- match bs ps err
                        return (Alt  (litconvert l (getType b)) m)
                as@(_:_) <- mapM f gps
                [TVr { tvrIdent = vr }] <- newVars [Unknown]
                dataTable <- asks ceDataTable
                return $ unbox dataTable b vr $ \tvr -> eCase (EVar tvr) as err
                --return $ eCase b as err
            | all isHsPApp patternHeads = do
                let gps =  sortGroupUnderF (hsPatName . (\ (x:_) -> x) . fst) ps
                    f (name,ps) = do
                        let spats = hsPatPats $ (\ (x:_) -> x) $ fst ((\ (x:_) -> x) ps)
                            nargs = length spats
                        dataTable <- asks ceDataTable
                        vs <- newVars (slotTypes dataTable (toName DataConstructor name) (getType b))
                        vs' <- newVars (map (const Unknown) vs)

                        ps' <- mapM pp ps
                        m <- match (map EVar vs ++ bs) ps' err
                        return $ deconstructionExpression dataTable (toName DataConstructor name) (getType b) vs vs' m
                        --return (Alt (LitCons (toName DataConstructor name) vs (getType b))  m)
                    --pp :: Monad m =>  ([HsPat], E->E) -> m ([HsPat], E->E)
                    pp (HsPApp n ps:rps,e)  = do
                        return $ (ps ++ rps , e)
                as@(_:_) <- mapM f gps
                return $ eCase b as err
            | otherwise = error $ "Heterogenious list: " ++ show patternHeads
            where
            patternHeads = map ((\ (x:_) -> x) . fst) ps
        patternGroups = groupUnder (isStrictPat . (\ (x:_) -> x) . fst) ps
        procAs b (HsPNeg p:ps, ef) =  (p:ps,ef)  -- TODO, negative patterns
        procAs b (HsPAsPat n p:ps, ef) =  (p:ps,eLetRec [((tv n),b)] . ef)
        procAs b (HsPIrrPat p:ps, ef) =  (p:ps, ef) -- TODO, irrefutable patterns
        procAs _ x = x
        toBinding (HsPVar v) = [(tv v,b)]
        toBinding (HsPNeg (HsPVar v)) = [(tv v,doNegate b)]
        toBinding (HsPIrrPat p) = toBinding p
        toBinding (HsPAsPat n p) = (tv n,b):toBinding p
        toBinding p = error $ "toBinding: " ++ show p



isStrictPat HsPVar {} = False
isStrictPat (HsPNeg p) = isStrictPat p
isStrictPat (HsPAsPat _ p) = isStrictPat p
isStrictPat (HsPIrrPat p) = isStrictPat p  -- TODO irrefutable patterns
isStrictPat _ = True



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
        f _ _ = fail "specializeE: attempt to specialize types that do not unify"
    f [] gt



makeSpec :: Monad m => (TVr,E) -> T.Rule -> m ((TVr,E),Rule)
makeSpec (t,e) T.RuleSpec { T.ruleType = rt, T.ruleUniq = (Module m,ui), T.ruleSuper = ss } = do
    let nt = tipe rt
    as <- specializeE (getType t) nt
    let ntvr = tvr { tvrIdent = toId newName, tvrType = nt, tvrInfo = setProperties (prop_SPECIALIZATION:sspec) mempty }
        Just nn = fromId (tvrIdent t)
        (ntype,Just m,q) = nameParts nn
        newName = toName ntype (Just $ "Spec@." ++ m ++ "." ++ show ui,'f':m ++ "." ++ q)
        sspec = if ss then [prop_SUPERSPECIALIZE] else []
        ar = makeRule ("Specialize.{" ++ show newName) (Module m,ui) [] t as (EVar ntvr)
    return ((ntvr,foldl EAp e as),ar)


deNewtype :: DataTable -> E -> E
deNewtype dataTable e = f e where
    f ECase { eCaseScrutinee = e, eCaseAlts =  ((Alt (LitCons n [v] t) z):_) } | alias = eLet v (f e)  (f z) where
        Just Constructor { conAlias = alias } = getConstructor n dataTable
    f e = runIdentity $ emapE (return . f) e



