module E.FromHs(
    altConv,
    convertDecls,
    convertRules,
    createInstanceRules,
    createMethods,
    getMainFunction,
    guardConv,
    matchesConv,
    theMainName
    ) where

import Char
import Control.Monad.Identity
import Control.Monad.State
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
import C.Prims
import DataConstructors
import Doc.DocLike
import Doc.PPrint
import E.E
import E.Eval(eval)
import E.LetFloat(atomizeApps)
import E.Rules
import E.Subst
import E.Traverse
import E.TypeCheck
import E.Values
import FrontEnd.Rename(unRename)
import FrontEnd.SrcLoc
import FrontEnd.Tc.Type(prettyPrintType)
import FrontEnd.Utils
import GenUtil
import HsSyn
import Info.Types
import Name.Name as Name
import Name.Names
import Name.VConsts
import Options
import qualified FlagOpts as FO
import qualified Stats
import qualified Util.Seq as Seq
import Representation
import Support.CanType
import Support.FreeVars
import Type(schemeToType)
import Util.NameMonad

localVars = [10,12..]
theMainName = toName Name.Val "theMain"
ump sl e = EError  (srcLocShow sl ++ ": Unmatched pattern") e
srcLocShow sl = concat [srcLocFileName sl, ":",show $ srcLocLine sl,":", show $ srcLocColumn sl ]
nameToInt n = atomIndex $ toAtom n

concatMapM f xs = do
    xs <- mapM f xs
    return $ concat xs

head (x:_) = x
head _ = error "FromHsHeadError"

--newVars :: MonadState Int m => [E] -> m [TVr]
newVars xs = f xs [] where
    f [] xs = return $ reverse xs
    f (x:xs) ys = do
        s <- get
        put $! s + 2
        f xs (tVr ( s) x:ys)

lt :: Name -> Int
lt n | nameType n == TypeVal =  atomIndex $ toAtom $  n

tipe t = f t where
    f (TAp t1 t2) = eAp (f t1) (f t2)
    f (TArrow t1 t2) =  EPi (tVr 0 (f t1)) (f t2)
    f (TCon (Tycon n k)) =  ELit (LitCons n [] (kind k))
    f (TVar tv) = EVar (cvar tv)
    f (TMetaVar mv) = cmvar mv
    f (TGen _ (Tyvar _ n k _)) = EVar (tVr (lt n) (kind k))
    f (TForAll vs (_ :=> t)) = foldr EPi (f t) (map cvar vs)
    f (TExists xs (_ :=> t)) = let
        xs' = map (kind . tyvarKind) xs
        in ELit (LitCons (unboxedNameTuple TypeConstructor (length xs' + 1)) (f t:xs') eHash)
    cvar (Tyvar _ n k _) = (tVr (lt n) (kind k))
    cmvar MetaVar { metaKind = k } = tAbsurd (kind k)

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

{-
convertVal assumps n = (mp EPi ts (tipe t), mp eLam ts) where
    (Forall _ (_ :=> t)) = case Map.lookup n assumps of
        Just z -> z
        Nothing -> error $ "convertVal.Lookup failed: " ++ (show n)
    mp fn (((Tyvar _ n k _)):rs) t = fn (tVr (lt n) (kind k)) (mp fn rs t)
    mp _ [] t = t
    ts = ctgen t
    lt n =  nameToInt n
-}


fromTyvar (Tyvar _ n k _) = tVr (toId n) (kind k)

fromSigma (TForAll vs (_ :=> t)) = (map fromTyvar vs, tipe t)
fromSigma t = ([], tipe t)

convertVal assumps n = (foldr ePi t vs, flip (foldr eLam) vs) where
    (vs,t) = case Map.lookup n assumps of
        Just z -> fromSigma $ schemeToType z
        Nothing -> error $ "convertVal.Lookup failed: " ++ (show n)

convertOneVal (Forall _ (_ :=> t)) = (mp EPi ts (tipe t)) where
    mp fn (((Tyvar _ n k _)):rs) t = fn (tVr (lt n) (kind k)) (mp fn rs t)
    mp _ [] t = t
    ts = ctgen t
    lt n =  nameToInt n

toTVr assumps n = tVr ( nameToInt n) (typeOfName n) where
    typeOfName n = fst $ convertVal assumps n

matchesConv ms = map v ms where
    v (HsMatch _ _ ps rhs wh) = (map simplifyHsPat ps,rhs,wh)

altConv as = map v as where
    v (HsAlt _ p rhs wh) = ([simplifyHsPat p],guardConv rhs,wh)

guardConv (HsUnGuardedAlt e) = HsUnGuardedRhs e
guardConv (HsGuardedAlts gs) = HsGuardedRhss (map (\(HsGuardedAlt s e1 e2) -> HsGuardedRhs s e1 e2) gs)

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
        let e | not (fopts FO.Wrapper) = maine
              | otherwise = case ioLike (getType maine) of
                Just x ->  EAp (EAp (EVar runMain)  x ) maine
                Nothing ->  EAp (EAp (EVar runExpr) ty) maine
            be = eAp e vWorld__
            theMain = (theMainName,theMainTvr,be)
            theMainTvr =  tVr (nameToInt theMainName) (infertype dataTable be)
            tvm@(TVr { tvrType =  ty}) =  main
            maine = foldl EAp (EVar tvm) [ tAbsurd k |  TVr { tvrType = k } <- xs ]
            (ty',xs) = fromPi ty
        return theMain
    ioLike ty = case smplE ty of
        ELit (LitCons n [x] _) -> if show n ==  "Jhc.IO.IO" then Just x else Nothing
        _ -> Nothing
    smplE = id
    findName name = case Map.lookup name ds of
        Nothing -> fail $ "Cannot find: " ++ show name
        Just (n,_) -> return n

createInstanceRules :: Monad m => ClassHierarchy -> (Map.Map Name (TVr,E)) -> m Rules
createInstanceRules classHierarchy funcs = return $ fromRules ans where
    ans = concatMap cClass (classRecords classHierarchy)
    cClass classRecord =  concat [ method classRecord n | n :>: Forall _ (_ :=> t) <- classAssumps classRecord ]
    method classRecord methodName = as where
        methodVar = tVr ( nameToInt methodName) ty
        Identity (TVr {tvrType = ty},_) = findName methodName
        defaultName =  (defaultInstanceName methodName)
        valToPat' (ELit (LitCons x ts t)) = ELit $ LitCons x [ EVar (tVr j (getType z)) | z <- ts | j <- [2,4 ..]]  t
        valToPat' (EPi (TVr { tvrType =  a}) b)  = ELit $ LitCons tc_Arrow [ EVar (tVr j (getType z)) | z <- [a,b] | j <- [2,4 ..]]  eStar
        valToPat' x = error $ "FromHs.valToPat': " ++ show x
        as = [ rule  t | (_ :=> IsIn _ t ) <- snub (classInsts classRecord) ]
        rule t = emptyRule { ruleHead = methodVar, ruleArgs = [valToPat' (tipe t)], ruleBinds = [ t | ~(EVar t) <- vs], ruleBody = body, ruleUniq = (Module (show name),0), ruleName = toAtom $ "Rule.{" ++ show name ++ "}"}  where
            name = ((instanceName methodName (getTypeCons t)))
            ELit (LitCons _ vs _) = valToPat' (tipe t)
            body = case findName name of
                Just (n,_) -> foldl EAp (EVar n) vs
                Nothing -> case findName defaultName of
                    Just (deftvr,_) -> EAp (EVar deftvr) (valToPat' (tipe t))
                    Nothing -> EError ( show methodName ++ ": undefined at type " ++  PPrint.render (pprint t)) (eAp ty (valToPat' (tipe t)))
    findName name = case Map.lookup name funcs of
        Nothing -> fail $ "Cannot find: " ++ show name
        Just n -> return n

createMethods :: Monad m => DataTable -> ClassHierarchy -> (Map.Map Name (TVr,E))  -> m [(Name,TVr,E)]
createMethods dataTable classHierarchy funcs = return ans where
    ans = concatMap cClass (classRecords classHierarchy)
    cClass classRecord =  [ method classRecord n | n :>: _ <- classAssumps classRecord ]
    method classRecord methodName = (methodName ,setProperty prop_METHOD (tVr ( nameToInt methodName) ty),v) where
        theDefault = findName (defaultInstanceName methodName)
        Identity (TVr {tvrType = ty},_) = findName methodName
        (EPi tvr t) = ty
        els = EError ("Bad: " ++ show methodName) t
        v = eLam tvr (eCase (EVar tvr) as els)
        as = concatMap cinst [ t | (_ :=> IsIn _ t ) <- classInsts classRecord]
        cinst t | Nothing <- getConstructor x dataTable = fail "skip un-imported primitives"
                | Just (tvr,_) <- findName name = return $ calt (foldl EAp (EVar tvr) vs)
                | Just (deftvr,defe) <- theDefault = return $ calt $ ELetRec [(tvr,tipe t)] (EAp (EVar deftvr) (EVar tvr))
                | otherwise  = return $ calt $  EError ( show methodName ++ ": undefined at type " ++  PPrint.render (pprint t)) (getType els)
            where
            name = (instanceName methodName (getTypeCons t))
            calt e =  Alt (LitCons x [ tvr | ~(EVar tvr) <- vs ]  ct)  e
            (x,vs,ct) = case tipe t of
                (ELit (LitCons x' vs' ct')) -> (x',vs',ct')
                (EPi (TVr { tvrType = a}) b) -> (tc_Arrow,[a,b],eStar)
                e -> error $ "FromHs.createMethods: " ++ show e
    findName name = case Map.lookup name funcs of
        Nothing -> fail $ "Cannot find: " ++ show name
        Just n -> return n

methodNames ::  ClassHierarchy ->  [TVr]
methodNames  classHierarchy =  ans where
    ans = concatMap cClass (classRecords classHierarchy)
    cClass classRecord =  [ setProperty prop_METHOD $ tVr (nameToInt $ n) (convertOneVal t) | n :>: t <- classAssumps classRecord ]

unbox :: DataTable -> E -> Int -> (TVr -> E) -> E
unbox dataTable e vn wtd = ECase e (tVr 0 te) [Alt (LitCons cna [tvra] te) (wtd tvra)] Nothing where
    te = getType e
    tvra = tVr vn sta
    Just (cna,sta,ta) = lookupCType' dataTable te

createFunc :: DataTable -> [Int] -> [E] -> ([(TVr,String)] -> (E -> E,E)) -> E
createFunc dataTable ns es ee = foldr ELam eee tvrs where
    xs = [(tVr n te,n',runIdentity $ lookupCType' dataTable te) | te <- es | n <- ns | n' <- drop (length es) ns ]
    tvrs' = [ (tVr n' sta,rt) | (_,n',(_,sta,rt)) <- xs ]
    tvrs = [ t | (t,_,_) <- xs]
    (me,innerE) = ee tvrs'
    eee = me $ foldr esr innerE xs
    esr (tvr,n',(cn,st,_)) e = ECase (EVar tvr) (tVr 0 te) [Alt (LitCons cn [tVr n' st] te) e] Nothing  where
        te = getType $ EVar tvr

instance GenName String where
   genNames i = map (('x':) . show) [i..]

convertRules ::  ClassHierarchy -> Map.Map Name Scheme -> DataTable -> [HsDecl] -> IO [(String,[TVr],E,E)]
convertRules classHierarchy assumps dataTable hsDecls = concatMapM f hsDecls where
    f pr@HsPragmaRules {} = do
        let ce = convertE classHierarchy assumps dataTable (hsDeclSrcLoc pr)
        e1 <- ce (hsDeclLeftExpr pr)
        e2 <- ce (hsDeclRightExpr pr)
        (ts,cs) <- runNameMT $ do
            ts <- flip mapM (filter (sortStarLike . getType) $ freeVars e1) $ \tvr -> do
                --return (tvrIdent tvr,tvr)
                nn <- newNameFrom (map (:[]) ['a' ..])
                return (tvrIdent tvr,tvr { tvrIdent = toId (toName TypeVal nn) })
            cs <- flip mapM [toTVr assumps (toName Val v) | v <- hsDeclFreeVars pr ] $ \tvr -> do
                let ur = show $ unRename $ nameName (toUnqualified $ runIdentity $ fromId (tvrIdent tvr))
                nn <- newNameFrom (ur:map (\v -> ur ++ show v) [1 ::Int ..])
                return (tvrIdent tvr,tvr { tvrIdent = toId (toName Val nn) })
            return (ts,cs)
        let smt = substMap $ Map.fromList [ (x,EVar y)| (x,y) <- ts ]
            sma = substMap $ Map.fromList [ (x,EVar y)| (x,y) <- cs' ]
            cs' =  [ (x,(tvrType_u smt y))| (x,y) <- cs ]
            e2' = deNewtype dataTable $ smt $ sma e2
        e2 <- atomizeApps mempty Stats.theStats e2'
        return [(hsDeclString pr,( snds (cs' ++ ts) ),eval $ smt $ sma e1,e2)]
    f _ = return []

convertE :: Monad m => ClassHierarchy -> Map.Map Name Scheme -> DataTable -> SrcLoc -> HsExp -> m E
convertE classHierarchy assumps dataTable srcLoc exp = do
    [(_,_,e)] <- convertDecls classHierarchy assumps dataTable [HsPatBind srcLoc (HsPVar sillyName') (HsUnGuardedRhs exp) []]
    return e

sillyName = toName Val ("Jhc@","silly")
sillyName' = nameName sillyName


convertDecls :: Monad m => ClassHierarchy -> Map.Map Name Scheme -> DataTable -> [HsDecl] -> m [(Name,TVr,E)]
convertDecls classHierarchy assumps dataTable hsDecls = return (map anninst $ concatMap cDecl hsDecls) where
    doNegate e = eAp (eAp (func_negate funcs) (getType e)) e
    Identity funcs = fmapM (return . EVar . toTVr assumps) sFuncNames
    anninst (a,b,c)
        | "Instance@" `isPrefixOf` show a = (a,setProperty prop_INSTANCE b, deNewtype dataTable c)
        | otherwise = (a,b, deNewtype dataTable c)
    pval = convertVal assumps
    cDecl :: HsDecl -> [(Name,TVr,E)]
    cDecl (HsForeignDecl _ ForeignPrimitive s n _) = [(name,var, lamt (foldr ($) (EPrim (primPrim s) (map EVar es) rt) (map ELam es)))]  where
        name = toName Name.Val n
        var = tVr (nameToInt name) ty
        (ty,lamt) = pval name
        (ts,rt) = argTypes' ty
        es = [ (tVr ( n) t) |  t <- ts, not (sortStarLike t) | n <- localVars ]
    cDecl (HsForeignDecl _ ForeignCCall s n _)
        | Func _ s _ _ <- p, not isIO =  expr $ createFunc dataTable [4,6..] (map tvrType es) $ \rs -> (,) id $ eStrictLet rtVar' (EPrim (APrim (Func False s (snds rs) rtt) req) [ EVar t | (t,_) <- rs ] rtt') (ELit $ LitCons cn [EVar rtVar'] rt')
        | Func _ s _ _ <- p, "void" <- toExtType rt' =
                expr $ (createFunc dataTable [4,6..] (map tvrType es) $ \rs -> (,) (ELam tvrWorld) $
                    eStrictLet tvrWorld2 (EPrim (APrim (Func True s (snds rs) "void") req) (EVar tvrWorld:[EVar t | (t,_) <- rs ]) tWorld__) (eJustIO (EVar tvrWorld2) vUnit))
        | Func _ s _ _ <- p =
                expr $ (createFunc dataTable [4,6..] (map tvrType es) $ \rs -> (,) (ELam tvrWorld) $
                    eCaseTup' (EPrim (APrim (Func True s (snds rs) rtt) req) (EVar tvrWorld:[EVar t | (t,_) <- rs ]) rttIO')  [tvrWorld2,rtVar'] (eLet rtVar (ELit $ LitCons cn [EVar rtVar'] rt') (eJustIO (EVar tvrWorld2) (EVar rtVar))))
        | AddrOf _ <- p = let
            (cn,st,ct) = runIdentity (lookupCType' dataTable rt)
            (var:_) = freeNames (freeVars rt)
            vr = tVr var st
          in expr $ eStrictLet vr (EPrim (APrim p req) [] st) (ELit (LitCons cn [EVar vr] rt))
        where
        expr x = [(name,var,lamt x)]
        Just (APrim p req) = parsePrimString s
        name = toName Name.Val n
        tvrWorld = tVr 256 tWorld__
        tvrWorld2 = tVr 258 tWorld__
        rtVar = tVr 260 rt'
        rtVar' = tVr 262 rtt'
        rttIO = ltTuple [tWorld__, rt']
        rttIO' = ltTuple' [tWorld__, rtt']
        (isIO,rt') = case  rt of
            ELit (LitCons c [x] _) | show c == "Jhc.IO.IO" -> (True,x)
            _ -> (False,rt)
        toExtType e | Just (_,pt) <-  lookupCType dataTable e = pt
        toExtType e = error $ "toExtType: " ++ show e
        var = tVr (nameToInt name) ty
        (ty,lamt) = pval name
        (ts,rt) = argTypes' ty
        es = [ (tVr ( n) t) |  t <- ts, not (sortStarLike t) | n <- localVars ]
        (cn,rtt',rtt) = case lookupCType' dataTable rt' of
            Right x -> x
            Left err -> error $ "Odd RetType foreign: " ++ err
    cDecl (HsPatBind sl p (HsUnGuardedRhs exp) []) | (HsPVar n) <- simplifyHsPat p, n == sillyName' = let
        in [(sillyName,tvr,cExpr exp)]
    cDecl (HsPatBind sl p rhs wh) | (HsPVar n) <- simplifyHsPat p = let
        name = toName Name.Val n
        var = tVr (nameToInt name) ty -- lp ps (hsLet wh e)
        (ty,lamt) = pval name
        in [(name,var,lamt $ hsLetE wh (cRhs sl rhs))]
    cDecl (HsFunBind [(HsMatch sl n ps rhs wh)]) | ps' <- map simplifyHsPat ps, all isHsPVar ps' = [(name,var,lamt $ lp  ps' (hsLetE wh (cRhs sl rhs))) ] where
        name = toName Name.Val n
        var = tVr ( nameToInt name) ty -- lp ps (hsLet wh e)
        (ty,lamt) = pval name
    cDecl (HsFunBind ms@((HsMatch sl n ps _ _):_)) = [(name,v,lamt $ z $ cMatchs bs (matchesConv ms) (ump sl rt))] where
        name = toName Name.Val n
        v = tVr (nameToInt name) t -- lp ps (hsLet wh e)
        (t,lamt) = pval name
        (targs,eargs) = argTypes t
        bs' = [(tVr (n) t) | n <- localVars | t <- take numberPatterns eargs]
        bs  = map EVar bs'
        rt = discardArgs (length targs + numberPatterns) t
        numberPatterns = length ps
        z e = foldr (eLam) e bs'
    cDecl HsNewTypeDecl {  hsDeclName = dname, hsDeclArgs = dargs, hsDeclCon = dcon, hsDeclDerives = derives } = makeDerives dname dargs [dcon] (map (toName ClassName) derives)
    cDecl HsDataDecl {  hsDeclName = dname, hsDeclArgs = dargs, hsDeclCons = dcons, hsDeclDerives = derives } = makeDerives dname dargs dcons (map (toName ClassName) derives)
    cDecl cd@(HsClassDecl {}) = cClassDecl cd
    cDecl _ = []
    makeDerives dname dargs dcons derives  = concatMap f derives where
        f n | n == class_Bounded, all (null . hsConDeclArgs) dcons  = []
        f _ = []
    cExpr (HsAsPat n' (HsVar n)) = foldl eAp (EVar (tv n)) (map ty $ specialize t t') where
        t = getAssump n
        t' = getAssump n'
    --cExpr (HsAsPat n' (HsVar n)) = spec t t' $ EVar (tv n) where
        --(Forall _ (_ :=> t)) = getAssump n
        --Forall [] ((_ :=> t')) = getAssump n'
    cExpr (HsAsPat n' (HsCon n)) =  constructionExpression dataTable (toName DataConstructor n) rt where
        --Forall [] ((_ :=> t')) = getAssump n'
        t' = getAssump n'
        (_,rt) = argTypes' (ty t')
    cExpr (HsLit (HsString s)) = E.Values.toE s
    cExpr (HsLit (HsInt i)) = intConvert i
    cExpr (HsLit (HsChar ch)) = toE ch
    cExpr (HsLit (HsFrac i))  = toE i
    cExpr (HsLambda sl ps e)
        | all isHsPVar ps' =  lp ps' (cExpr e)
        | otherwise = error $ "Invalid HSLambda at: " ++ show sl
        where
        ps' = map simplifyHsPat ps
    cExpr (HsInfixApp e1 v e2) = eAp (eAp (cExpr v) (cExpr e1)) (cExpr e2)
    cExpr (HsLeftSection op e) = eAp (cExpr op) (cExpr e)
    cExpr (HsApp (HsRightSection e op) e') = eAp (eAp (cExpr op) (cExpr e')) (cExpr e)
    cExpr (HsRightSection e op) = eLam var (eAp (eAp cop (EVar var)) ce)  where
        (_,TVr { tvrType = ty}:_) = fromPi (getType cop)
        var = (tVr ( nv) ty)
        cop = cExpr op
        ce = cExpr e
        fvSet = (freeVars cop `Set.union` freeVars ce)
        (nv:_) = [ v  | v <- localVars, not $  v `Set.member` fvSet  ]
    cExpr (HsApp e1 e2) = eAp (cExpr e1) (cExpr e2)
    cExpr (HsParen e) = cExpr e
    cExpr (HsExpTypeSig _ e _) = cExpr e
    cExpr (HsNegApp e) = (doNegate (cExpr e))
    cExpr (HsLet dl e) = hsLet dl e
    cExpr (HsIf e a b) = eIf (cExpr e) (cExpr a) (cExpr b)
    cExpr (HsCase _ []) = error "empty case"
    cExpr hs@(HsCase e alts) = z where
        z = cMatchs [cExpr e] (altConv alts) (EError ("No Match in Case expression at " ++ show (srcLoc hs))  (getType z))
    cExpr (HsTuple es) = eTuple (map cExpr es)
    cExpr (HsAsPat n (HsList xs)) = cl xs where
        cl (x:xs) = eCons (cExpr x) (cl xs)
        cl [] = eNil (cType n)
    cExpr e = error ("Cannot convert: " ++ show e)
    hsLetE [] e =  e
    hsLetE dl e =  ELetRec [ (b,c) | (_,b,c) <- (concatMap cDecl dl)] e
    hsLet dl e = hsLetE dl (cExpr e)

    ty x = tipe x
    kd x = kind x
    cMatchs :: [E] -> [([HsPat],HsRhs,[HsDecl])] -> E -> E
    cMatchs bs ms els = convertMatches funcs dataTable tv cType bs (processGuards ms) els

    cGuard (HsUnGuardedRhs e) _ = cExpr e
    cGuard (HsGuardedRhss (HsGuardedRhs _ g e:gs)) els = eIf (cExpr g) (cExpr e) (cGuard (HsGuardedRhss gs) els)
    cGuard (HsGuardedRhss []) e = e

    getAssumpCon n  = case Map.lookup (toName Name.DataConstructor n) assumps of
        Just z -> schemeToType z
        Nothing -> error $ "Lookup failed: " ++ (show n)
    getAssump n  = case Map.lookup (toName Name.Val n) assumps of
        Just z -> schemeToType z
        Nothing -> error $ "Lookup failed: " ++ (show n)
    tv n = toTVr assumps (toName Name.Val n)
    lp  [] e = e
    lp  (HsPVar n:ps) e = eLam (tv n) $ lp  ps e
    lp  p e  =  error $ "unsupported pattern:" <+> tshow p  <+> tshow e
    cRhs sl (HsUnGuardedRhs e) = cExpr e
    cRhs sl (HsGuardedRhss []) = error "HsGuardedRhss: empty"
    cRhs sl (HsGuardedRhss gs@(HsGuardedRhs _ _ e:_)) = f gs where
        f (HsGuardedRhs _ g e:gs) = eIf (cExpr g) (cExpr e) (f gs)
        f [] = ump sl $ getType (cExpr e)
    processGuards xs = [ (map simplifyHsPat ps,hsLetE wh . cGuard e) | (ps,e,wh) <- xs ]
    spec g s e = ct (gg g s)  e  where
        ct ts e = foldl eAp e $ map ty $ snds ts
        gg a b = snubFst $ gg' a b
        gg' (TAp t1 t2) (TAp ta tb) = gg' t1 ta ++ gg' t2 tb
        gg' (TArrow t1 t2) (TArrow ta tb) = gg' t1 ta ++ gg' t2 tb
        gg' (TCon a) (TCon b) = if a /= b then error "constructors don't match." else []
        gg' _ (TGen _ _) = error "Something impossible happened!"
        gg' (TGen n _) t = [(n,t)]
        gg' (TVar a) (TVar b) | a == b = []
        gg' (TMetaVar a) (TMetaVar b) | a == b = []
        gg' a b = error $ "specialization: " <> parens  (prettyPrintType a) <+> parens (prettyPrintType b) <+> "\nin spec\n" <+> vcat (map parens [prettyPrintType g, prettyPrintType s, show e])
    cType (n::HsName) = fst $ pval (toName Name.Val n)

    cClassDecl (HsClassDecl _ (HsQualType _ (HsTyApp (HsTyCon name) _)) decls) = ans where
        ds = map simplifyDecl decls
        cr = findClassRecord classHierarchy (toName ClassName name)
        ans = cClass cr ++ concatMap method [  n | n :>: _ <- classAssumps cr]
        method n = [(defaultName,tVr (nameToInt defaultName) ty,els) | els <- mels] where
            defaultName = defaultInstanceName n
            (TVr { tvrType = ty}) = tv (nameName n)
            mels = case [ d | d <- ds, maybeGetDeclName d == Just n] of
                [] -> []
                (d:_) | ~[(_,_,v)] <- cDecl d -> [v]
        cClass classRecord =  [ f n (nameToInt n) (convertOneVal t) | n :>: t <- classAssumps classRecord ] where
            f n i t = (n,setProperties [prop_METHOD,prop_PLACEHOLDER] $ tVr i t, EPrim (primPrim ("Placeholder: " ++ show n)) [] t)
    cClassDecl _ = error "cClassDecl"

-- | determine what arguments must be passed to something of the first type, to transform it into something of the second type.
specialize :: Type -> Type -> [Type]
specialize TForAll {} TForAll {} = []  -- we assume program is typesafe
specialize g@(TForAll vs (_ :=> t)) s = snds (gg t s)  where
    ps = zip vs [0 :: Int ..]
    gg a b = snubFst $ gg' a b
    gg' (TAp t1 t2) (TAp ta tb) = gg' t1 ta ++ gg' t2 tb
    gg' (TArrow t1 t2) (TArrow ta tb) = gg' t1 ta ++ gg' t2 tb
    gg' (TCon a) (TCon b) = if a /= b then error "constructors don't match." else []
    gg' (TVar a) t | Just n <- lookup a ps = [(n,t)]
    gg' (TVar a) (TVar b) | a == b = []
    gg' (TMetaVar a) (TMetaVar b) | a == b = []
    gg' a b = error $ "specialization: " <> parens  (prettyPrintType a) <+> parens (prettyPrintType b) <+> "\nin spec\n" <+> vcat (map parens [prettyPrintType g, prettyPrintType s])
specialize _g _s = []

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

convertMatches funcs dataTable tv cType bs ms err = evalState (match bs ms err) (20 + 2*length bs)  where
    doNegate e = eAp (eAp (func_negate funcs) (getType e)) e
    fromInt = func_fromInt funcs
    fromInteger = func_fromInteger funcs
    fromRational = func_fromRational funcs
    match :: [E] -> [([HsPat],E->E)] -> E -> State Int E
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
                return $ unbox dataTable b vr $ \tvr -> eCase (EVar tvr) as err
                --return $ eCase b as err
            | all isHsPApp patternHeads = do
                let gps =  sortGroupUnderF (hsPatName . (\ (x:_) -> x) . fst) ps
                    f (name,ps) = do
                        let spats = hsPatPats $ (\ (x:_) -> x) $ fst ((\ (x:_) -> x) ps)
                            nargs = length spats
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


deNewtype :: DataTable -> E -> E
deNewtype dataTable e = f e where
--    f (ELit (LitCons n [x] t)) | alias =  (f x)  where
--        alias = case getConstructor n dataTable of
--                 Just v -> conAlias v
--                 x      -> error ("deNewtype for "++show n++": "++show x)
    f ECase { eCaseScrutinee = e, eCaseAlts =  ((Alt (LitCons n [v] t) z):_) } | alias = eLet v (f e)  (f z) where
        Just Constructor { conAlias = alias } = getConstructor n dataTable
    f e = runIdentity $ emapE (return . f) e


