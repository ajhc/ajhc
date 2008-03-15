
-- |
-- This module implements the Kind Inference algorithm, and the routines which
-- use the product of kind inference to convert haskell source types into the
-- simplified kind annotated types used by the rest of the FrontEnd.

module FrontEnd.KindInfer (
    kiDecls,
    KindEnv(),
    hsQualTypeToSigma,
    hsAsstToPred,
    kindOfClass,
    kindOf,
    restrictKindEnv,
    hsTypeToType,
    getConstructorKinds
    ) where

import Control.Monad.Reader
import Data.List
import qualified Data.Traversable as T
import Util.Inst()
import Data.Maybe
import Control.Monad.Identity
import Control.Monad.Writer
import Data.Generics
import Data.IORef
import Data.Monoid
import qualified Data.Map as Map
import System.IO.Unsafe

import Data.Binary
import Doc.DocLike
import Doc.PPrint
import FrontEnd.Tc.Type
import FrontEnd.Tc.Kind
import FrontEnd.Utils
import GenUtil
import Support.FreeVars
import HsSyn
import Support.MapBinaryInstance
import Name.Name
import qualified Util.Seq as Seq
import qualified FlagDump as FD
import Options
import Util.ContextMonad
import Util.HasSize


data KindEnv = KindEnv {
    kindEnv :: Map.Map Name Kind,
    kindEnvAssocs :: Map.Map Name (Int,Int),
    kindEnvClasses :: Map.Map Name [Kind]
    } deriving(Typeable,Show)
        {-!derive: Monoid !-}

instance Binary KindEnv where
    put KindEnv { kindEnv = a, kindEnvAssocs = b, kindEnvClasses = c } = putMap a >> putMap b >> putMap c
    get = do
        a <- getMap
        b <- getMap
        c <- getMap
        return KindEnv { kindEnv = a, kindEnvAssocs = b, kindEnvClasses = c }

instance HasSize KindEnv where
    size KindEnv { kindEnv = env } = size env

instance FreeVars Kind [Kindvar] where
   freeVars (KVar kindvar) = [kindvar]
   freeVars (kind1 `Kfun` kind2) = freeVars kind1 `union` freeVars kind2
   freeVars KBase {} = []



instance DocLike d =>  PPrint d KindEnv where
    pprint KindEnv { kindEnv = m, kindEnvAssocs = ev, kindEnvClasses = cs } = vcat $
        [ pprint x <+> text "=>" <+> pprint y | (x,y) <- Map.toList m] ++
        [ text "associated type" <+> pprint n <+> pprint ab  | (n,ab) <- Map.toList ev] ++
        [ text "class" <+> pprint n <+> pprint ab  | (n,ab) <- Map.toList cs] ++
        [empty]

--------------------------------------------------------------------------------


-- The kind inference monad

data KiWhere = InClass | InInstance | Other
    deriving(Eq)

data KiEnv  = KiEnv {
    kiContext :: [String],
    kiEnv :: IORef KindEnv,
    kiWhere :: KiWhere,
    kiVarnum :: IORef Int
    }

newtype Ki a = Ki (ReaderT KiEnv IO a)
    deriving(Monad,MonadReader KiEnv,MonadIO,Functor)


restrictKindEnv :: (Name -> Bool) -> KindEnv -> KindEnv
restrictKindEnv f ke = ke { kindEnv = Map.filterWithKey (\k _ -> f k) (kindEnv ke) }

--------------------------------------------------------------------------------

findKind :: MonadIO m => Kind -> m Kind
findKind tv@(KVar Kindvar {kvarRef = r, kvarConstraint = con }) = liftIO $ do
    rt <- readIORef r
    case rt of
        Nothing
            | con == KindStar -> writeIORef r (Just kindStar) >> return kindStar
            | otherwise -> return tv
        Just t -> do
            t' <- findKind t
            writeIORef r (Just t')
            return t'
findKind tv = return tv

-- useful operations in the inference monad

runKI :: KindEnv -> Ki a -> IO a
runKI env (Ki ki) = (kienv >>= ki') where
    kienv = do
        env <- newIORef env
        varnum <- newIORef 0
        return KiEnv { kiContext = [], kiEnv = env, kiVarnum = varnum, kiWhere = Other }
    ki' e = runReaderT ki e


instance ContextMonad String Ki where
    withContext nc x = local (\s -> s { kiContext = nc :kiContext s }) x


getEnv :: Ki KindEnv
getEnv = do asks kiEnv >>= liftIO . readIORef


unify :: Kind -> Kind -> Ki ()
unify k1 k2 = do
    k1 <- flattenKind k1
    k2 <- flattenKind k2
    printRule $ "unify:" <+> pprint k1 <+> text "<->" <+> pprint k2
    mgu k1 k2

mgu :: Kind -> Kind -> Ki ()
mgu (KBase a) (KBase b) | a == b = return ()
mgu (Kfun a b) (Kfun a' b') = do
    unify a a'
    unify b b'
mgu (KVar u) k = varBind u k
mgu k (KVar u) = varBind u k
mgu k1 k2 = fail $ "attempt to unify these two kinds: " ++ show k1 ++ " <-> " ++ show k2

varBind :: Kindvar -> Kind -> Ki ()
varBind u k = do
    k <- flattenKind k
    printRule $ "varBind:" <+> pprint u <+> text ":=" <+> pprint k
    if k == KVar u then return () else do
    when (u `elem` freeVars k) $ fail $ "occurs check failed in kind inference: " ++ show u ++ " := " ++ show k
    v <- liftIO $ readIORef (kvarRef u)
    case v of
        Just v -> fail $ "varBind unfree"
        Nothing -> do
            liftIO $ writeIORef (kvarRef u) (Just k)
            constrain (kvarConstraint u) k

zonkConstraint :: KindConstraint -> Kindvar -> Ki ()
zonkConstraint nk mv = do
    let fk = mappend nk (kvarConstraint mv)
    if fk == kvarConstraint mv then return () else do
        nref <- liftIO $ newIORef Nothing
        let nmv = mv { kvarConstraint = fk, kvarRef = nref }
        liftIO $ modifyIORef (kvarRef mv) (\Nothing -> Just $ KVar nmv)

constrain KindAny k = return ()
constrain KindStar        (KBase Star) = return ()
constrain KindQuest       k@KBase {}  = kindCombine kindFunRet k >> return ()
constrain KindQuestQuest  (KBase KQuest) = fail "cannot constraint ? to be ??"
constrain KindQuestQuest  k@KBase {}  = kindCombine kindArg k >> return ()
constrain KindSimple (KBase Star) = return ()
constrain KindSimple (a `Kfun` b) = do
    a <- findKind a
    b <- findKind b
    constrain KindSimple a
    constrain KindSimple b
constrain con (KVar v) = zonkConstraint con v
constrain con k = fail $ "constraining kind: " ++ show (con,k)


flattenKind :: Kind -> Ki Kind
flattenKind k = f' k where
    f (a `Kfun` b) = return Kfun `ap` f' a `ap` f' b
    f k = return k
    f' k = findKind k >>= f


newKindVar :: KindConstraint -> Ki Kindvar
newKindVar con = do
    KiEnv { kiVarnum = vr } <- ask
    liftIO $ do
    n <- readIORef vr
    writeIORef vr $! (n + 1)
    nr <- newIORef Nothing
    return Kindvar { kvarUniq = n, kvarRef = nr, kvarConstraint = con }


lookupKind :: KindConstraint -> Name -> Ki Kind
lookupKind con name = do
    KindEnv { kindEnv = env } <- getEnv
    case Map.lookup name env of
        Just k -> do
            k <- findKind k
            constrain con k
            findKind k
        Nothing -> do
            kv <- newKindVar con
            extendEnv mempty { kindEnv = Map.singleton name (KVar kv) }
            return (KVar kv)

extendEnv :: KindEnv -> Ki ()
extendEnv newEnv = do
    ref <- asks kiEnv
    liftIO $ modifyIORef ref (mappend newEnv) -- (\ (KindEnv env x) -> KindEnv (env `Map.union` newEnv) (nx `mappend` x))


getConstructorKinds :: KindEnv -> Map.Map Name Kind
getConstructorKinds ke = kindEnv ke -- Map.fromList [ (toName TypeConstructor x,y) | (x,y)<- Map.toList m]

--------------------------------------------------------------------------------

-- kind inference proper
-- this is what gets called from outside of this module


printRule :: String -> Ki ()
printRule s
    | dump FD.KindSteps = liftIO $ putStrLn s
    | otherwise = return ()

kiDecls :: KindEnv -> [HsDecl] -> IO KindEnv
kiDecls inputEnv classAndDataDecls = ans where
    ans = do
        ke <- run
        return ke -- TODO (Map.fromList (concatMap kgAssocs kindGroups) `mappend` as))
    run = runKI inputEnv $ withContext ("kiDecls: " ++ show (map getDeclName classAndDataDecls)) $ do
        kiInitClasses classAndDataDecls
        mapM_ kiDecl classAndDataDecls
        getEnv >>= postProcess

postProcess ke = do
    kindEnv <- T.mapM flattenKind (kindEnv ke)
    kindEnvClasses <- T.mapM (mapM flattenKind) (kindEnvClasses ke)
    let defs = snub (freeVars (Map.elems kindEnv,Map.elems kindEnvClasses))
    printRule $ "defaulting the following kinds: " ++ pprint defs
    mapM_ (flip varBind kindStar) defs
    kindEnv <- T.mapM flattenKind kindEnv
    kindEnvClasses <- T.mapM (mapM flattenKind) kindEnvClasses
    return ke { kindEnvClasses = kindEnvClasses, kindEnv = kindEnv }


kiType,kiType' :: Kind -> HsType -> Ki ()
kiType' k t = do
    k <- findKind k
    kiType k t

kiType k (HsTyTuple ts) = do
    unify kindStar k
    mapM_ (kiType' kindStar) ts
kiType k (HsTyUnboxedTuple ts) = do
    unify kindUTuple k
    flip mapM_ ts $ \t -> do
        kt <- newKindVar KindQuestQuest
        kiType (KVar kt) t
kiType k (HsTyFun a b) = do
    unify kindStar k
    ka <- newKindVar KindQuestQuest
    kb <- newKindVar KindQuest
    kiType (KVar ka) a
    kiType (KVar kb) b
kiType k (HsTyApp a b) = do
    kv <- newKindVar KindAny
    kiType  (KVar kv `Kfun` k) a
    kiType' (KVar kv) b
kiType k (HsTyVar v) = do
    kv <- lookupKind KindAny (toName TypeVal v)
    unify k kv
kiType k (HsTyCon v) = do
    kv <- lookupKind KindAny (toName TypeConstructor v)
    unify k kv
kiType k (HsTyCon v) = do
    kv <- lookupKind KindAny (toName TypeConstructor v)
    unify k kv
kiType k HsTyAssoc = do
    constrain KindSimple k
kiType _ HsTyEq {} = error "kiType.HsTyEq"
kiType k HsTyForall { hsTypeVars = vs, hsTypeType = HsQualType con t } = do
    mapM_ initTyVarBind vs
    mapM_ kiPred con
    kiType' k t
kiType k HsTyExpKind { hsTyType = t, hsTyKind = ek } = do
    unify (hsKindToKind ek) k
    kiType' k t
kiType k HsTyExists { hsTypeVars = vs, hsTypeType = HsQualType con t } = do
    mapM_ initTyVarBind vs
    mapM_ kiPred con
    kiType' k t

initTyVarBind HsTyVarBind { hsTyVarBindName = name, hsTyVarBindKind = kk } = do
    nk <- lookupKind KindSimple (toName TypeVal name)
    case kk of
        Nothing -> return ()
        Just kk -> unify nk (hsKindToKind kk)



hsKindToKind (HsKindFn a b) = hsKindToKind a `Kfun` hsKindToKind b
hsKindToKind a | a == hsKindStar = kindStar
hsKindToKind a | a == hsKindHash = kindHash
hsKindToKind (HsKind n) = KBase (KNamed (toName SortName n))
-- hsKindToKind (HsKind n) = toName SortName n

kiApps :: Kind -> [HsType] -> Kind -> Ki ()
kiApps ca args fk = f ca args fk where
    f ca [] fk = unify ca fk
    f (x `Kfun` y) (a:as) fk = do
        kiType' x a
        y <- findKind y
        f y as fk
    f (KVar var) as fk = do
        x <- newKindVar KindAny
        y <- newKindVar KindAny
        let nv = (KVar x `Kfun` KVar y)
        varBind var nv
        f nv as fk

kiApps' :: Kind -> [Kind] -> Kind -> Ki ()
kiApps' ca args fk = f ca args fk where
    f ca [] fk = unify ca fk
    f (x `Kfun` y) (a:as) fk = do
        unify a x
        y <- findKind y
        f y as fk
    f (KVar var) as fk = do
        x <- newKindVar KindAny
        y <- newKindVar KindAny
        let nv = (KVar x `Kfun` KVar y)
        varBind var nv
        f nv as fk

kiPred :: HsAsst -> Ki ()
kiPred asst@(HsAsst n ns) = do
    env <- getEnv
    let f k n = do
            k' <- lookupKind KindAny (toName TypeVal n)
            unify k k'
    case Map.lookup (toName ClassName n) (kindEnvClasses env) of
        Nothing -> fail $ "unknown class: " ++ show asst
        Just ks -> zipWithM_ f ks ns
kiPred (HsAsstEq a b) = do
    mv <- newKindVar KindSimple
    kiType  (KVar mv) a
    kiType' (KVar mv) b

kiInitClasses :: [HsDecl] -> Ki ()
kiInitClasses ds =  sequence_ [ f className [classArg] |  HsClassDecl _ (HsQualType _ (HsTyApp (HsTyCon className) (HsTyVar classArg))) _ <- ds]
                    >> sequence_ [ f (hsDeclName cad) [v | HsTyVar v <- hsDeclTypeArgs cad]
                                   | cad@(HsClassAliasDecl {}) <- ds ]
    where
    f className args = do
        args <- mapM (lookupKind KindSimple . toName TypeVal) args
        extendEnv mempty { kindEnvClasses = Map.singleton (toName ClassName className) args }


kiDecl :: HsDecl -> Ki ()
kiDecl HsDataDecl { hsDeclContext = context, hsDeclName = tyconName, hsDeclArgs = args, hsDeclCons = [], hsDeclHasKind = Just kk } = do
    args <- mapM (lookupKind KindSimple . toName TypeVal) args
    kc <- lookupKind KindAny (toName TypeConstructor tyconName)
    kiApps' kc args (hsKindToKind kk)
    mapM_ kiPred context
kiDecl HsDataDecl { hsDeclContext = context, hsDeclName = tyconName, hsDeclArgs = args, hsDeclCons = condecls } = kiData context tyconName args condecls
kiDecl HsNewTypeDecl { hsDeclContext = context, hsDeclName = tyconName, hsDeclArgs = args, hsDeclCon = condecl } = kiData context tyconName args [condecl]
kiDecl HsTypeDecl { hsDeclName = name, hsDeclTArgs = args, hsDeclType = ty } = do
    wh <- asks kiWhere
    let theconstraint = if wh == Other then KindAny else KindSimple
    kc <- lookupKind theconstraint (toName TypeConstructor name)
    mv <- newKindVar theconstraint
    kiApps kc args (KVar mv)
    kiType' (KVar mv) ty
kiDecl (HsTypeSig _ _ (HsQualType ps t)) = do
    mapM_ kiPred ps
    kiType kindStar t
kiDecl (HsClassDecl _sloc qualType sigsAndDefaults) = ans where
    HsQualType contxt (HsTyApp (HsTyCon _className) (HsTyVar classArg)) =  qualType
    ans = do
        carg <- lookupKind KindSimple (toName TypeVal classArg)
        mapM_ kiPred contxt
        extendEnv mempty { kindEnvAssocs = Map.fromList assocs }
        mapM_ (\n -> lookupKind KindSimple n >>= unify carg ) rn
        local (\e -> e { kiWhere = InClass }) $ mapM_ kiDecl sigsAndDefaults

    numClassArgs = 1
    newAssocs = [ (name,[ n | ~(HsTyVar n) <- names],t,names) | HsTypeDecl _sloc name names t <- sigsAndDefaults ]
    assocs = [ (toName TypeConstructor n,(numClassArgs,length names - numClassArgs)) | (n,names,_,_) <- newAssocs ]
    rn = Seq.toList $ everything (Seq.<>) (mkQ Seq.empty f) (newClassBodies,newAssocs)
    newClassBodies = map typeFromSig $ filter isHsTypeSig sigsAndDefaults
    f (HsTyVar n') | hsNameToOrig n' == hsNameToOrig classArg = Seq.single (toName TypeVal n')
    f _ = Seq.empty
    typeFromSig :: HsDecl -> HsQualType
    typeFromSig (HsTypeSig _sloc _names qualType) = qualType
kiDecl _ = return ()

kiData context tyconName args condecls = do
    args <- mapM (lookupKind KindSimple . toName TypeVal) args
    kc <- lookupKind KindSimple (toName TypeConstructor tyconName)
    kiApps' kc args kindStar
    mapM_ kiPred context
    flip mapM_  (concatMap (map hsBangType . hsConDeclArgs) condecls) $ \t -> do
        v <- newKindVar KindQuestQuest
        kiType (KVar v) t

kiHsQualType :: KindEnv -> HsQualType -> KindEnv
kiHsQualType inputEnv qualType@(HsQualType ps t) = newState where
    newState = unsafePerformIO $ runKI inputEnv $ withContext ("kiHsQualType: " ++ show qualType) $ do
        kiType kindStar t
        mapM_ kiPred ps
        getEnv >>= postProcess


--------------------------------------------------------------------------------

kindOf :: Name -> KindEnv -> Kind
kindOf name KindEnv { kindEnv = env } = case Map.lookup name env of
            Nothing | nameType name `elem` [TypeConstructor,TypeVal] -> kindStar
            Just k -> k
            _ -> error $ "kindOf: could not find kind of : " ++ show (nameType name,name)

kindOfClass :: Name -> KindEnv -> [Kind]
kindOfClass name KindEnv { kindEnvClasses = cs } = case Map.lookup name cs of
        --Nothing -> Star
        Nothing -> error $ "kindOf: could not find kind of class : " ++ show (nameType name,name)
        Just k -> k

----------------------
-- Conversion of Types
----------------------

fromTyApp t = f t [] where
    f (HsTyApp a b) rs = f a (b:rs)
    f t rs = (t,rs)


aHsTypeToType :: KindEnv -> HsType -> Type
aHsTypeToType kt@KindEnv { kindEnvAssocs = at } t | (HsTyCon con,xs) <- fromTyApp t, let nn = toName TypeConstructor con, Just (n1,n2) <- Map.lookup nn at =
    TAssoc {
        typeCon = Tycon nn (kindOf nn kt),
        typeClassArgs = map (aHsTypeToType kt) (take n1 xs),
        typeExtraArgs = map (aHsTypeToType kt) (take n2 $ drop n1 xs)
    }
aHsTypeToType kt (HsTyFun t1 t2) = aHsTypeToType kt t1 `fn` aHsTypeToType kt t2
aHsTypeToType kt HsTyExpKind { hsTyType = t } = aHsTypeToType kt t
aHsTypeToType kt tuple@(HsTyTuple types) = tTTuple $ map (aHsTypeToType kt) types
aHsTypeToType kt tuple@(HsTyUnboxedTuple types) = tTTuple' $ map (aHsTypeToType kt) types
aHsTypeToType kt (HsTyApp t1 t2) = TAp (aHsTypeToType kt t1) (aHsTypeToType kt t2)


-- variables, we must know the kind of the variable here!
-- they are assumed to already exist in the kindInfoTable
-- which was generated by the process of KindInference

aHsTypeToType kt (HsTyVar name) = TVar $ toTyvar kt name --  tyvar  name (kindOf name kt) Nothing

-- type constructors, we must know the kind of the constructor.
-- here we also qualify the type constructor if it is
-- currently unqualified

aHsTypeToType kt (HsTyCon name) = TCon $ Tycon nn (kindOf nn kt)  where
    nn =  (toName TypeConstructor name)

aHsTypeToType kt (HsTyForall vs qt) = TForAll (map (toTyvar kt . hsTyVarBindName) vs) (aHsQualTypeToQualType kt qt)
aHsTypeToType kt (HsTyExists vs qt) = TExists (map (toTyvar kt . hsTyVarBindName) vs) (aHsQualTypeToQualType kt qt)

aHsTypeToType _ t = error $ "aHsTypeToType: " ++ show t

toTyvar kt name =  tyvar  nn (kindOf nn kt) where
    nn = toName TypeVal name

aHsQualTypeToQualType :: KindEnv -> HsQualType -> Qual Type
aHsQualTypeToQualType kt (HsQualType cntxt t) = map (hsAsstToPred kt) cntxt :=> aHsTypeToType kt t


hsAsstToPred :: KindEnv -> HsAsst -> Pred
hsAsstToPred kt (HsAsst className [varName])
   -- = IsIn className (TVar $ Tyvar varName (kindOf varName kt))
   | isConstructorLike (hsIdentString . hsNameIdent $ varName) = IsIn  (toName ClassName className) (TCon (Tycon (toName TypeConstructor varName) (head $ kindOfClass (toName ClassName className) kt)))
   | otherwise = IsIn (toName ClassName className) (TVar $ tyvar (toName TypeVal varName) (head $ kindOfClass (toName ClassName className) kt))
hsAsstToPred kt (HsAsstEq t1 t2) = IsEq (runIdentity $ hsTypeToType kt t1) (runIdentity $ hsTypeToType kt t2)



hsQualTypeToSigma kt qualType = hsQualTypeToType kt (Just []) qualType

hsTypeToType :: Monad m => KindEnv -> HsType -> m Type
hsTypeToType kt t = return $ hoistType $ aHsTypeToType kt t -- (forallHoist t)

hsQualTypeToType :: Monad m =>
    KindEnv            -- ^ the kind environment
    -> Maybe [HsName]  -- ^ universally quantify free variables excepting those in list.
    -> HsQualType      -- ^ the type to convert
    -> m Sigma
hsQualTypeToType kindEnv qs qualType = return $ hoistType $ tForAll quantOver ( ps' :=> t') where
   newEnv = kiHsQualType kindEnv qualType
   --newEnv = kindEnv
   Just t' = hsTypeToType newEnv (hsQualTypeType qualType)
   ps = hsQualTypeHsContext qualType
   ps' = map (hsAsstToPred newEnv) ps
   quantOver = nub $ freeVars ps' ++ fvs
   fvs = case qs of
       Nothing -> []
       Just xs -> [ v | v <- freeVars t', nameName (tyvarName v) `notElem` xs]

hoistType :: Type -> Type
hoistType t = f t where
    f t@TVar {} = t
    f t@TCon {} = t
    f t@TMetaVar {} = t
    f t@TAssoc {} = t { typeClassArgs = map f (typeClassArgs t), typeExtraArgs = map f (typeExtraArgs t) }
    f (TAp a b) = TAp (f a) (f b)
    f (TForAll vs (ps :=> t))
        | (TForAll vs' (ps' :=> t')) <- nt = f $ TForAll (vs ++ vs') ((ps ++ ps') :=> t')
        | otherwise = TForAll vs (ps :=> nt)
        where
        nt = f t
    f (TExists vs (ps :=> t))
        | (TExists vs' (ps' :=> t')) <- nt = f $ TExists (vs ++ vs') ((ps ++ ps') :=> t')
        | otherwise = TExists vs (ps :=> nt)
        where
        nt = f t
    f (TArrow a b)
        | TForAll vs (ps :=> t) <- nb = f $ TForAll vs (ps :=> TArrow na t)
        | TExists vs (ps :=> t) <- na = f $ TForAll vs (ps :=> TArrow t nb)
        | otherwise = TArrow na nb
        where
        na = f a
        nb = f b

