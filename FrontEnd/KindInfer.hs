
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
    hoistType,
    hsTypeToType,
    getConstructorKinds
    ) where

import Control.Monad
import Data.Generics
import Data.IORef
import Data.Monoid
import List (nub)
import qualified Data.Map as Map
import System.IO.Unsafe

import Binary
import DependAnalysis
import Doc.DocLike
import Doc.PPrint
import FrontEnd.Tc.Type(Sigma())
import FrontEnd.Utils
import GenUtil
import Support.FreeVars
import HsSyn
import MapBinaryInstance()
import Name.Name
import qualified Util.Seq as Seq
import Representation hiding (Subst)
import Type(tTTuple)
import Util.ContextMonad
import Util.HasSize


newtype KindEnv = KindEnv (Map.Map Name Kind)
    deriving(Binary,Typeable,Show,HasSize,Monoid)

type Subst = [(Kindvar, Kind)]

nullSubst :: Subst
nullSubst = []

class Kinds a where
   vars :: a -> [Kindvar]
   apply :: Subst -> a -> a

instance Kinds Kind where
   vars Star = []
   vars (KVar kindvar) = [kindvar]
   vars (kind1 `Kfun` kind2) = vars kind1 ++ vars kind2

   apply s Star = Star
   apply s (KVar kindvar)
      = case lookup kindvar s of
           Just k -> k
           Nothing -> KVar kindvar
   apply s (kind1 `Kfun` kind2)
      = (apply s kind1) `Kfun` (apply s kind2)

instance Kinds a => Kinds [a] where
   vars = nub . concatMap vars
   apply s = map (apply s)

instance Kinds a => Kinds (b, a) where
   apply s (x, y) = (x, apply s y)
   vars (x, y) = vars y

instance Kinds KindEnv where
   apply s (KindEnv m) = KindEnv $ Map.map (\el -> apply s el) m
   vars (KindEnv env) = vars $ map snd $ Map.toList env


instance DocLike d =>  PPrint d KindEnv where
    pprint (KindEnv m) = vcat [ pprint x <+> text "=>" <+> pprint y | (x,y) <- Map.toList m]
--------------------------------------------------------------------------------

-- unification

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = [(u, apply s1 k) | (u, k) <- s2] ++ s1


{-# SPECIALIZE mgu :: Kind -> Kind -> KI Subst #-}

-- can return either a substitution or a string
mgu :: Monad m => Kind -> Kind -> m Subst
mgu Star Star = return nullSubst
mgu (k1 `Kfun` k2) (k3 `Kfun` k4) = do
    s1 <- mgu k1 k3
    s2 <- mgu (apply s1 k2) (apply s1 k4)
    return (s2 `composeSubst` s1)
mgu (KVar u) k = varBind u k
mgu k (KVar u) = varBind u k
mgu k1 k2 = fail $ "attempt to unify these two kinds: " ++ show k1 ++ ", " ++ show k2

{-# SPECIALIZE varBind :: Kindvar -> Kind -> KI Subst #-}

varBind :: Monad m => Kindvar -> Kind -> m Subst
varBind u k
   | k == KVar u = return nullSubst
   | u `elem` vars k = fail $ "occurs check failed in kind inference: " ++
                               show u ++ ", " ++ show k
   | otherwise = return [(u, k)]


--------------------------------------------------------------------------------

-- The kind inference monad

data KiEnv  = KiEnv {
    kiContext :: [String],
    kiEnv :: IORef KindEnv,
    kiSubst :: IORef Subst,
    kiVarnum :: IORef Int
    }

newtype KI a = KI (KiEnv -> IO a)-- -> (a, State))


instance Monad KI where
    return a = KI (\_ -> return a)
    KI comp >>= fun
        = KI (\v  -> comp v >>= \r -> case fun r   of KI x -> x v)
    fail x = KI (\s -> fail (unlines $ reverse (x:kiContext s)))


restrictKindEnv :: (Name -> Bool) -> KindEnv -> KindEnv
restrictKindEnv f (KindEnv m) = KindEnv $ Map.filterWithKey (\k _ -> f k) m

--------------------------------------------------------------------------------

-- useful operations in the inference monad

runKI :: KindEnv -> KI a -> IO (a, KindEnv)
runKI env (KI ki) = (kienv >>= ki') where
    kienv = do
        env <- newIORef env
        subst <- newIORef nullSubst
        varnum <- newIORef 0
        return KiEnv { kiContext = [], kiEnv = env, kiSubst = subst, kiVarnum = varnum }
    ki' e = do
        x <- ki e
        env <- readIORef (kiEnv e)
        return (x,env)


instance ContextMonad String KI where
    withContext nc (KI x)= KI (\s -> x s { kiContext = nc :kiContext s })

getSubst :: KI Subst
getSubst = KI $ \e -> do
    readIORef (kiSubst e)

getVarNum :: KI Int
getVarNum = KI $ \e -> do
    readIORef (kiVarnum e)

getEnv :: KI KindEnv
getEnv = KI $ \e -> readIORef (kiEnv e)


getEnvVars :: KI [Kindvar]
getEnvVars
   = do e <- getEnv
        return $ vars e

incVarNum :: KI ()
incVarNum = KI $ \e -> do
    n <- readIORef (kiVarnum e)
    writeIORef (kiVarnum e ) $! (n + 1)

unify :: Kind -> Kind -> KI ()
unify k1 k2 = do
    s <- getSubst
    newSubst <- mgu (apply s k1) (apply s k2)
    extendSubst newSubst
    --case mgu (apply s k1) (apply s k2) of
    --       Right newSubst  -> extendSubst newSubst
    --       Left errorMsg -> error $ unlines (reverse c ++ [errorMsg])


extendSubst :: Subst -> KI ()
extendSubst s = KI $ \e -> do
    modifyIORef (kiSubst e) (s `composeSubst`)

newKindVar :: KI Kind
newKindVar
   = do n <- getVarNum
        incVarNum
        return (KVar (Kindvar n))

lookupKindEnv :: Name -> KI (Maybe Kind)
lookupKindEnv name
   = do KindEnv env <- getEnv
        return $ Map.lookup name env

extendEnv :: KindEnv -> KI ()
extendEnv (KindEnv newEnv) = KI $ \e ->
    modifyIORef (kiEnv e) (\ (KindEnv env) -> KindEnv $ env `Map.union` newEnv)

applySubstToEnv :: Subst -> KI ()
applySubstToEnv subst = KI $ \e ->
    modifyIORef (kiEnv e) (apply subst)

envVarsToStars :: KI ()
envVarsToStars
   = do vars <- getEnvVars
        let varsToStarSubst = map (\v -> (v, Star)) vars   -- clobber all remaining variables to stars
        applySubstToEnv varsToStarSubst


getConstructorKinds :: KindEnv -> Map.Map Name Kind
getConstructorKinds (KindEnv m) = m -- Map.fromList [ (toName TypeConstructor x,y) | (x,y)<- Map.toList m]

--------------------------------------------------------------------------------

-- kind inference proper
-- this is what gets called from outside of this module
kiDecls :: KindEnv -> [HsDecl] -> IO KindEnv
kiDecls inputEnv classAndDataDecls = run >>= return . snd where
   run = runKI inputEnv $ withContext ("kiDecls: " ++ show (map getDeclName classAndDataDecls)) $ mapM_ kiKindGroup kindGroups
   kindGroups = map declsToKindGroup depGroups
   depGroups = getDataAndClassBg classAndDataDecls

kiKindGroup :: KindGroup -> KI ()
kiKindGroup tap@(classDecls, heads, context, dataBodies, classBodies) = do
        withContext ("kiKindGroup: " ++ show tap) $ do
        mapM_ kiClassDecl classDecls
        mapM_ kiTyConDecl heads
        mapM_ kiAsst context
        dataBodyKinds <- mapM (kiType False) dataBodies        -- vars must be seen previously here (hence True)
        --mapM_ (\k -> unify k Star) dataBodyKinds                set to true for existentials
        classBodyKinds <- mapM (kiQualType False) classBodies  -- vars may not have been seen previously here (hence False)
        --mapM_ (\k -> unify k Star) classBodyKinds
        currentSubst <- getSubst
        applySubstToEnv currentSubst
        envVarsToStars


kiTyConDecl :: DataDeclHead -> KI ()
kiTyConDecl (tyconName, args) = do
        argKindVars <- mapM newNameVar args
        let tyConKind = foldr Kfun Star $ map snd argKindVars
        let newEnv = KindEnv $ Map.fromList $ [(toName TypeConstructor tyconName, tyConKind)] ++ argKindVars
        extendEnv newEnv

kiClassDecl :: (HsName,[HsName]) -> KI ()
--kiClassDecl nn | trace ("kiClassDecl: " ++ show nn) False = undefined
kiClassDecl (className, argNames) = do
        varKind <- newKindVar
        let newEnv = KindEnv $ Map.fromList $ (toName ClassName className, varKind): [(toName TypeVal argName, varKind) | argName <- argNames]
        extendEnv newEnv

-- here we expext the classname to be already defined and should be in the
-- environment, we do not require that the variables will be defined
kiAsst :: HsAsst -> KI Kind
kiAsst x@(className, argName) = withContext ("kiAsst: " ++ show x) $ do
    classKind <- lookupKindEnv (toName ClassName className)
    case classKind of
           Nothing -> fail $ "kiAsst: could not find kind information for class: " ++ show className
           Just ck -> do argKind <- lookupKindEnv (toName TypeVal argName)
                         case argKind of
                            --Nothing -> error  $ "kiAsst: could not find kind information for class/arg: " ++ show className ++ "/" ++ show argName
                            Nothing -> do varKind <- newKindVar
					  extendEnv $ KindEnv $ Map.singleton (toName TypeVal argName) varKind
                                          unify ck varKind
                                          return ck
                            Just ak -> do unify ck ak
                                          return ck

kiQualType :: Bool -> HsQualType -> KI Kind
kiQualType varExist qt@(HsQualType cntxt t) = do
        withContext ("kiQualType: " ++ show qt) $ do
        mapM_ kiAsst cntxt
        kiType varExist t


-- boolean arg = True = throw error if var does not exist
--               False = if var does not exist then add it to the environment

kiType :: Bool -> HsType -> KI Kind
kiType _ tap@(HsTyCon name) = do
        withContext ("kiType: " ++ show tap) $ do
        tyConKind <- lookupKindEnv (toName TypeConstructor name)
        case tyConKind of
           Nothing
              -> do env <- getEnv
                    fail $ "kiType: could not find kind for this constructor: " ++ show name ++
                         "\nin this kind environment:\n" ++ pprint env
           Just k -> return k

kiType varExist tap@(HsTyVar name) = do
        withContext ("kiType: " ++ show tap) $ do
        varKind <- lookupKindEnv (toName TypeVal name)
        case varKind of
           Nothing
              -> case varExist of
                    True
                       -> fail $ "kiType: could not find kind for this type variable: " ++ show name
                    False -> do varKind <- newKindVar
				extendEnv $ KindEnv $ Map.singleton (toName TypeVal name) varKind
                                return varKind
           Just k -> return k

-- kind(t1) = kind(t2) -> var

kiType varExist tap@(HsTyApp t1 t2) = do
        withContext ("kiType: " ++ show tap) $ do
        k1 <- kiType varExist t1
        k2 <- kiType varExist t2
        varKind <- newKindVar
        unify k1 (k2 `Kfun` varKind)
        return varKind

-- kind(->) = * -> * -> *
-- kind (t1 -> t2) = *, |- kind(t1) = *, kind(t2) = *


kiType varExist tap@(HsTyFun t1 t2) = do
        withContext ("kiType: " ++ show tap) $ do
        k1 <- kiType varExist t1
        k2 <- kiType varExist t2
        unify k1 Star
        unify k2 Star
        return Star

-- kind (t1, t2, ..., tn) = *
-- |- kind(t1) = *, kind(t2) = *, ... , kind(tn) = *

kiType varExist tap@(HsTyTuple ts) = do
        withContext ("kiType: " ++ show tap) $ do
        tsKs <- mapM (kiType varExist) ts
        mapM_ (\k -> unify k Star) tsKs
        return Star

kiType varExist tap@(HsTyForall { hsTypeVars = vs, hsTypeType = qt }) = do
    argKindVars <- mapM (newNameVar . hsTyVarBindName) vs
    let newEnv = KindEnv $ Map.fromList $ argKindVars
    extendEnv newEnv
    kiQualType varExist qt
kiType varExist tap@(HsTyExists { hsTypeVars = vs, hsTypeType = qt }) = do
    argKindVars <- mapM (newNameVar . hsTyVarBindName) vs
    let newEnv = KindEnv $ Map.fromList $ argKindVars
    extendEnv newEnv
    kiQualType varExist qt

newNameVar :: HsName -> KI (Name, Kind)
newNameVar n = do
    newVar <- newKindVar
    return (toName TypeVal n, newVar)


--------------------------------------------------------------------------------

-- code for getting the kinds of variables in type sigs

kiHsQualType :: KindEnv -> HsQualType -> KindEnv
kiHsQualType inputEnv qualType = newState where
    (_, newState) = unsafePerformIO $ runKI inputEnv $ withContext ("kiHsQualType: " ++ show qualType) $ do
        kiQualType False qualType
        envVarsToStars

{-
kiHsQualTypePredPred :: KindEnv -> HsQualType -> KindEnv
kiHsQualTypePredPred inputEnv qt@(HsQualType cntxt (HsTyApp (HsTyCon className) t))  = env newState where
    (_, newState) = runKI inputEnv $ do
        withContext ("kiQualTypePredPred: " ++ show qt) $ do
        mapM_ kiAsst (cntxt)
        kt <- kiType False t
        Just ck <- lookupKindEnv className
        unify kt ck
        envVarsToStars
-}

--------------------------------------------------------------------------------

getDataAndClassBg :: [HsDecl] -> [[HsDecl]]
getDataAndClassBg decls = getBindGroups decls getDeclName dataAndClassDeps

dataAndClassDeps :: HsDecl -> [Name]
dataAndClassDeps (HsDataDecl _sloc cntxt _name _args condecls _derives)
   = snub $ namesFromContext cntxt ++ (concatMap namesFromType $ concatMap conDeclToTypes condecls) ++ concatMap conDeclNames condecls
dataAndClassDeps (HsNewTypeDecl _sloc cntxt _name _args condecl _derives)
   = snub $ namesFromContext cntxt ++ (concatMap namesFromType $ conDeclToTypes condecl) ++ conDeclNames condecl
dataAndClassDeps (HsClassDecl _sloc (HsQualType cntxt _classApp) decls)
   = snub $ namesFromContext cntxt ++ (concat [ namesFromQualType (typeFromSig s) | s <- decls,  isHsTypeSig s])

namesFromQualType :: HsQualType -> [Name]
namesFromQualType (HsQualType cntxt t) = namesFromContext cntxt ++ namesFromType t

namesFromType :: HsType -> [Name]
namesFromType (HsTyFun t1 t2) = namesFromType t1 ++ namesFromType t2
namesFromType (HsTyTuple ts) = concatMap namesFromType ts
namesFromType (HsTyApp t1 t2) = namesFromType t1 ++ namesFromType t2
namesFromType (HsTyVar _) = []
namesFromType (HsTyCon n) = [toName TypeConstructor n]
namesFromType (HsTyForall _vs qt) = namesFromQualType qt -- map (toName TypeVal . hsTyVarBindName) vs
namesFromType (HsTyExists _vs qt) = namesFromQualType qt -- map (toName TypeVal . hsTyVarBindName) vs
--namesFromType HsTyExists { hsTypeVars = vs } = map (toName TypeVal . hsTyVarBindName) vs

namesFromContext :: HsContext -> [Name]
namesFromContext cntxt = map fst (hsContextToContext cntxt)

--------------------------------------------------------------------------------

-- (type constructor name, arguments to constructor)
type DataDeclHead = (HsName, [HsName])
-- (class decls, data decl heads, class and data contexts, types in body of data decl, types in body of class)
type KindGroup = ([(HsName,[HsName])], [DataDeclHead], HsContext, [HsType], [HsQualType])
-- data KindGroup = KindGroup {
--     kgClassDecls :: [(HsName,[HsName])],
--     kgDataDecls ::[DataDeclHead],
--     kgContexts ::HsContext,
--     kgTypes ::[HsType],
--     kgQualTypes ::[HsQualType]
--     }

declsToKindGroup :: [HsDecl] -> KindGroup
declsToKindGroup [] = ([], [], [], [], [])

declsToKindGroup ((HsDataDecl _sloc context tyconName tyconArgs condecls _derives):decls)
   = (restClassDecls,
      newHead:restDataHeads,
      context++restContext,
      newBodies ++ restDataBodies,
      restClassBodies)
   where
   (restClassDecls, restDataHeads, restContext, restDataBodies, restClassBodies)
      = declsToKindGroup decls
   newHead = (tyconName, tyconArgs)
   newBodies = concatMap conDeclToTypes condecls

declsToKindGroup ((HsNewTypeDecl _sloc context tyconName tyconArgs condecl _derives):decls)
   = (restClassDecls,
      newHead:restDataHeads,
      context++restContext,
      newBodies ++ restDataBodies,
      restClassBodies)
   where
   (restClassDecls, restDataHeads, restContext, restDataBodies, restClassBodies)
      = declsToKindGroup decls
   newHead = (tyconName, tyconArgs)
   newBodies = conDeclToTypes condecl


declsToKindGroup (HsClassDecl _sloc qualType sigsAndDefaults : decls)
   = (newClassDecl:restClassDecls,
      restDataHeads,
      newContext++restContext,
      restDataBodies,
      newClassBodies++restClassBodies)
   where
   (restClassDecls, restDataHeads, restContext, restDataBodies, restClassBodies) = declsToKindGroup decls
   newClassBodies = map typeFromSig $ filter isHsTypeSig sigsAndDefaults
   --rn = runIdentity $ applyTU (full_tdTU $ adhocTU (constTU ([])) f) newClassBodies
   --f (HsTyVar n') | hsNameToOrig n' == hsNameToOrig classArg = return [n']
   --f _ = return []
   rn = Seq.toList $ everything (Seq.<>) (mkQ Seq.empty f) newClassBodies
   f (HsTyVar n') | hsNameToOrig n' == hsNameToOrig classArg = Seq.single n'
   f _ = Seq.empty
   (newClassDecl, newContext) = ((className, classArg:rn), contxt)
   HsQualType contxt (HsTyApp (HsTyCon className) (HsTyVar classArg)) =  qualType


conDeclToTypes :: HsConDecl -> [HsType]
conDeclToTypes rd = map bangTypeToType (hsConDeclArgs rd)

conDeclNames :: HsConDecl -> [Name]
conDeclNames rd = map (toName TypeVal) $ map hsTyVarBindName $ hsConDeclExists rd
--conDeclToTypes (HsConDecl _sloc name bangTypes)
--   = map bangTypeToType bangTypes
--   = error $ "conDeclToType (HsRecDecl _lsoc _name _recs): not implemented yet"

bangTypeToType :: HsBangType -> HsType
bangTypeToType (HsBangedTy t) = t
bangTypeToType (HsUnBangedTy t) = t

typeFromSig :: HsDecl -> HsQualType
typeFromSig (HsTypeSig _sloc _names qualType) = qualType

--------------------------------------------------------------------------------

kindOf :: Name -> KindEnv -> Kind
kindOf name (KindEnv env) = case Map.lookup name env of
            Nothing | nameType name `elem` [TypeConstructor,TypeVal] -> Star
            Just k -> k
            _ -> error $ "kindOf: could not find kind of : " ++ show (nameType name,name)

kindOfClass :: Name -> KindEnv -> [Kind]
kindOfClass name (KindEnv env) = case Map.lookup name env of
        --Nothing -> Star
        Nothing -> error $ "kindOf: could not find kind of class : " ++ show (nameType name,name)
        Just k -> [k]

----------------------
-- Conversion of Types
----------------------

-- note that the types are generated without generalised type
-- variables, ie there will be no TGens in the output
-- to get the generalised variables a second phase
-- of generalisation must be applied

aHsTypeToType :: KindEnv -> HsType -> Type
aHsTypeToType kt (HsTyFun t1 t2) = aHsTypeToType kt t1 `fn` aHsTypeToType kt t2
aHsTypeToType kt tuple@(HsTyTuple types) = tTTuple $ map (aHsTypeToType kt) types
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
hsAsstToPred kt (className, varName)
   -- = IsIn className (TVar $ Tyvar varName (kindOf varName kt))
   | isConstructorLike (hsIdentString . hsNameIdent $ varName) = IsIn  (toName ClassName className) (TCon (Tycon (toName TypeConstructor varName) (head $ kindOfClass (toName ClassName className) kt)))
   | otherwise = IsIn (toName ClassName className) (TVar $ tyvar (toName TypeVal varName) (head $ kindOfClass (toName ClassName className) kt))



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

