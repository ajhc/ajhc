module FrontEnd.Rename(unRename, collectDefsHsModule, renameModule, FieldMap, renameStatement ) where

import Char
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative
import Data.Monoid
import List
import Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set


import Doc.DocLike(tupled)
import FrontEnd.Desugar (doToExp,listCompToExp)
import FrontEnd.SrcLoc hiding(srcLoc)
import FrontEnd.Syn.Traverse
import FrontEnd.Utils
import FrontEnd.HsSyn
import Name.Name as Name hiding(qualifyName)
import Name.Names
import Support.FreeVars
import Util.Gen
import Util.Inst()
import FrontEnd.Warning
import qualified FlagOpts as FO
import qualified FrontEnd.HsErrors as HsErrors
import qualified Name.VConsts as V
import Options

type FieldMap =  (Map.Map Name Int,Map.Map Name [(Name,Int)])

--------------------------------------------------------------------------------

-- a 'Substitution Table' which is a map from old names to new names
-- All names in the current scope are stored in here, with their renamings

type SubTable = Map.Map HsName HsName

-- an Identifier Table is a map from renamed names to that identifier's source
-- location and binding type


-- the monadic state

data ScopeState = ScopeState {
    unique         :: !Int,
    globalSubTable :: Map.Map HsName HsName,  -- Current substition
    typeSubTable   :: Map.Map HsName HsName,  -- type substition table
    errorTable     :: Map.Map HsName String,  -- special error message. else it's just unknown.
    fieldLabels    :: FieldMap
    }


data Env = Env {
    envSubTable  :: Map.Map HsName HsName,  -- all these need to go away
    envNameSpace :: [NameType],
    envModule  :: Module,
    envNameMap :: Map.Map Name (Either String Name),
    envOptions :: Opt,
    envSrcLoc  :: SrcLoc
}

instance Applicative RM where
    pure = return
    (<*>) = ap

newtype RM a = RM (RWS Env [Warning] ScopeState a)
    deriving(Monad,Functor,MonadReader Env, MonadWriter [Warning], MonadState ScopeState)

unRM (RM x) = x

instance MonadWarn RM where
    addWarning w = tell [w]

instance UniqueProducer RM where
    newUniq = do
        u <- gets unique
        modify (\state -> state {unique = (unique state) + 1})
        return u


getCurrentModule :: RM Module
getCurrentModule = asks envModule

instance MonadSrcLoc RM where
    getSrcLoc = asks envSrcLoc
instance MonadSetSrcLoc RM where
    withSrcLoc sl a = local (\s -> s { envSrcLoc = sl `mappend` envSrcLoc s}) a



addTopLevels ::  [HsDecl]  -> RM a -> RM a
addTopLevels  hsDecls action = do
    mod <- getCurrentModule
    let (ns,ts) = mconcat (map namesHsDecl hsDecls)
        nm = Map.fromList $ foldl f [] (fsts ns)
        tm = Map.fromList $ foldl f [] (fsts ts)
        f r hsName@Qual {}
            | Just _ <- V.fromTupname hsName, Module "Jhc.Basics" <- mod
                = let nn = hsName in (nn,nn):r
            | nameName tc_Arrow == hsName, Module "Jhc.Basics" == mod
                = let nn = hsName in (nn,nn):r
            | otherwise = error $ "strong bad: " ++ show hsName
        f r z@(UnQual n) = let nn = Qual mod n in (z,nn):(nn,nn):r
        z ns = mapM mult (filter (\x -> length x > 1) $ groupBy (\a b -> fst a == fst b) (sort ns))
        mult xs@(~((n,sl):_)) = warn sl "multiply-defined" (show n ++ " is defined multiple times: " ++ show xs )
    z ns >> z ts
    modify (\s -> s { globalSubTable = nm `Map.union` globalSubTable s })
    modify (\s -> s { typeSubTable = tm `Map.union` typeSubTable s })
    action


ambig x ys = "Ambiguous Name: " ++ show x ++ "\nCould refer to: " ++ tupled (map show ys)

runRename :: MonadWarn m => (a -> RM a) -> Opt -> Module -> FieldMap -> [(Name,[Name])] -> a -> m a
runRename doit opt mod fls ns m = mapM_ addWarning errors >> return renamedMod where
    initialGlobalSubTable = Map.fromList [ (x,y) | ((typ,x),[y]) <- ns', typ == Val || typ == DataConstructor ]
    initialTypeSubTable = Map.fromList [ (x,y) | ((typ,x),[y]) <- ns', typ == TypeConstructor || typ == ClassName ]
    nameMap = Map.fromList $ map f ns where
        f (x,[y]) = (x,Right y)
        f (x,ys)  = (x,Left $ ambig x ys)
    ns' = map fn ns
    fn (n,ns) = (fromName n, map nameName ns)

    errorTab =  Map.fromList [ (x,ambig x ys) | ((typ,x),ys@(_:_:_)) <- ns' ]

    startState = ScopeState {
        typeSubTable   = initialTypeSubTable,
        errorTable     = errorTab,
        unique         = 1,   -- start the counting at 1
        globalSubTable = initialGlobalSubTable,
        fieldLabels    = fls
        }
    startEnv = Env {
        envSubTable = initialGlobalSubTable,
        envNameSpace = [Val,DataConstructor],
        envModule = mod,
        envNameMap  = nameMap,
        envOptions = opt,
        envSrcLoc = mempty
    }
    (renamedMod, _, errors) = runRWS (unRM $ doit m) startEnv startState

{-# NOINLINE renameModule #-}
renameModule :: MonadWarn m => Opt -> FieldMap -> [(Name,[Name])] -> HsModule -> m HsModule
renameModule opt fls ns m = runRename renameDecls opt (hsModuleName m) fls ns m

{-# NOINLINE renameStatement #-}
renameStatement :: MonadWarn m => FieldMap -> [(Name,[Name])] ->  Module -> HsStmt -> m HsStmt
renameStatement fls ns modName stmt = runRename rename options modName fls ns stmt

renameOld :: (SubTable -> RM a) -> RM a
renameOld rm = asks envSubTable >>= rm

withSubTable :: SubTable -> RM a -> RM a
withSubTable st action = local ( \e -> e { envSubTable = st `Map.union` envSubTable e }) action



renameDecls :: HsModule -> RM HsModule
renameDecls tidy = do
        addTopLevels (hsModuleDecls tidy) $ do
        gst <- gets globalSubTable
        withSubTable gst $ do
        decls' <- rename (hsModuleDecls tidy)
        mapM_ HsErrors.hsDeclTopLevel decls'
        return tidy { hsModuleDecls = decls' }


expandTypeSigs :: [HsDecl] -> [HsDecl]
expandTypeSigs ds =  (concatMap f ds) where
    f (HsTypeSig sl ns qt) =  [ HsTypeSig sl [n] qt | n <- ns]
    f d = return d

instance Rename HsDecl where
    rename (HsPatBind srcLoc hsPat hsRhs {-where-} hsDecls) = do
        withSrcLoc srcLoc $ do
        hsPat'    <- rename hsPat
        updateWith hsDecls $ do
        hsDecls'  <- rename hsDecls
        hsRhs'    <- rename hsRhs
        return (HsPatBind srcLoc hsPat' hsRhs' {-where-} hsDecls')

    rename (HsForeignExport a b n t) = do
        withSrcLoc a $ do
        n <- rename n
        updateWith t $ do
            t <- rename t
            return (HsForeignExport a b n t)

    rename (HsForeignDecl a b n t) = do
        withSrcLoc a $ do
        n <- rename n
        updateWith t $ do
        t <- rename t
        return (HsForeignDecl a b n t)

    rename (HsFunBind hsMatches) = do
        hsMatches' <- rename hsMatches
        return (HsFunBind hsMatches')

    rename (HsTypeSig srcLoc hsNames hsQualType) = do
        withSrcLoc srcLoc $ do
        hsNames' <- rename hsNames
        updateWith hsQualType $ do
        hsQualType' <- rename hsQualType
        return (HsTypeSig srcLoc hsNames' hsQualType')
    rename dl@HsDataDecl { hsDeclSrcLoc = srcLoc, hsDeclContext = hsContext, hsDeclName = hsName, hsDeclArgs = hsNames1, hsDeclCons = hsConDecls, hsDeclDerives = hsNames2 } = do
        withSrcLoc srcLoc $ do
        hsName' <- renameTypeName hsName
        updateWith hsNames1 $ do
        hsContext' <- rename hsContext
        hsNames1' <- rename hsNames1
        hsConDecls' <- rename hsConDecls
        -- don't need to rename the hsNames2 as it is just a list of TypeClasses
        hsNames2' <- mapM renameTypeName hsNames2
        return dl { hsDeclContext = hsContext', hsDeclName = hsName', hsDeclArgs = hsNames1', hsDeclCons = hsConDecls', hsDeclDerives = hsNames2' }
    rename (HsTypeDecl srcLoc name hsNames t) = do
        withSrcLoc srcLoc $ do
        hsName' <- renameTypeName name
        updateWith (Set.toList $ freeVars hsNames :: [HsName]) $ do
            hsNames' <- rename hsNames
            t' <- rename t
            return (HsTypeDecl srcLoc  hsName' hsNames' t')
    rename (HsNewTypeDecl srcLoc hsContext hsName hsNames1 hsConDecl hsNames2) = do
        withSrcLoc srcLoc $ do
        hsName' <- renameTypeName hsName
        updateWith hsNames1 $ do
        hsContext' <- rename hsContext
        hsNames1' <- rename hsNames1
        hsConDecl' <- rename hsConDecl
        -- don't need to rename the hsNames2 as it is just a list of TypeClasses
        hsNames2' <- mapM renameTypeName hsNames2
        return (HsNewTypeDecl srcLoc hsContext' hsName' hsNames1' hsConDecl' hsNames2')
    rename (HsClassDecl srcLoc hsQualType hsDecls) = do
        withSrcLoc srcLoc $ do
        hsQualType' <- updateWith hsQualType  $ rename hsQualType
        doesClassMakeSense hsQualType'
        hsDecls' <- rename hsDecls
        return (HsClassDecl srcLoc hsQualType' hsDecls')
    rename (HsClassAliasDecl srcLoc name args hsContext hsClasses hsDecls) = do
        withSrcLoc srcLoc $ do
        name' <- renameTypeName name
        updateWith args $ do
        args' <- mapM rename args
        hsContext' <- rename hsContext
        hsClasses' <- rename hsClasses
        hsDecls' <- rename hsDecls
        return (HsClassAliasDecl srcLoc name' args' hsContext' hsClasses' hsDecls')
    rename (HsInstDecl srcLoc hsQualType hsDecls) = do
        withSrcLoc srcLoc $ do
        updateWith hsQualType $ do
        hsQualType' <- rename hsQualType
        hsDecls' <- rename hsDecls
        return (HsInstDecl srcLoc hsQualType' hsDecls')
    rename (HsInfixDecl srcLoc assoc int hsNames) = do
        withSrcLoc srcLoc $ do
        hsNames' <- rename hsNames
        return $ HsInfixDecl srcLoc assoc int hsNames'
    rename (HsActionDecl srcLoc pat e) = do
        withSrcLoc srcLoc $ do
        pat <- rename pat
        e <- rename e
        return (HsActionDecl srcLoc pat e)
    rename (HsPragmaProps srcLoc prop hsNames) = do
        withSrcLoc srcLoc $ do
        hsNames' <- rename hsNames
        return (HsPragmaProps  srcLoc prop hsNames')
    rename (HsPragmaRules rs) = do
        rs' <- rename rs
        return $ HsPragmaRules rs'
    rename prules@HsPragmaSpecialize { hsDeclSrcLoc = srcLoc, hsDeclName = n, hsDeclType = t } = do
        withSrcLoc srcLoc $ do
        n <- rename n
        t <- rename t
        m <- getCurrentModule
        i <- newUniq
        return prules { hsDeclUniq = (m,i), hsDeclName = n, hsDeclType = t }
    rename (HsDefaultDecl sl e) = HsDefaultDecl sl <$> rename e
    rename (HsDeclDeriving sl ch) = HsDeclDeriving sl <$> rename ch
    rename h = error $ "renameerr: " ++ show h


instance Rename HsClassHead where
    rename (HsClassHead cx n ts) = do
        updateWith ts $ HsClassHead <$> rename cx <*> renameTypeName n <*> rename ts



instance Rename HsRule where
    rename prules@HsRule { hsRuleSrcLoc = srcLoc, hsRuleFreeVars = fvs, hsRuleLeftExpr = e1, hsRuleRightExpr = e2 } = do
        withSrcLoc srcLoc $ do
        updateWith (fsts fvs) $ do
        subTable'' <- getUpdates (catMaybes $ snds fvs)
        fvs' <- sequence [ liftM2 (,) (rename x) (withSubTable subTable'' $ rename y)| (x,y) <- fvs]
        e1' <- rename e1
        e2' <- rename e2
        m <- getCurrentModule
        i <- newUniq
        return prules {  hsRuleUniq = (m,i), hsRuleFreeVars = fvs', hsRuleLeftExpr = e1', hsRuleRightExpr = e2' }

doesClassMakeSense :: HsQualType -> RM ()
doesClassMakeSense (HsQualType _ type_) = case type_ of
    (HsTyApp (HsTyCon _) (HsTyVar _)) -> return ()
    (HsTyApp (HsTyApp _ _) _)         -> failRename "Multiparameter typeclasses not supported"
    (HsTyCon _)                       -> failRename "Typeclass with no parameters"
    _                                 -> failRename $ "Invalid type in class declaration: "++show type_

instance Rename HsQualType where
    rename (HsQualType hsContext hsType) = return HsQualType `ap` rename hsContext `ap` rename hsType


instance Rename HsAsst where
    rename (HsAsst hsName1  hsName2s) = do
        hsName1' <- renameTypeName hsName1
        hsName2s' <- mapM renameTypeName hsName2s
        return (HsAsst hsName1' hsName2s')
    rename (HsAsstEq t1 t2) = return HsAsstEq `ap` rename t1 `ap` rename t2


instance Rename HsConDecl where
    rename cd@(HsConDecl { hsConDeclSrcLoc = srcLoc, hsConDeclName = hsName, hsConDeclConArg = hsBangTypes }) = do
        withSrcLoc srcLoc $ do
        hsName' <- rename hsName
        updateWith  (map hsTyVarBindName (hsConDeclExists cd)) $ do
        es <- rename (hsConDeclExists cd)
        hsBangTypes' <- rename hsBangTypes
        return cd { hsConDeclName = hsName', hsConDeclConArg = hsBangTypes', hsConDeclExists = es }
    rename cd@HsRecDecl { hsConDeclSrcLoc = srcLoc, hsConDeclName = hsName, hsConDeclRecArg = stuff} = do
        withSrcLoc srcLoc $ do
        hsName' <- rename hsName
        subTable <- asks envSubTable
        updateWith (map hsTyVarBindName (hsConDeclExists cd)) $ do
        es <- rename (hsConDeclExists cd)
        stuff' <- sequence [ do ns' <- rename ns; t' <- withSubTable subTable $ rename t; return (ns',t')  |  (ns,t) <- stuff]
        return cd { hsConDeclName = hsName', hsConDeclRecArg = stuff', hsConDeclExists = es }


instance Rename HsBangType where
    rename (HsBangedTy t) = HsBangedTy `fmap` rename t
    rename (HsUnBangedTy t) = HsUnBangedTy `fmap` rename t

instance Rename HsType where
    rename t = do
        t <- renameHsType' True t
        HsErrors.hsType t
        return t

renameHsType' dovar t = pp (rt t) where
    rt :: HsType -> RM HsType
    rt (HsTyVar hsName) | dovar = do
        hsName' <- renameTypeName hsName
        return (HsTyVar hsName')
    rt v@(HsTyVar _)   = return v

    rt (HsTyCon hsName) = do
        hsName' <- renameTypeName hsName
        return (HsTyCon hsName')
    rt (HsTyForall ts v) = do
        updateWith (map hsTyVarBindName ts)  $ do
        ts' <- rename ts
        v' <- rename v
        return $ HsTyForall ts' v'
    rt (HsTyExists ts v) = do
        updateWith (map hsTyVarBindName ts) $ do
        ts' <- rename ts
        v' <- rename v
        return $ HsTyExists ts' v'
    rt ty = traverseHsType (renameHsType' dovar) ty
    pp t | not dovar = t
    pp t = t


class UpdateTable a where
    updateWith :: a -> RM b -> RM b
    updateWith x action = getUpdates x >>= flip withSubTable action

    getUpdates :: a -> RM SubTable
    getUpdates x = Map.unions `fmap` mapM clobberName (getNames x)

    getNames :: a -> [HsName]
    getNames a = []


instance UpdateTable a => UpdateTable [a] where
    getUpdates xs = Map.unions `fmap` mapM getUpdates xs
    getNames xs = concatMap getNames xs

instance UpdateTable HsName where
    getNames x = [x]

class Rename a where
    rename :: a -> RM a
    rename x = return x


instance Rename x => Rename (Located x) where
    rename (Located sl x) = Located sl `fmap` rename x

instance Rename SrcLoc where

instance Rename a => Rename [a] where
    rename xs = mapM rename xs



instance (Rename a,Rename b) => Rename (a,b) where
    rename (a,b) = return (,) `ap` rename a `ap` rename b


instance Rename a => Rename (Maybe a) where
    rename Nothing = return Nothing
    rename (Just x) = fmap Just $ rename x






instance Rename HsTyVarBind where
    rename tvb@HsTyVarBind { hsTyVarBindName = n } = do
        n' <- renameTypeName n
        return tvb { hsTyVarBindName = n' }

-- note that for renameHsMatch, the 'wheres' dominate the 'pats'

instance Rename HsMatch where
    rename (HsMatch srcLoc hsName hsPats hsRhs {-where-} hsDecls) = do
        withSrcLoc srcLoc $ do
        hsName' <- rename hsName
        updateWith hsPats  $ do
        hsPats' <- rename hsPats
        updateWith hsDecls $ do
        hsDecls' <- rename (expandTypeSigs hsDecls)
        mapM_ HsErrors.hsDeclLocal hsDecls'
        hsRhs' <- rename hsRhs
        return (HsMatch srcLoc hsName' hsPats' hsRhs' {-where-} hsDecls')


instance Rename HsPat where
    rename (HsPVar hsName) = HsPVar `fmap` rename hsName
    rename (HsPInfixApp hsPat1 hsName hsPat2)  = return HsPInfixApp `ap` rename hsPat1 `ap` rename hsName `ap` rename hsPat2
    rename (HsPApp hsName hsPats) = HsPApp <$> rename hsName <*> rename hsPats
    rename (HsPRec hsName hsPatFields) = do
        hsName' <- rename hsName
        hsPatFields' <- rename hsPatFields
        fls <- gets fieldLabels
        buildRecPat fls hsName' hsPatFields'
    rename (HsPAsPat hsName hsPat) = HsPAsPat <$> rename hsName <*> rename hsPat
    rename (HsPTypeSig sl hsPat qt)  = HsPTypeSig sl <$> rename hsPat <*> rename qt
    rename p = traverseHsPat rename p

buildRecPat :: FieldMap -> HsName -> [HsPatField] -> RM HsPat
buildRecPat (amp,fls) n us = case Map.lookup (toName DataConstructor n) amp of
    Nothing -> failRename $ "Unknown Constructor: " ++ show n
    Just t -> do
        let f (HsPFieldPat x p) = case  Map.lookup (toName FieldLabel x) fls of
                Nothing -> failRename $ "Field Label does not exist: " ++ show x
                Just cs -> case lookup n [ (nameName x,(y)) | (x,y) <- cs ] of
                    Nothing -> failRename $ "Field Label does not belong to constructor: " ++ show (x,n)
                    Just i -> return (i,HsPParen p)
        fm <- mapM f us
        let g i | Just e <- lookup i fm = return e
                | otherwise = do
                    v <- newVar
                    return $ HsPVar v
        rs <- mapM g [0 .. t - 1 ]
        return $ HsPApp n rs


instance Rename HsPatField where
    rename (HsPFieldPat hsName hsPat) = do
        gt <- gets globalSubTable      -- field names are not shadowed by local definitions.
        hsName' <- renameHsName hsName gt
        hsPat' <- rename hsPat
        return (HsPFieldPat hsName' hsPat')


instance Rename HsRhs where
    rename (HsUnGuardedRhs hsExp) = fmap HsUnGuardedRhs $ rename hsExp
    rename (HsGuardedRhss rs) = fmap HsGuardedRhss $ rename rs

instance Rename HsGuardedRhs where
    rename (HsGuardedRhs srcLoc hsExp1 hsExp2) = do
        withSrcLoc srcLoc $ do
        hsExp1' <- rename hsExp1
        hsExp2' <- rename hsExp2
        return (HsGuardedRhs srcLoc hsExp1' hsExp2')


f_fromRational = HsVar $ nameName (toUnqualified v_fromRational)

newVar = do
    unique <- newUniq
    mod <- getCurrentModule
    let hsName'' = (Qual mod (HsIdent $ show unique {- ++ fromHsName hsName' -} ++ "_var@"))
    return hsName''


instance Rename HsExp where
    rename (HsVar hsName) = return HsVar `ap` rename hsName
    rename (HsCon hsName) = return HsCon `ap` rename hsName
    rename i@(HsLit HsInt {}) = do return i
    rename i@(HsLit HsFrac {}) = do
        z <- rename f_fromRational
        return $ HsParen (HsApp z i)
    rename (HsLambda srcLoc hsPats hsExp) = do
        withSrcLoc srcLoc $ do
        updateWith hsPats $ do
        hsPats' <- rename hsPats
        hsExp' <- rename hsExp
        return (HsLambda srcLoc hsPats' hsExp')
    rename (HsLet hsDecls hsExp) = do
        updateWith hsDecls $ do
        hsDecls' <- rename (expandTypeSigs hsDecls)
        mapM_ HsErrors.hsDeclLocal hsDecls'
        hsExp' <- rename hsExp
        return (HsLet hsDecls' hsExp')
    rename (HsCase hsExp hsAlts) = do return HsCase `ap` rename hsExp `ap` rename hsAlts
    rename (HsDo hsStmts) = rename =<< doToExp newVar (nameName v_bind) (nameName v_bind_) (nameName v_fail) hsStmts
    rename (HsRecConstr hsName hsFieldUpdates) = do
        hsName' <- rename hsName  -- do I need to change this name?
        hsFieldUpdates' <- rename hsFieldUpdates
        fls <- gets fieldLabels
        buildRecConstr fls (hsName':: HsName) (hsFieldUpdates'::[HsFieldUpdate]) -- HsRecConstr hsName' hsFieldUpdates')
    rename (HsRecUpdate hsExp hsFieldUpdates) = do
        hsExp' <- rename hsExp
        hsFieldUpdates' <- rename hsFieldUpdates
        fls <- gets fieldLabels
        buildRecUpdate fls hsExp' hsFieldUpdates' -- HsRecConstr hsName' hsFieldUpdates')
        --return (HsRecUpdate hsExp' hsFieldUpdates')
    rename (HsEnumFrom hsExp) = rename $ desugarEnum "enumFrom" [hsExp]
    rename (HsEnumFromTo hsExp1 hsExp2) = rename $  desugarEnum "enumFromTo" [hsExp1, hsExp2]
    rename (HsEnumFromThen hsExp1 hsExp2) = rename $ desugarEnum "enumFromThen" [hsExp1, hsExp2]
    rename (HsEnumFromThenTo hsExp1 hsExp2 hsExp3) = rename $  desugarEnum "enumFromThenTo" [hsExp1, hsExp2, hsExp3]
    rename (HsListComp hsExp hsStmts) = do
        (ss,e) <- renameHsStmts hsStmts (rename hsExp)
        listCompToExp newVar e ss
    rename (HsExpTypeSig srcLoc hsExp hsQualType) = do
        hsExp' <- rename hsExp
        updateWith hsQualType $ do
        hsQualType' <- rename hsQualType
        return (HsExpTypeSig srcLoc hsExp' hsQualType')
    rename (HsAsPat hsName hsExp) = return HsAsPat `ap` rename hsName `ap` rename hsExp
    rename (HsWildCard sl) = do
        withSrcLoc sl $ do
            e <- createError HsErrorUnderscore ("_")
            return e
    rename p = traverseHsExp rename p

desugarEnum s as = foldl HsApp (HsVar (nameName $ toName Val s)) as

createError et s = do
    sl <- getSrcLoc
    return $ HsError { hsExpSrcLoc = sl, hsExpErrorType = et, hsExpString = (show sl ++ ": " ++ s) }

failRename s = do
    sl <- getSrcLoc
    fail (show sl ++ ": " ++ s)


buildRecConstr ::  FieldMap -> HsName -> [HsFieldUpdate] -> RM HsExp
buildRecConstr (amp,fls) n us = do
    undef <- createError HsErrorUninitializedField "Uninitialized Field"
    case Map.lookup (toName DataConstructor n) amp of
        Nothing -> failRename $ "Unknown Constructor: " ++ show n
        Just t -> do
            let f (HsFieldUpdate x e) = case  Map.lookup (toName FieldLabel x) fls of
                    Nothing -> failRename $ "Field Label does not exist: " ++ show x
                    Just cs -> case lookup n [ (nameName x,(y)) | (x,y) <- cs ] of
                        Nothing -> failRename $ "Field Label does not belong to constructor: " ++ show (x,n)
                        Just i -> return (i,hsParen e)
            fm <- mapM f us
            let rs = map g [0 .. t - 1 ]
                g i | Just e <- lookup i fm = e
                    | otherwise = undef
            return $ foldl HsApp (HsCon n) rs

buildRecUpdate ::  FieldMap -> HsExp -> [HsFieldUpdate] -> RM HsExp
buildRecUpdate (amp,fls) n us = do
        sl <- getSrcLoc
        let f (HsFieldUpdate x e) = case  Map.lookup (toName FieldLabel x) fls of
                Nothing -> failRename $ "Field Label does not exist: " ++ show x
                Just cs -> return [ (x,(y,hsParen e)) | (x,y) <- cs ]
        fm <- liftM concat $ mapM f us
        let fm' = sortGroupUnderFG fst snd fm
        let g (c,zs) = case Map.lookup c amp of
                Nothing -> failRename $ "Unknown Constructor: " ++ show n
                Just t -> do
                    vars <- replicateM t newVar
                    let vars' = (map HsVar vars)
                    let c' = nameName c
                    let x = foldl HsApp (HsCon c') [ maybe v id (lookup i zs) | v <- vars' | i <- [ 0 .. t - 1] ]
                    return $ HsAlt sl (HsPApp c' (map HsPVar vars))  (HsUnGuardedRhs x) []
        as <- mapM g fm'
        pe <- createError HsErrorRecordUpdate "Record Update Error"
        return $ HsCase n (as ++ [HsAlt sl HsPWildCard (HsUnGuardedRhs pe) []])


instance Rename HsAlt where
    rename (HsAlt srcLoc hsPat hsGuardedAlts {-where-} hsDecls) = withSrcLoc srcLoc $ do
        updateWith hsPat $ do
        hsPat' <- rename hsPat
        updateWith hsDecls $ do
        hsDecls' <- rename (expandTypeSigs hsDecls)
        mapM_ HsErrors.hsDeclLocal hsDecls'
        hsGuardedAlts' <- rename hsGuardedAlts
        return (HsAlt srcLoc hsPat' hsGuardedAlts' hsDecls')


renameHsStmts :: [HsStmt] -> RM a  -> RM ([HsStmt],a)
renameHsStmts ss fe = f ss [] where
    f (HsGenerator sl p e:ss) rs = do
        e' <- rename e
        updateWith p $ do
          p' <- rename p
          f ss (HsGenerator sl p' e':rs)
    f (s:ss) rs = do
        updateWith s $ do
          s' <- rename s
          f ss (s':rs)
    f [] rs = do
        e <- fe
        return (reverse rs,e)


{-
renameHsStmts (hsStmt:hsStmts) exp = do
    updateWith hsStmt $ do
      subTable' <- getUpdates hsStmt
      withSubTable subTable' $ do
      hsStmt' <- withSubTable subTable' $ rename hsStmt
      (hsStmts',subTable'') <- renameHsStmts hsStmts subTable'
      return ((hsStmt':hsStmts'),subTable'')
renameHsStmts [] = do
    fe <- exp
    return ([],subTable)

-- renameHsStmts is trickier than you would expect because
-- the statements are only in scope after they have been declared
-- and thus the subTable must be more carefully threaded through

-- the updated subTable is returned at the end because it is needed by
-- the first section of a list comprehension.

renameHsStmts :: [HsStmt] -> SubTable -> RM (([HsStmt],SubTable))
renameHsStmts (hsStmt:hsStmts) subTable = do
    updateWith hsStmt $ do
      subTable' <- getUpdates hsStmt
      withSubTable subTable' $ do
      hsStmt' <- withSubTable subTable' $ rename hsStmt
      (hsStmts',subTable'') <- renameHsStmts hsStmts subTable'
      return ((hsStmt':hsStmts'),subTable'')
renameHsStmts [] subTable = do
      return ([],subTable)
 -}

instance Rename HsStmt where
    rename (HsGenerator srcLoc hsPat hsExp) = do
        hsExp' <- rename hsExp
        hsPat' <- rename hsPat
        return (HsGenerator srcLoc hsPat' hsExp')
    rename (HsQualifier hsExp) = do
        hsExp' <- rename hsExp
        return (HsQualifier hsExp')
    rename (HsLetStmt hsDecls) = do
        hsDecls' <- rename (expandTypeSigs hsDecls)
        mapM_ HsErrors.hsDeclLocal hsDecls'
        return (HsLetStmt hsDecls')



instance Rename HsFieldUpdate where
    rename (HsFieldUpdate hsName hsExp) = do
        gt <- gets globalSubTable              -- field names are global and not shadowed
        hsName' <- renameHsName hsName gt      -- TODO field names should have own namespace
        hsExp' <- rename hsExp
        return (HsFieldUpdate hsName' hsExp')



instance Rename HsName where
    rename n = renameOld $ renameHsName n

renameTypeName n = renameOld $ renameTypeHsName n

-- This looks up a replacement name in the subtable.
-- Regardless of whether the name is found, if it's not qualified
-- it will be qualified with the current module's prefix.
renameHsName :: HsName -> SubTable -> RM (HsName)
renameHsName hsName subTable
    | nameName tc_Arrow == hsName = return hsName
    | Qual (Module ('@':m)) (HsIdent i) <- hsName = return $ Qual (Module m) (HsIdent i)
renameHsName hsName subTable = case Map.lookup hsName subTable of
    Just name@(Qual _ _) -> return name
    Just _ -> error "renameHsName"
    Nothing
        | Just n <- V.fromTupname hsName -> return hsName
        | otherwise -> do
            sl <- getSrcLoc
            et <- gets errorTable
            let err = case Map.lookup hsName et of {
                Just s -> s;
                Nothing -> "Unknown name: " ++ show hsName }
            warn sl "undefined-name" err
            -- e <- createError ("Undefined Name: " ++ show hsName)
            return $ hsName
            --return (Qual modName name)

renameTypeHsName hsName subTable  =  gets typeSubTable  >>= \t -> case Map.lookup hsName t of
    Just _ -> renameHsName hsName t
    Nothing -> renameHsName hsName subTable

clobberName :: HsName -> RM SubTable
clobberName hsName = do
    unique     <- newUniq
    currModule <- getCurrentModule
    let hsName'     = renameAndQualify hsName unique currModule
    return $ Map.singleton hsName hsName'


renameAndQualify :: HsName -> Int -> Module -> HsName
renameAndQualify name unique currentMod
    = case renameName name unique of
           UnQual name' -> Qual currentMod name'
           qual_name    -> qual_name

-- renames a haskell name with its unique number
renameName :: HsName -> Int -> HsName
renameName n unique = hsNameIdent_u (hsIdentString_u ((show unique ++ "_") ++)) n

-- | unRename gets the original identifier name from the renamed version

unRename :: HsName -> HsName
unRename name
   = case isRenamed name of
          False -> name
          True  -> case name of
                      UnQual i   -> UnQual   $ unrenameIdent i
                      Qual mod i -> Qual mod $ unrenameIdent i

unrenameIdent :: HsIdentifier -> HsIdentifier
unrenameIdent = hsIdentString_u unRenameString

isRenamed :: HsName -> Bool
isRenamed (UnQual i)    = isIdentRenamed i
isRenamed (Qual _mod i) = isIdentRenamed i

-- an identifier is renamed if it starts with one or more digits
-- such an identifier would normally be illegal in Haskell
isIdentRenamed :: HsIdentifier -> Bool
isIdentRenamed i = not $ null $ takeWhile isDigit $ hsIdentString i




unRenameString :: String -> String
unRenameString s = (dropUnderscore . dropDigits) s where
   dropUnderscore ('_':rest) = rest
   dropUnderscore otherList = otherList
   dropDigits = dropWhile isDigit



--------------------------------------------------------
----This section of code updates the current SubTable to reflect the present scope

instance UpdateTable HsDecl where
    getNames hsDecl = fsts $  getHsNamesAndASrcLocsFromHsDecl hsDecl

instance UpdateTable HsPat where
    getNames hsPat = getNamesFromHsPat hsPat

instance UpdateTable HsStmt where
    getNames hsStmt = fsts $  getHsNamesAndASrcLocsFromHsStmt hsStmt



getHsNamesAndASrcLocsFromHsDecl :: HsDecl -> [(HsName, SrcLoc)]
getHsNamesAndASrcLocsFromHsDecl (HsPatBind srcLoc (HsPVar hsName) _ _) = [(hsName, srcLoc)]
getHsNamesAndASrcLocsFromHsDecl (HsPatBind sloc _ _ _) = error $ "non simple pattern binding found (sloc): " ++ show sloc
getHsNamesAndASrcLocsFromHsDecl (HsFunBind hsMatches) = getHsNamesAndASrcLocsFromHsMatches hsMatches
getHsNamesAndASrcLocsFromHsDecl (HsForeignDecl a _ n _) = [(n,a)]
getHsNamesAndASrcLocsFromHsDecl _otherHsDecl = []

getHsNamesAndASrcLocsFromHsMatches :: [HsMatch] -> [(HsName, SrcLoc)]
getHsNamesAndASrcLocsFromHsMatches [] = []
getHsNamesAndASrcLocsFromHsMatches (hsMatch:_hsMatches) = getHsNamesAndASrcLocsFromHsMatch hsMatch

getHsNamesAndASrcLocsFromHsMatch :: HsMatch -> [(HsName, SrcLoc)]
getHsNamesAndASrcLocsFromHsMatch (HsMatch srcLoc hsName _ _ _) = [(hsName, srcLoc)]


-- | Collect all names defined in a module as well as their declaration points and
-- any subnames they might have.

collectDefsHsModule :: HsModule -> ([(Name,SrcLoc,[Name])],[(Name,Int)])
collectDefsHsModule m = execWriter (mapM_ f (hsModuleDecls m)) where
    --g (b,n,sl,ns) = (b,mod n, sl, map mod ns)
    mod = qualifyName (hsModuleName m)
    toName t n = Name.toName t (mod n)
    -- f :: HsDecl -> Writer [(Name,SrcLoc,[Name])] ()
    tellF xs = tell (xs,[]) >> return ()
    tellS xs = tell ([],xs) >> return ()
    f (HsForeignDecl a _ n _)  = tellF [(toName Val n,a,[])]
    f (HsForeignExport a e _ _)  = tellF [(ffiExportName e,a,[])]
    f (HsFunBind [])  = return ()
    f (HsFunBind (HsMatch a n _ _ _:_))  = tellF [(toName Val n,a,[])]
    f (HsPatBind srcLoc p _ _) = tellF [ (toName Val n,srcLoc,[]) | n <- (getNamesFromHsPat p) ]
    f (HsActionDecl srcLoc p _) = tellF [ (toName Val n,srcLoc,[]) | n <- (getNamesFromHsPat p) ]
    f (HsTypeDecl sl n _ _) = tellF [(toName TypeConstructor n,sl,[])]
    f HsDataDecl { hsDeclSrcLoc =sl, hsDeclName = n, hsDeclCons = cs } = do
        tellF $ (toName TypeConstructor n,sl,snub [ x |(x,_,_) <- cs']): cs' ; zup cs where
            cs' = concatMap (namesHsConDecl' toName) cs
    f (HsNewTypeDecl sl _ n _ c _) = do tellF $ (toName TypeConstructor n,sl,snub [ x |(x,_,_) <- cs']): cs' ; zup [c] where
        cs' = namesHsConDecl' toName c
    f cd@(HsClassDecl sl _ ds) = tellF $ (toName Name.ClassName (nameName z),sl,snub $ fsts cs):[ (n,a,[]) | (n,a) <- cs]  where
        z = case maybeGetDeclName cd of
            Just x | nameType x == ClassName -> x
            _ -> error "not a class name"
        cs = fst (mconcatMap (namesHsDeclTS' toName) ds)
    f cad@(HsClassAliasDecl { hsDeclSrcLoc = sl, hsDeclName = n, hsDeclDecls = ds })
           = tellF $ (toName Name.ClassName n,sl,snub $ fsts cs):[ (n,a,[]) | (n,a) <- cs]
        where
          cs = fst (mconcatMap (namesHsDeclTS' toName) ds)

    f _ = return ()
    zup cs = tellS (map g cs) where
        g ca = (toName DataConstructor (hsConDeclName ca), length $ hsConDeclArgs ca)

namesHsConDecl' toName c = ans where
    dc = (toName DataConstructor $ hsConDeclName c,sl,fls')
    sl = hsConDeclSrcLoc c
    ans = dc : [ (toName Val n,sl,[]) |  n <- fls ]  ++  [ (n,sl,[]) |  n <- fls' ]
    fls' = map (toName FieldLabel) fls
    fls = case c of
        HsRecDecl { hsConDeclRecArg = ra } -> concatMap fst ra -- (map (rtup (hsConDeclSrcLoc c). toName FieldLabel) . fst) ra
        _ -> []

namesHsDeclTS' toName (HsTypeSig sl ns _) = ((map (rtup sl . toName Val) ns),[])
namesHsDeclTS' toName (HsTypeDecl sl n _ _) = ([(toName TypeConstructor n,sl)],[])
namesHsDeclTS' _ _ = ([],[])


namesHsDecl :: HsDecl -> ([(HsName, SrcLoc)],[(HsName, SrcLoc)])
namesHsDecl (HsForeignDecl a _ n _)  = ([(n,a)],[])
namesHsDecl (HsFunBind hsMatches)  = (getHsNamesAndASrcLocsFromHsMatches hsMatches, [])
namesHsDecl (HsPatBind srcLoc p _ _) = (map (rtup srcLoc) (getNamesFromHsPat p),[])
namesHsDecl (HsTypeDecl sl n _ _) = ([],[(n,sl)])
namesHsDecl HsDataDecl { hsDeclSrcLoc = sl, hsDeclName = n, hsDeclCons = cs } = ( (concatMap namesHsConDecl cs) ,[(n,sl)])
namesHsDecl (HsNewTypeDecl sl _ n _ c _) = ( (namesHsConDecl c),[(n,sl)])
namesHsDecl cd@(HsClassDecl sl _ ds) = (mconcatMap namesHsDeclTS ds) `mappend` ([],[(nameName z,sl)]) where
    z = case maybeGetDeclName cd of
        Just x | nameType x == ClassName -> x
        --       | otherwise ->  parseName ClassName (show x ++ show (nameType x))
        _ -> error "really not a class name"
namesHsDecl _ = mempty

namesHsDeclTS (HsTypeSig sl ns _) = ((map (rtup sl) ns),[])
namesHsDeclTS _ = ([],[])

namesHsConDecl c = (hsConDeclName c,hsConDeclSrcLoc c) : case c of
    -- HsRecDecl { hsConDeclRecArg = ra } -> concatMap (map (rtup (hsConDeclSrcLoc c)) . fst) ra
    _ -> []

getHsNamesAndASrcLocsFromHsStmt :: HsStmt -> [(HsName, SrcLoc)]
getHsNamesAndASrcLocsFromHsStmt (HsGenerator srcLoc hsPat _hsExp) = zip (getNamesFromHsPat hsPat) (repeat srcLoc)
getHsNamesAndASrcLocsFromHsStmt (HsQualifier _hsExp) = []
getHsNamesAndASrcLocsFromHsStmt (HsLetStmt hsDecls) = concat $ map getHsNamesAndASrcLocsFromHsDecl hsDecls


instance UpdateTable HsQualType where
    getNames (HsQualType _hsContext hsType) = getNames hsType

instance UpdateTable HsType where
    getNames t = execWriter (getNamesFromType t)  where
        getNamesFromType (HsTyVar hsName) = tell [hsName]
        getNamesFromType t = traverseHsType_ getNamesFromType t


qualifyName :: Module -> HsName -> HsName
qualifyName _ name@(Qual {}) = name
qualifyName mod (UnQual name) = Qual mod name


