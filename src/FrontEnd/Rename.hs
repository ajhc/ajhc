module FrontEnd.Rename(
    renameModule,
    unRename,
    collectDefsHsModule,
    FieldMap(..),
    DeNameable(..),
    renameStatement
    ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.RWS
import Control.Monad.Writer
import Data.Char
import Data.Maybe
import List hiding(union)
import qualified Data.Foldable as Seq
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Doc.DocLike(tupled)
import DerivingDrift.Drift
import FrontEnd.Desugar (desugarHsModule,doToExp,listCompToExp)
import FrontEnd.HsSyn
import FrontEnd.SrcLoc hiding(srcLoc)
import qualified FrontEnd.SrcLoc
import FrontEnd.Syn.Traverse
import FrontEnd.Warning
import Name.Name as Name
import Name.Names
import Options
import Support.FreeVars
import Util.Gen
import Util.Inst()
import Util.SetLike
import qualified FrontEnd.HsErrors as HsErrors
import qualified Name.VConsts as V

data FieldMap = FieldMap
    !(Map.Map Name Int)          -- a map of data constructors to their arities
    !(Map.Map Name [(Name,Int)]) -- a map of field labels to ...

instance Monoid FieldMap where
    mempty = FieldMap mempty mempty
    mappend (FieldMap a b) (FieldMap c d) =
        FieldMap (a `mappend` c) (b `mappend` d)

type SubTable = Map.Map Name Name

newtype ScopeState = ScopeState Int

data Context
    = ContextTopLevel
    | ContextInstance !Name
    | ContextLocal
    deriving(Eq)

data Env = Env {
    envModule      :: Module,
    envNameMap     :: Map.Map Name (Either String Name),
    envOptions     :: Opt,
    envFieldLabels :: FieldMap,
    envSrcLoc      :: SrcLoc
    }

addTopLevels :: HsModule -> RM a -> RM a
addTopLevels  hsmod action = do
    mod <- getCurrentModule
    let cdefs = map (\ (x,y,_) -> (x,y)) $ fst $ collectDefsHsModule hsmod
        nmap = foldl f [] (fsts cdefs)
        f r hsName@(getModule -> Just _)
            | Just _ <- V.fromTupname hsName, toModule "Jhc.Prim.Prim" == mod
                = let nn = hsName in (nn,nn):r
            | nameName tc_Arrow == hsName, toModule "Jhc.Prim.Prim" == mod
                = let nn = hsName in (nn,nn):r
            | otherwise = let nn = toUnqualified hsName in (nn,hsName):(hsName,hsName):r
        f r z = let nn = qualifyName mod z in (z,nn):(nn,nn):r
        z ns = mapM mult (filter (\x -> length x > 1) $
            groupBy (\a b -> fst a == fst b) (sort ns))
        mult xs@(~((n,sl):_)) = warn sl (MultiplyDefined n (snds xs))
            (show n ++ " is defined multiple times: " ++ show xs)
    z cdefs
    let amb k x y | x == y = x
        amb k (Right n1) (Right n2) = Left (ambig k [n1,n2])
        amb _ _ l  = l
    local (\e -> e { envNameMap = Map.unionWithKey amb (Map.map Right $ Map.fromList nmap) (envNameMap e) }) action

createSelectors sloc ds = mapM g ns where
    ds' :: [(Name,[(Name,HsBangType)])]
    ds' = [ (c,[(n,t) | (ns,t) <- rs , n <- ns ]) | HsRecDecl { hsConDeclName = c, hsConDeclRecArg = rs } <- ds ]
    ns = sortGroupUnderF fst $ concatMap f ds' -- [  | (c,nts) <- ds' ]
    f (c,nts) = [ (n,(c,i,length nts)) | (n,_) <- nts | i <- [0..]]
    g (n,cs) = do
        var <- clobberedName (toName Val "_sel")
        let f (_,(c,i,l)) = HsMatch sloc n [pat c i l] (HsUnGuardedRhs (HsVar var)) []
            pat c i l = HsPApp c [ if p == i then HsPVar var else HsPWildCard | p <- [0 .. l - 1]]
            els = HsMatch sloc n [HsPWildCard] (HsUnGuardedRhs HsError { hsExpSrcLoc = sloc, hsExpString = show n, hsExpErrorType = HsErrorFieldSelect } ) []
        return $ HsFunBind (map f cs ++ [els]) where

ambig x ys = "Ambiguous Name: " ++ show x ++ "\nCould refer to: " ++ tupled (map show ys)

runRename :: MonadWarn m => (a -> RM b) -> Opt -> Module -> FieldMap -> [(Name,[Name])] -> a -> m (b,Map.Map Name Name)
runRename doit opt mod fls ns m = mapM_ addWarning errors >> return (renamedMod,reverseMap) where
    nameMap = fromList $ map f ns where
        f (x,[y]) = (x,Right y)
        f (x,ys)  = (x,Left $ ambig x ys)
    startState = ScopeState 1
    startEnv = Env {
        envModule      = mod,
        envNameMap     = nameMap,
        envOptions     = opt,
        envFieldLabels = fls,
        envSrcLoc      = mempty
    }
    (renamedMod, _, (reverseMap,errors)) = runRWS (unRM $ doit m) startEnv startState

{-# NOINLINE renameModule #-}
renameModule :: MonadWarn m => Opt -> FieldMap -> [(Name,[Name])] -> HsModule -> m ((HsModule,[HsDecl]),Map.Map Name Name)
renameModule opt fls ns m = runRename go opt (hsModuleName m) fls (ns ++ driftResolvedNames) m
  where go mod = do
          let renDesugared = renameDecls . desugarHsModule
          rmod <- renDesugared mod
          inst <- hsModuleDecls `fmap` renDesugared mod{hsModuleDecls = driftDerive rmod}
          return (hsModuleDecls_u (++ inst) rmod,inst)


{-# NOINLINE renameStatement #-}
renameStatement :: MonadWarn m => FieldMap -> [(Name,[Name])] ->  Module -> HsStmt -> m HsStmt
renameStatement fls ns modName stmt = fst `liftM` runRename rename options modName fls ns stmt

withSubTable :: SubTable -> RM a -> RM a
withSubTable st action = local (\e -> e { envNameMap = Map.map Right st `union` envNameMap e }) action

renameDecls :: HsModule -> RM HsModule
renameDecls mod = do
    withSrcLoc (hsModuleSrcLoc mod) $ do
    addTopLevels mod $ do
    decls' <- renameHsDecls ContextTopLevel (hsModuleDecls mod)
    mapM_ checkExportSpec $ fromMaybe [] (hsModuleExports mod)
    return mod { hsModuleDecls = decls' }

checkExportSpec :: HsExportSpec -> RM ()
checkExportSpec e = f e  where
    f (HsEVar n) = do check [Val] n
    f (HsEAbs n) = do check [DataConstructor,TypeConstructor,ClassName] n
    f (HsEThingAll n) = do check [DataConstructor,TypeConstructor,ClassName] n
    f (HsEThingWith n ns) = do
        check [DataConstructor,TypeConstructor,ClassName] n
        mapM_ (check [DataConstructor,Val]) ns
    f HsEModuleContents {} = return ()
    check ts n = do
        nm <- asks envNameMap
        let idef = any isJust (map (flip mlookup nm) $ zipWith toName ts (repeat n))
        unless idef $ do
            sl <- getSrcLoc
            warn sl (UndefinedName n) ("unknown name in export list: " ++ show n)

expandTypeSigs :: [HsDecl] -> [HsDecl]
expandTypeSigs ds =  (concatMap f ds) where
    f (HsTypeSig sl ns qt) =  [ HsTypeSig sl [n] qt | n <- ns]
    f d = return d

getTypeClassModule :: HsClassHead -> Maybe Module
getTypeClassModule typ = getModule (hsClassHead typ)

qualifyMethodName :: Module -> Name -> Name
qualifyMethodName mod name = quoteName . toName Val $ qualifyName mod name

qualifyInstMethod :: Maybe Module -> HsDecl -> RM HsDecl
qualifyInstMethod Nothing decl = rename decl
qualifyInstMethod (Just moduleName) decl = case decl of
    HsPatBind srcLoc HsPVar {hsPatName = name} rhs decls ->
        rename $ HsPatBind srcLoc (HsPVar {hsPatName = qualifyMethodName moduleName name}) rhs decls
    HsFunBind matches -> rename $ HsFunBind (map f matches) where
        f m@HsMatch { hsMatchName } = m { hsMatchName = qualifyMethodName moduleName hsMatchName }
    _ -> rename decl

renameHsDecls :: Context -> [HsDecl] -> RM [HsDecl]
renameHsDecls c ds = f ds where
    f (d:ds) = do
        d' <- rename d
        when (c == ContextTopLevel) $ HsErrors.hsDeclTopLevel d'
        eds <- g d'
        ds' <- f ds
        return $ d':eds ++ ds'
    f [] = return []
    g HsDataDecl { hsDeclSrcLoc = sloc, hsDeclCons = cs } = createSelectors sloc cs
    g HsNewTypeDecl { hsDeclSrcLoc = sloc, hsDeclCon = c } = createSelectors sloc [c]
    g _ = return []

instance Rename HsDecl where
    rename d = withSrcLoc (FrontEnd.SrcLoc.srcLoc d) $ renameHsDecl d

renameHsDecl d = f d where
    f (HsPatBind srcLoc hsPat hsRhs {-where-} hsDecls) = do
        hsPat'    <- rename hsPat
        updateWithN Val hsDecls $ do
        hsDecls'  <- rename hsDecls
        hsRhs'    <- rename hsRhs
        return (HsPatBind srcLoc hsPat' hsRhs' {-where-} hsDecls')
    f (HsForeignExport a b n t) = do
        n <- renameValName n
        updateWith t $ do
            t <- rename t
            return (HsForeignExport a b n t)
    f (HsForeignDecl a b n t) = do
        n <- renameValName n
        updateWith t $ do
        t <- rename t
        return (HsForeignDecl a b n t)
    f (HsFunBind hsMatches) = do
        hsMatches' <- rename hsMatches
        return (HsFunBind hsMatches')
    f (HsTypeSig srcLoc hsNames hsQualType) = do
        hsNames' <- mapM renameValName hsNames
        updateWith hsQualType $ do
            hsQualType' <- rename hsQualType
            return (HsTypeSig srcLoc hsNames' hsQualType')
    f HsDataDecl { .. } | hsDeclKindDecl = do
        hsDeclName <- renameKindName hsDeclName
        unless (null hsDeclArgs) $
            addWarn InvalidDecl "kind declarations can't have arguments."
        when (any isHsRecDecl hsDeclCons) $
            addWarn InvalidDecl "kind declarations can't have records."
        hsDeclCons <- mapM renameKindHsCon hsDeclCons
        unless (null hsDeclDerives) $
            addWarn InvalidDecl "kind declarations can't derive classes"
        unless (null hsDeclContext) $
            addWarn InvalidDecl "kind declarations can't have context"
        return HsDataDecl { .. }
    f HsDataDecl { .. } = do
        hsDeclName <- renameTypeName hsDeclName
        updateWith (map fromTypishHsName hsDeclArgs) $ do
            hsDeclContext <- rename hsDeclContext
            hsDeclArgs <- mapM renameTypeName hsDeclArgs
            hsDeclCons <- rename hsDeclCons
            hsDeclDerives <- mapM (renameName . toName ClassName) hsDeclDerives
            return HsDataDecl { .. }
    f (HsTypeDecl srcLoc name hsNames t) = do
        hsName' <- renameTypeName name
        updateWith (Set.toList $ freeVars hsNames :: [Name]) $ do
            hsNames' <- rename hsNames
            t' <- rename t
            return (HsTypeDecl srcLoc  hsName' hsNames' t')
    f HsTypeFamilyDecl { .. } = do
        hsDeclCName <- renameTypeName hsDeclName
        updateWith (Set.toList $ freeVars hsDeclTArgs :: [Name]) $ do
            hsDeclTArgs <- rename hsDeclTArgs
            return HsTypeFamilyDecl { .. }
    f (HsNewTypeDecl srcLoc hsContext hsName hsNames1 hsConDecl hsNames2) = do
        hsName' <- renameTypeName hsName
        updateWith (map fromTypishHsName hsNames1) $ do
            hsContext' <- rename hsContext
            hsNames1' <- mapM renameTypeName hsNames1 -- TODO
            hsConDecl' <- rename hsConDecl
            hsNames2' <- mapM (renameName . toName ClassName) hsNames2
            return (HsNewTypeDecl srcLoc hsContext' hsName' hsNames1' hsConDecl' hsNames2')
    f (HsClassDecl srcLoc classHead hsDecls) = do
        classHead' <- updateWithN TypeVal (hsClassHeadArgs classHead) $ rename classHead
        hsDecls' <- rename hsDecls
        return (HsClassDecl srcLoc classHead' hsDecls')
    f (HsClassAliasDecl srcLoc name args hsContext hsClasses hsDecls) = do
        name' <- renameTypeName name
        updateWith args $ do
        args' <- mapM rename args
        hsContext' <- rename hsContext
        hsClasses' <- rename hsClasses
        hsDecls' <- rename hsDecls
        return (HsClassAliasDecl srcLoc name' args' hsContext' hsClasses' hsDecls')
    f (HsInstDecl srcLoc classHead hsDecls) = do
        updateWithN TypeVal (hsClassHeadArgs classHead) $ do
        classHead' <- rename classHead
        hsDecls' <- mapM (qualifyInstMethod (getTypeClassModule classHead')) hsDecls
        return (HsInstDecl srcLoc classHead' hsDecls')
    f (HsInfixDecl srcLoc assoc int hsNames) = do
        hsNames' <- mapM renameValName hsNames
        return $ HsInfixDecl srcLoc assoc int hsNames'
    f (HsActionDecl srcLoc pat e) = do
        pat <- rename pat
        e <- rename e
        return (HsActionDecl srcLoc pat e)
    f (HsPragmaProps srcLoc prop hsNames) = do
        hsNames' <- mapM renameValName hsNames
        return (HsPragmaProps  srcLoc prop hsNames')
    f (HsPragmaRules rs) = do
        rs' <- rename rs
        return $ HsPragmaRules rs'
    f prules@HsPragmaSpecialize { hsDeclSrcLoc = srcLoc, hsDeclName = n, hsDeclType = t } = do
        n <- if n == nameName u_instance then return n else renameValName n
        let ns = snub (getNames t)
        updateWith t $ do
            ns' <- mapM renameTypeName ns
            t <- rename t
            m <- getCurrentModule
            i <- newUniq
            let _nt = if null ns' then t else HsTyForall bs (HsQualType [] t)
                bs = [ hsTyVarBind { hsTyVarBindName = n } | n <- ns']
            return prules { hsDeclUniq = (m,i), hsDeclName = n, hsDeclType = t }
    f (HsDefaultDecl sl e) = HsDefaultDecl sl <$> rename e
    f (HsDeclDeriving sl ch) = HsDeclDeriving sl <$> rename ch
    f h = error $ "renameerr: " ++ show h

instance Rename HsClassHead where
    rename (HsClassHead cx n ts) = do
        updateWith ts $ HsClassHead <$> rename cx <*> renameName (toName ClassName n) <*> rename ts

instance Rename HsRule where
    rename prules@HsRule { hsRuleSrcLoc = srcLoc, hsRuleFreeVars = fvs, hsRuleLeftExpr = e1, hsRuleRightExpr = e2 } = do
        withSrcLoc srcLoc $ do
        updateWith (map fromValishHsName $ fsts fvs) $ do
        subTable'' <- getUpdates (catMaybes $ snds fvs)
        fvs' <- sequence [ liftM2 (,) (renameValName x) (withSubTable subTable'' $ rename y)| (x,y) <- fvs]
        e1' <- rename e1
        e2' <- rename e2
        m <- getCurrentModule
        i <- newUniq
        return prules {  hsRuleUniq = (m,i), hsRuleFreeVars = fvs', hsRuleLeftExpr = e1', hsRuleRightExpr = e2' }

instance Rename HsQualType where
    rename (HsQualType hsContext hsType) =
        HsQualType <$> rename hsContext <*> rename hsType

instance Rename HsAsst where
    rename (HsAsst hsName1 hsName2s) = do
        hsName1' <- renameName (toName ClassName hsName1)
        hsName2s' <- mapM renameTypeName hsName2s
        return (HsAsst hsName1' hsName2s')
    rename (HsAsstEq t1 t2) = HsAsstEq <$> rename t1 <*> rename t2

instance Rename HsConDecl where
    --rename cd@(HsConDecl { hsConDeclSrcLoc = srcLoc, hsConDeclName = hsName, hsConDeclConArg = hsBangTypes }) = do
    rename cd@(HsConDecl {  hsConDeclName = hsName, hsConDeclConArg = hsBangTypes, .. }) = do
        withSrcLoc hsConDeclSrcLoc $ do
        hsName' <- renameValName hsName
        updateWith  (map (toName TypeVal . hsTyVarBindName) hsConDeclExists) $ do
        hsConDeclExists <- rename hsConDeclExists
        hsBangTypes' <- rename hsBangTypes
        return cd { hsConDeclName = hsName', hsConDeclConArg = hsBangTypes', hsConDeclExists }
    rename cd@HsRecDecl { hsConDeclSrcLoc = srcLoc, hsConDeclName = hsName, hsConDeclRecArg = stuff} = do
        withSrcLoc srcLoc $ do
        hsName' <- renameValName hsName
        updateWith (map (toName TypeVal . hsTyVarBindName) (hsConDeclExists cd)) $ do
        es <- rename (hsConDeclExists cd)
        stuff' <- sequence [ do ns' <- mapM renameName (map (toName FieldLabel) ns); t' <- rename t; return (ns',t')  |  (ns,t) <- stuff]
        return cd { hsConDeclName = hsName', hsConDeclRecArg = stuff', hsConDeclExists = es }

renameKindHsCon HsConDecl { .. } = do
    withSrcLoc hsConDeclSrcLoc $ do
    hsConDeclName <- renameTypeName hsConDeclName
    unless (null hsConDeclExists) $
        addWarn InvalidDecl "kind declarations cannot have existential types"
    let bt e@HsBangedTy {} = do
            addWarn InvalidDecl "strictness annotations not relevant to kind declarations"
            return e
        bt (HsUnBangedTy e) = HsUnBangedTy `liftM` f e
        f (HsTyCon n) = HsTyCon `liftM` renameKindName n
        f e = addWarn InvalidDecl "invalid argument in kind declaration" >> return e
    hsConDeclConArg <- mapM bt hsConDeclConArg
    return HsConDecl { .. }

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
    rt v@HsTyVar {} = return v
    rt (HsTyCon hsName) = do
        hsName' <- renameTypeName hsName
        return (HsTyCon hsName')
    rt (HsTyForall ts v) = do
        updateWith (map (toName TypeVal) $ map hsTyVarBindName ts)  $ do
        ts' <- rename ts
        v' <- rename v
        return $ HsTyForall ts' v'
    rt (HsTyExists ts v) = do
        updateWith (map (toName TypeVal) $ map hsTyVarBindName ts) $ do
        ts' <- rename ts
        v' <- rename v
        return $ HsTyExists ts' v'
    rt ty = traverseHsType (renameHsType' dovar) ty
    pp t | not dovar = t
    pp t = t

class Rename a where
    rename :: a -> RM a
    rename x = return x

instance Rename x => Rename (Located x) where
    rename (Located sl x) = Located sl `fmap` rename x

instance Rename SrcLoc where

instance Rename a => Rename [a] where
    rename xs = mapM rename xs

instance (Rename a,Rename b) => Rename (a,b) where
    rename (a,b) = (,) <$> rename a <*> rename b

instance Rename a => Rename (Maybe a) where
    rename Nothing = return Nothing
    rename (Just x) = Just <$> rename x

instance Rename HsTyVarBind where
    rename tvb@HsTyVarBind { hsTyVarBindName = n } = do
        n' <- renameTypeName n
        return tvb { hsTyVarBindName = n' }

-- note that for renameHsMatch, the 'wheres' dominate the 'pats'

instance Rename HsMatch where
    rename (HsMatch srcLoc hsName hsPats hsRhs {-where-} hsDecls) = do
        withSrcLoc srcLoc $ do
        hsName' <- renameValName hsName
        updateWithN Val hsPats  $ do
        hsPats' <- rename hsPats
        updateWithN Val hsDecls $ do
        hsDecls' <- rename (expandTypeSigs hsDecls)
        mapM_ HsErrors.hsDeclLocal hsDecls'
        hsRhs' <- rename hsRhs
        return (HsMatch srcLoc hsName' hsPats' hsRhs' {-where-} hsDecls')

instance Rename HsPat where
    rename (HsPVar hsName) = HsPVar `fmap` renameValName hsName
    rename (HsPInfixApp hsPat1 hsName hsPat2) = HsPInfixApp <$> rename hsPat1 <*> renameValName hsName <*> rename hsPat2
    rename (HsPApp hsName hsPats) = HsPApp <$> renameValName hsName <*> rename hsPats
    rename (HsPRec hsName hsPatFields) = do
        hsName' <- renameValName hsName
        hsPatFields' <- rename hsPatFields
        fls <- asks envFieldLabels
        buildRecPat fls hsName' hsPatFields'
    rename (HsPAsPat hsName hsPat) = HsPAsPat <$> renameValName hsName <*> rename hsPat
    rename (HsPTypeSig sl hsPat qt)  = HsPTypeSig sl <$> rename hsPat <*> rename qt
    rename p = traverseHsPat rename p

buildRecPat :: FieldMap -> Name -> [HsPatField] -> RM HsPat
buildRecPat (FieldMap amp fls) n us = case mlookup (toName DataConstructor n) amp of
    Nothing -> failRename $ "Unknown Constructor: " ++ show n
    Just t -> do
        let f (HsPFieldPat x p) = case  mlookup (toName FieldLabel x) fls of
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
        --gt <- gets globalSubTable      -- field names are not shadowed by local definitions.
        hsName' <- renameName (toName FieldLabel hsName) --renameName hsName gt
        hsPat' <- rename hsPat
        return (HsPFieldPat hsName' hsPat')

instance Rename HsRhs where
    rename (HsUnGuardedRhs hsExp) = HsUnGuardedRhs <$> rename hsExp
    rename (HsGuardedRhss rs) = HsGuardedRhss <$> rename rs

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
    --let hsName'' = (Qual mod (HsIdent $ show unique {- ++ fromName hsName' -} ++ "_var@"))
    let hsName'' = toName Val (mod,show unique ++ "_var@")
    return hsName''

instance Rename HsExp where
    rename (HsVar hsName) = HsVar <$> renameValName hsName
    rename (HsCon hsName) = HsCon <$> renameValName hsName
    rename i@(HsLit HsInt {}) = do return i
    rename i@(HsLit HsFrac {}) = do
        z <- rename f_fromRational
        return $ HsParen (HsApp z i)
    rename (HsLambda srcLoc hsPats hsExp) = do
        withSrcLoc srcLoc $ do
        updateWithN Val hsPats $ do
        hsPats' <- rename hsPats
        hsExp' <- rename hsExp
        return (HsLambda srcLoc hsPats' hsExp')
    rename (HsLet hsDecls hsExp) = do
        updateWithN Val hsDecls $ do
        hsDecls' <- rename (expandTypeSigs hsDecls)
        mapM_ HsErrors.hsDeclLocal hsDecls'
        hsExp' <- rename hsExp
        return (HsLet hsDecls' hsExp')
    rename (HsCase hsExp hsAlts) = do HsCase <$> rename hsExp <*> rename hsAlts
    rename (HsDo hsStmts) = do
        (ss,()) <- renameHsStmts hsStmts (return ())
        doToExp newVar (nameName v_bind) (nameName v_bind_) (nameName v_fail) ss
    rename (HsRecConstr hsName hsFieldUpdates) = do
        hsName' <- renameValName hsName
        hsFieldUpdates' <- rename hsFieldUpdates
        fls <- asks envFieldLabels
        buildRecConstr fls hsName' (hsFieldUpdates'::[HsFieldUpdate])
    rename (HsRecUpdate hsExp hsFieldUpdates) = do
        hsExp' <- rename hsExp
        hsFieldUpdates' <- rename hsFieldUpdates
        fls <- asks envFieldLabels
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
    rename (HsAsPat hsName hsExp) = HsAsPat <$> renameValName hsName <*> rename hsExp
    rename (HsWildCard sl) = do
        withSrcLoc sl $ do
            e <- createError HsErrorUnderscore ("_")
            return e
    rename p = traverseHsExp rename p

desugarEnum s as = foldl HsApp (HsVar (toName Val s)) as

createError et s = do
    sl <- getSrcLoc
    return $ HsError { hsExpSrcLoc = sl, hsExpErrorType = et, hsExpString = (show sl ++ ": " ++ s) }

failRename s = do
    sl <- getSrcLoc
    fail (show sl ++ ": " ++ s)

buildRecConstr ::  FieldMap -> Name -> [HsFieldUpdate] -> RM HsExp
buildRecConstr (FieldMap amp fls) n us = do
    undef <- createError HsErrorUninitializedField "Uninitialized Field"
    case mlookup (toName DataConstructor n) amp of
        Nothing -> failRename $ "Unknown Constructor: " ++ show n
        Just t -> do
            let f (HsFieldUpdate x e) = case  mlookup (toName FieldLabel x) fls of
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
buildRecUpdate (FieldMap amp fls) n us = do
        sl <- getSrcLoc
        let f (HsFieldUpdate x e) = case  mlookup (toName FieldLabel x) fls of
                Nothing -> failRename $ "Field Label does not exist: " ++ show x
                Just cs -> return [ (x,(y,hsParen e)) | (x,y) <- cs ]
        fm <- liftM concat $ mapM f us
        let fm' = sortGroupUnderFG fst snd fm
        let g (c,zs) = case mlookup c amp of
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
        updateWithN Val hsPat $ do
        hsPat' <- rename hsPat
        updateWithN Val hsDecls $ do
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
--        gt <- gets globalSubTable              -- field names are global and not shadowed
 --       hsName' <- renameName hsName gt      -- TODO field names should have own namespace
        hsName' <- renameName (toName FieldLabel hsName)      -- TODO field names should have own namespace
        hsExp' <- rename hsExp
        return (HsFieldUpdate hsName' hsExp')

renameValName :: Name -> RM Name
renameValName hsName = renameName (fromValishHsName hsName)

renameTypeName :: Name -> RM Name
renameTypeName hsName = renameName (fromTypishHsName hsName)

renameKindName :: Name -> RM Name
renameKindName hsName = renameName (toName SortName hsName)

renameName :: Name -> RM Name
-- a few hard coded cases
renameName hsName
    | Just n <- fromQuotedName hsName = return n
    | hsName `elem` [tc_Arrow,dc_Unit,tc_Unit] = return hsName
    | (nt,Just m,i) <- nameParts hsName, '@':_ <- show m = return $ toName nt (m, i)
    | Just _ <- V.fromTupname hsName = return hsName
renameName hsName = do
    subTable <- asks envNameMap
    case mlookup hsName subTable of
        Just (Right name) -> do
            tell (Map.singleton name hsName,mempty)
            return name
        Just (Left err) -> do
            addWarn (UndefinedName hsName) err
            return hsName
        Nothing -> do
            let err = "Unknown name: " ++ show hsName
            addWarn (UndefinedName hsName) err
            return hsName

clobberedName :: Name -> RM Name
clobberedName hsName = do
    unique     <- newUniq
    currModule <- getCurrentModule
    return $ renameAndQualify hsName unique currModule

clobberName :: Name -> RM SubTable
clobberName hsName = do
    hsName' <- clobberedName hsName
    return $ msingleton hsName hsName'

renameAndQualify :: Name -> Int -> Module -> Name
renameAndQualify name unique currentMod = qualifyName currentMod (renameName name unique) where
    renameName n unique = mapName (id,((show unique ++ "_") ++)) n

-- | unRename gets the original identifier name from the renamed version
unRename :: Name -> Name
unRename name = mapName (id,unRenameString) name

unRenameString :: String -> String
unRenameString s@((isDigit -> False):_) = s
unRenameString s = (dropUnderscore . dropDigits) s where
    dropUnderscore ('_':rest) = rest
    dropUnderscore otherList = otherList
    dropDigits = dropWhile isDigit

updateWithN nt x action = getUpdatesN nt x >>= flip withSubTable action
getUpdatesN nt x = unions `fmap` mapM clobberName (map (toName nt) $ getNames x)

updateWith x action = getUpdates x >>= flip withSubTable action
getUpdates x = unions `fmap` mapM clobberName (getNames x)

class UpdateTable a where
    getNames :: a -> [Name]
    getNames a = []

instance UpdateTable a => UpdateTable [a] where
    getNames xs = concatMap getNames xs
instance (UpdateTable a, UpdateTable b) => UpdateTable (a,b) where
    getNames (a,b) = getNames a ++ getNames b

instance UpdateTable Name where
    getNames x | nameType x == QuotedName = []
               | otherwise = [x]

instance UpdateTable HsDecl where
    getNames hsDecl = fsts $ getNamesAndASrcLocsFromHsDecl hsDecl
instance UpdateTable HsPat where
    getNames hsPat = getNamesFromHsPat hsPat
instance UpdateTable HsStmt where
    getNames hsStmt = fsts $ getNamesAndASrcLocsFromHsStmt hsStmt
instance UpdateTable HsQualType where
    getNames (HsQualType _hsContext hsType) = getNames hsType
instance UpdateTable HsType where
    getNames t = execWriter (getNamesFromType t)  where
        getNamesFromType (HsTyVar hsName) = tell [fromTypishHsName hsName]
        getNamesFromType t = traverseHsType_ getNamesFromType t

getNamesAndASrcLocsFromHsDecl :: HsDecl -> [(Name, SrcLoc)]
getNamesAndASrcLocsFromHsDecl d = f d where
    f (HsPatBind srcLoc (HsPVar hsName) _ _) = [(fromValishHsName hsName, srcLoc)]
    f (HsPatBind sloc _ _ _) = error $ "non simple pattern binding found (sloc): " ++ show sloc
    f (HsFunBind (HsMatch { .. }:_)) = [(fromValishHsName hsMatchName,hsMatchSrcLoc)]
    f (HsForeignDecl { .. }) = [(fromValishHsName hsDeclName, hsDeclSrcLoc)]
    f _ = []

-- | Collect all names defined in a module as well as their declaration points
-- and any subnames they might have. In addition, collect the arities of any
-- constructors.

collectDefsHsModule :: HsModule -> ([(Name,SrcLoc,[Name])],[(Name,Int)])
collectDefsHsModule m = (\ (x,y) -> (Seq.toList x,Seq.toList y)) $ execWriter (mapM_ f (hsModuleDecls m)) where
    toName t n = Name.toName t (qualifyName (hsModuleName m) n)
    tellName sl n = tellF [(n,sl,[])]
    tellF xs = tell (Seq.fromList xs,Seq.empty) >> return ()
    tellS xs = tell (Seq.empty,Seq.fromList xs) >> return ()
    f (HsForeignDecl a _ n _)    = tellName a (toName Val n)
    f (HsForeignExport a e _ _)  = tellName a (ffiExportName e)
    f (HsFunBind [])  = return ()
    f (HsFunBind (HsMatch a n _ _ _:_))  = tellName a (toName Val n)
    f (HsPatBind srcLoc p _ _)  = mapM_ (tellName srcLoc) [ (toName Val n) | n <- (getNamesFromHsPat p) ]
    f (HsActionDecl srcLoc p _) = mapM_ (tellName srcLoc) [ (toName Val n) | n <- (getNamesFromHsPat p) ]
    f (HsTypeDecl sl n _ _) = tellName sl (toName TypeConstructor n)
    f HsDataDecl { hsDeclKindDecl = True, hsDeclSrcLoc =sl, hsDeclName = n, hsDeclCons = cs } = do
        tellF $ (toName SortName n,sl,snub [ x |(x,_,_) <- cs']): cs' ; zup cs where
            cs' = concatMap (namesHsConDeclSort' toName) cs
    f HsDataDecl { hsDeclSrcLoc =sl, hsDeclName = n, hsDeclCons = cs } = do
        tellF $ (toName TypeConstructor n,sl,snub [ x |(x,_,_) <- cs']): cs' ; zup cs where
            cs' = concatMap (namesHsConDecl' toName) cs
    f (HsNewTypeDecl sl _ n _ c _) = do tellF $ (toName TypeConstructor n,sl,snub [ x |(x,_,_) <- cs']): cs' ; zup [c] where
        cs' = namesHsConDecl' toName c
    f cd@(HsClassDecl sl ch ds) = tellF $ (toName ClassName $ hsClassHead ch,sl,snub $ fsts cs):[ (n,a,[]) | (n,a) <- cs]  where
        cs = (mconcatMap (namesHsDeclTS' toName) ds)
    f cad@(HsClassAliasDecl { hsDeclSrcLoc = sl, hsDeclName = n, hsDeclDecls = ds })
           = tellF $ (toName Name.ClassName n,sl,snub $ fsts cs):[ (n,a,[]) | (n,a) <- cs]
        where
          cs = (mconcatMap (namesHsDeclTS' toName) ds)

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

    namesHsConDeclSort' toName c = [dc] where
        dc = (toName TypeConstructor $ hsConDeclName c,sl,[])
        sl = hsConDeclSrcLoc c

    namesHsDeclTS' toName (HsTypeSig sl ns _) = (map ((,sl) . toName Val) ns)
    namesHsDeclTS' toName (HsTypeDecl sl n _ _) = [(toName TypeConstructor n,sl)]
    namesHsDeclTS' _ _ = []

getNamesAndASrcLocsFromHsStmt :: HsStmt -> [(Name, SrcLoc)]
getNamesAndASrcLocsFromHsStmt (HsGenerator srcLoc hsPat _hsExp) = zip (getNamesFromHsPat hsPat) (repeat srcLoc)
getNamesAndASrcLocsFromHsStmt (HsQualifier _hsExp) = []
getNamesAndASrcLocsFromHsStmt (HsLetStmt hsDecls) = concat $ map getNamesAndASrcLocsFromHsDecl hsDecls

-----------
-- RM Monad
-----------

newtype RM a = RM (RWS Env (Map.Map Name Name,[Warning]) ScopeState a)
    deriving(Monad,Functor,MonadReader Env, MonadWriter (Map.Map Name Name,[Warning]), MonadState ScopeState)

unRM (RM x) = x

instance Applicative RM where
    pure = return
    (<*>) = ap

instance MonadWarn RM where
    addWarning w = tell (mempty,[w])

instance UniqueProducer RM where
    newUniq = do
        ScopeState u <- get
        modify (\(ScopeState s) -> ScopeState (1 + s))
        return u

getCurrentModule :: RM Module
getCurrentModule = asks envModule

instance MonadSrcLoc RM where
    getSrcLoc = asks envSrcLoc
instance MonadSetSrcLoc RM where
    withSrcLoc sl a = local (\s -> s { envSrcLoc = sl `mappend` envSrcLoc s}) a
instance OptionMonad RM where
    getOptions = asks envOptions

class DeNameable a where
    deName :: Module -> a -> a

instance (Functor f,DeNameable a) => DeNameable (f a) where
    deName m fx = fmap (deName m) fx

instance DeNameable Name where
    deName mod name = mapName' fm unRenameString name where
        fm (Just m) | m == mod = Nothing
                    | m `elem` removedMods = Nothing
        fm m = m
        removedMods = map toModule [
            "Prelude","Jhc.Basics","Jhc.Prim.IO","Jhc.Type.Word","Jhc.Type.Basic"]

instance DeNameable HsPat where
    deName mod p = f p where
        f (HsPVar v) = HsPVar (deName mod v)
        f (HsPNeg p) = HsPNeg (f p)
        f (HsPIrrPat p) = HsPIrrPat (deName mod p)
        f (HsPBangPat p) = HsPBangPat (deName mod p)
        f (HsPParen p) = HsPParen (f p)
        f (HsPApp cn pats) = HsPApp (deName mod cn) (deName mod pats)
        f (HsPList pats) = HsPList (deName mod pats)
        f (HsPAsPat n p) = HsPAsPat (deName mod n) (deName mod p)
        f p = p

--instance DeNameable n => DeNameable Located l n where
--    deName mod p

instance DeNameable HsAlt where
    deName _ n = n

instance DeNameable HsExp where
    deName mod e = f e where
        dn :: DeNameable b => b -> b
        dn n = deName mod n
        f (HsVar hsName) = HsVar (dn hsName)
        f (HsCon hsName) = HsCon (dn hsName)
        f (HsLambda srcLoc hsPats hsExp) =
            HsLambda srcLoc (dn hsPats) (dn hsExp)
        f (HsCase hsExp hsAlts) =
            HsCase (dn hsExp) (dn hsAlts)
        f p = runIdentity $ traverseHsExp (return . dn) p
--        f (HsDo hsStmts) = do
--        (ss,()) <- renameHsStmts hsStmts (return ())
--        doToExp newVar (nameName v_bind) (nameName v_bind_) (nameName v_fail) ss
--    rename (HsRecConstr hsName hsFieldUpdates) = do
--        hsName' <- renameValName hsName
--        hsFieldUpdates' <- rename hsFieldUpdates
--        fls <- asks envFieldLabels
--        buildRecConstr fls hsName' (hsFieldUpdates'::[HsFieldUpdate])
--    rename (HsRecUpdate hsExp hsFieldUpdates) = do
--        hsExp' <- rename hsExp
--        hsFieldUpdates' <- rename hsFieldUpdates
--        fls <- asks envFieldLabels
--        buildRecUpdate fls hsExp' hsFieldUpdates' -- HsRecConstr hsName' hsFieldUpdates')
        --return (HsRecUpdate hsExp' hsFieldUpdates')
--    rename (HsListComp hsExp hsStmts) = do
--        (ss,e) <- renameHsStmts hsStmts (rename hsExp)
--        listCompToExp newVar e ss
--    rename (HsExpTypeSig srcLoc hsExp hsQualType) = do
--        hsExp' <- rename hsExp
--        updateWith hsQualType $ do
--            hsQualType' <- rename hsQualType
--            return (HsExpTypeSig srcLoc hsExp' hsQualType')
--    rename (HsAsPat hsName hsExp) = HsAsPat <$> renameValName hsName <*> rename hsExp
--    rename (HsWildCard sl) = do
--        withSrcLoc sl $ do
--            e <- createError HsErrorUnderscore ("_")
--            return e
