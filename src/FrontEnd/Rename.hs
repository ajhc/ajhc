module FrontEnd.Rename(
    renameModule,
    unRename,
    collectDefsHsModule,
    FieldMap(..),
    DeNameable(..),
    renameStatement
    ) where

import Control.Monad.RWS
import Control.Monad.Writer
import Data.Char
import Util.Std hiding(union)
import qualified Data.Foldable as Seq
import qualified Data.Traversable as T
import qualified Data.List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Data.Word
import DerivingDrift.Drift
import Doc.DocLike(tupled)
import FrontEnd.Desugar (desugarHsModule)
import FrontEnd.HsSyn
import FrontEnd.SrcLoc hiding(srcLoc)
import FrontEnd.Syn.Traverse
import FrontEnd.Warning
import Name.Name as Name
import Name.Names
import Options
import Support.FreeVars
import Util.Gen
import Util.Inst()
import Util.SetLike
import qualified FlagOpts as FO
import qualified FrontEnd.HsErrors as HsErrors
import qualified FrontEnd.SrcLoc
import qualified Name.VConsts as V

{-

Special renaming rules.

When it maps to a 'Prelude.*' name, it means it will map to whatever version
the Prelude in scope exports.

For other 'Jhc.*.* names, it refers to the _exact_ version defined in jhc-prim.

module JPP = Jhc.Prim.Prim

name         prelude           -fno-prelude

names introduced by desugaring:
do:>>=           Prelude.>>=            (>>=)
do:>>            Prelude.>>             (>>)
do:fail          Prelude.fail           fail
4:fromInteger    Prelude.fromInteger    fromInteger
4:fromInt        Prelude.fromInt        fromInt
4.4:fromRational Prelude.fromRational   fromRational
pattern:(==)     Prelude.==             (==)
unary:-          Prelude.negate         negate
n+k:>=           Prelude.>=             >=
n+k:-            Prelude.-              (-)
[..]             Prelude.enumFrom..     (enumFrom..)
(x,y,..)         JPP.(,*) x y           JPP.(,*) x y

terms:
[]           JPP.[]       JPP.[]
()           JPP.()       JPP.()
(,*)         JPP.(,*)     JPP.(,*)
[x,y,..]     x JPP.: ..   x JPP.: ..
(:)          JPP.:        JPP.:

types:
[]           JPP.[]    JPP.[]
()           JPP.()    JPP.()
(,*)         JPP.(,*)  JPP.(,*)
(->)         JPP.->    JPP.->
if:Bool      JPP.Bool  JPP.Bool
[..]         JPP.[]    JPP.[]

other:
(#.. #)      always special.
list comprehensions always turn into JPP versions

-}

data FieldMap = FieldMap
    !(Map.Map Name Word8)          -- a map of data constructors to their arities
    !(Map.Map Name [(Name,Word8)]) -- a map of field labels to constructors and positions

instance Monoid FieldMap where
    mempty = FieldMap mempty mempty
    FieldMap a b `mappend` FieldMap c d =
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
    envNameMap     :: Map.Map Name NameEntry,
    envOptions     :: Opt,
    envFieldLabels :: FieldMap,
    envSrcLoc      :: SrcLoc,
    envInPattern   :: Bool
    }

envInPattern_s p e = e { envInPattern = p }

addTopLevels :: HsModule -> RM a -> RM a
addTopLevels  hsmod action = do
    mod <- getCurrentModule
    let cdefs = map (\ (x,y,_) -> (x,y)) $ fst $ collectDefsHsModule hsmod
        nmap = foldl f [] (fsts cdefs)
        f r hsName@(getModule -> Just _)
            | Just _ <- V.fromTupname hsName, mod_JhcPrimPrim == mod = (hsName,hsName):r
            | tc_Arrow == hsName, mod_JhcPrimPrim == mod = (hsName,hsName):r
            | otherwise = let nn = toUnqualified hsName in (nn,hsName):(hsName,hsName):r
        f r z = let nn = qualifyName mod z in (z,nn):(nn,nn):r
        z ns = mapM mult (filter (\x -> length x > 1) $
            groupBy (\a b -> fst a == fst b) (sort ns))
        mult xs@(~((n,sl):_)) = warn sl (MultiplyDefined n (snds xs))
            (show n ++ " is defined multiple times: " ++ show xs)
    z cdefs
    let amb k x y = if x < y then f x y else f y x where
            f x y | x == y = y
            f (NameJust n1) (NameJust n2) | getModule n1 == Just mod = NameBound n1 [n2]
            f (NameJust n1) (NameJust n2) | getModule n2 == Just mod = NameBound n2 [n1]
            f (NameJust n1) (NameBound x xs) | n1  `elem` (x:xs)  = NameBound x xs
            f (NameBound x xs) (NameBound y ys) | x == y = NameBound x (xs `Data.List.union` ys)
                                                    | otherwise = NameAmbig ((x:xs) `Data.List.union` (y:ys))
            f (names -> xs) (names -> ys) = NameAmbig (Data.List.union xs ys)
        names (NameJust n1) = [n1]
        names (NameBound x xs) = x:xs
        names (NameAmbig xs) = xs
    local (\e -> e { envNameMap = Map.unionWithKey amb (Map.map NameJust $ Map.fromList nmap) (envNameMap e) }) action

createSelectors sloc ds = mapM g ns where
    ds' :: [(Name,[(Name,HsBangType)])]
    ds' = [ (c,[(toName Val n,t) | (ns,t) <- rs , n <- ns ]) | HsRecDecl { hsConDeclName = c, hsConDeclRecArg = rs } <- ds ]
    ns = sortGroupUnderF fst $ concatMap f ds' -- [  | (c,nts) <- ds' ]
    f (c,nts) = [ (n,(c,i,length nts)) | (n,_) <- nts | i <- [0..]]
    g (n,cs) = do
        var <- clobberedName (mkName termLevel False Nothing "_sel")
        let f (_,(c,i,l)) = HsMatch sloc n [pat c i l] (HsUnGuardedRhs (HsVar var)) []
            pat c i l = HsPApp c [ if p == i then HsPVar var else HsPWildCard | p <- [0 .. l - 1]]
            els = HsMatch sloc n [HsPWildCard] (HsUnGuardedRhs HsError { hsExpSrcLoc = sloc, hsExpString = show n, hsExpErrorType = HsErrorFieldSelect } ) []
        return $ HsFunBind (map f cs ++ [els]) where

ambig x ys = "Ambiguous Name: " ++ show x ++ "\nCould refer to: " ++ tupled (map show ys)

data NameEntry
    = NameJust Name
    | NameBound Name [Name]
    | NameAmbig [Name]
    deriving(Eq,Ord)

runRename :: MonadWarn m => (a -> RM b) -> Opt -> Module -> FieldMap -> [(Name,[Name])] -> a -> m (b,Map.Map Name Name)
runRename doit opt mod fls ns m = mapM_ addWarning errors >> return (renamedMod,reverseMap) where
    nameMap = fmap f (Map.fromList ns) where
        f [y] = NameJust y
        f ys | ([ln],rs) <- partition (\c -> getModule c == Just mod) ys = NameBound ln rs
        f ys  = NameAmbig ys
    startState = ScopeState 1
    startEnv = Env {
        envModule      = mod,
        envNameMap     = nameMap,
        envOptions     = opt,
        envFieldLabels = fls,
        envInPattern   = False,
        envSrcLoc      = mempty
    }
    (renamedMod, _, (reverseMap,errors)) = runRWS (unRM $ doit m) startEnv startState

{-# NOINLINE renameModule #-}
renameModule :: MonadWarn m => Opt -> FieldMap -> [(Name,[Name])] -> HsModule -> m ((HsModule,[HsDecl]),Map.Map Name Name)
renameModule opt fls ns m = runRename go opt (hsModuleName m) fls (ns ++ driftResolvedNames) m
  where go mod = do
          let renDesugared = renameDecls . desugarHsModule
          rmod <- renDesugared mod
          dd <- driftDerive rmod
          inst <- hsModuleDecls `fmap` renDesugared mod{hsModuleDecls = dd}
          return (hsModuleDecls_u (++ inst) rmod,inst)

{-# NOINLINE renameStatement #-}
renameStatement :: MonadWarn m => FieldMap -> [(Name,[Name])] ->  Module -> HsStmt -> m HsStmt
renameStatement fls ns modName stmt = fst `liftM` runRename rename options modName fls ns stmt

withSubTable :: SubTable -> RM a -> RM a
withSubTable st action = local (\e -> e { envNameMap = Map.map NameJust st `union` envNameMap e }) action

renameDecls :: HsModule -> RM HsModule
renameDecls mod = do
    withSrcLoc (hsModuleSrcLoc mod) $ do
    addTopLevels mod $ do
    decls' <- renameHsDecls ContextTopLevel (hsModuleDecls mod)
    mapM_ checkExportSpec $ fromMaybe [] (hsModuleExports mod)
    return mod { hsModuleDecls = decls' }

checkExportSpec :: HsExportSpec -> RM ()
checkExportSpec e = f [DataConstructor, TypeConstructor, ClassName] e where
    f _ (HsEVar n) = do check [Val] n
    f dt (HsEAbs n) = do check dt n
    f dt (HsEThingAll n) = do check dt n
    f dt (HsEThingWith n ns) = do
        check dt n
        mapM_ (check [DataConstructor,Val]) ns
    f _ HsEModuleContents {} = return ()
    f _ (HsEQualified nt he) = f [nt] he
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
    g _ = return []

instance Rename HsDecl where
    rename d = withSrcLoc (FrontEnd.SrcLoc.srcLoc d) $ renameHsDecl d

renameHsDecl d = f d where
    f (HsPatBind srcLoc hsPat hsRhs {-where-} hsDecls) = do
        hsPat'    <- local (envInPattern_s True) $ rename hsPat
        updateWithN Val hsDecls $ do
        hsDecls'  <- rename hsDecls
        hsRhs'    <- rename hsRhs
        return (HsPatBind srcLoc hsPat' hsRhs' {-where-} hsDecls')
    f (HsForeignExport a b n t) = do
        n <- local (envInPattern_s True) $ renameName n
        updateWith t $ do
            t <- rename t
            return (HsForeignExport a b n t)
    f (HsForeignDecl a b n t) = do
        n <- local (envInPattern_s True) $ renameName n
        updateWith t $ do
        t <- rename t
        return (HsForeignDecl a b n t)
    f (HsFunBind hsMatches) = do
        hsMatches' <- rename hsMatches
        return (HsFunBind hsMatches')
    f (HsTypeSig srcLoc hsNames hsQualType) = do
        hsNames' <- local (envInPattern_s True) $ mapM renameName hsNames
        updateWith hsQualType $ do
            hsQualType' <- rename hsQualType
            return (HsTypeSig srcLoc hsNames' hsQualType')
    f HsDataDecl { .. } | hsDeclDeclType == DeclTypeKind = do
        hsDeclName <- renameName hsDeclName
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
        hsDeclName <- renameName hsDeclName
        updateWith (map fromTypishHsName hsDeclArgs) $ do
            hsDeclContext <- rename hsDeclContext
            hsDeclArgs <- mapM renameName hsDeclArgs
            hsDeclCons <- rename hsDeclCons
            hsDeclDerives <- mapM renameName hsDeclDerives
            return HsDataDecl { .. }
    f (HsTypeDecl srcLoc name hsNames t) = do
        hsName' <- renameName name
        updateWith (Set.toList $ freeVars hsNames :: [Name]) $ do
            hsNames' <- rename hsNames
            t' <- rename t
            return (HsTypeDecl srcLoc  hsName' hsNames' t')
    f HsTypeFamilyDecl { .. } = do
        hsDeclCName <- renameName hsDeclName
        updateWith (Set.toList $ freeVars hsDeclTArgs :: [Name]) $ do
            hsDeclTArgs <- rename hsDeclTArgs
            return HsTypeFamilyDecl { .. }
    f (HsClassDecl srcLoc classHead hsDecls) = do
        classHead' <- updateWithN TypeVal (hsClassHeadArgs classHead) $ rename classHead
        hsDecls' <- rename hsDecls
        return (HsClassDecl srcLoc classHead' hsDecls')
    f (HsClassAliasDecl srcLoc name args hsContext hsClasses hsDecls) = do
        name' <- renameName name
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
        hsNames' <- mapM renameName hsNames
        return $ HsInfixDecl srcLoc assoc int hsNames'
    f (HsActionDecl srcLoc pat e) = do
        pat <- rename pat
        e <- rename e
        return (HsActionDecl srcLoc pat e)
    f (HsPragmaProps srcLoc prop hsNames) = do
        hsNames' <- mapM renameName hsNames
        return (HsPragmaProps  srcLoc prop hsNames')
    f (HsPragmaRules rs) = do
        rs' <- rename rs
        return $ HsPragmaRules rs'
    f prules@HsPragmaSpecialize { hsDeclSrcLoc = srcLoc, hsDeclName = n, hsDeclType = t } = do
        n <- if n == u_instance then return n else renameName n
        let ns = snub (getNames t)
        updateWith t $ do
            ns' <- mapM renameName ns
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
        fvs' <- sequence [ liftM2 (,) (renameName x) (withSubTable subTable'' $ rename y)| (x,y) <- fvs]
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
        hsName2s' <- mapM renameName hsName2s
        return (HsAsst hsName1' hsName2s')
    rename (HsAsstEq t1 t2) = HsAsstEq <$> rename t1 <*> rename t2

instance Rename HsConDecl where
    --rename cd@(HsConDecl { hsConDeclSrcLoc = srcLoc, hsConDeclName = hsName, hsConDeclConArg = hsBangTypes }) = do
    rename cd@(HsConDecl {  hsConDeclName = hsName, hsConDeclConArg = hsBangTypes, .. }) = do
        withSrcLoc hsConDeclSrcLoc $ do
        hsName' <- renameName hsName
        updateWith  (map (toName TypeVal . hsTyVarBindName) hsConDeclExists) $ do
        hsConDeclExists <- rename hsConDeclExists
        hsBangTypes' <- rename hsBangTypes
        return cd { hsConDeclName = hsName', hsConDeclConArg = hsBangTypes', hsConDeclExists }
    rename cd@HsRecDecl { hsConDeclSrcLoc = srcLoc, hsConDeclName = hsName, hsConDeclRecArg = stuff} = do
        withSrcLoc srcLoc $ do
        hsName' <- renameName hsName
        updateWith (map (toName TypeVal . hsTyVarBindName) (hsConDeclExists cd)) $ do
        es <- rename (hsConDeclExists cd)
        stuff' <- sequence [ do ns' <- mapM renameName (map (toName FieldLabel) ns); t' <- rename t; return (ns',t')  |  (ns,t) <- stuff]
        return cd { hsConDeclName = hsName', hsConDeclRecArg = stuff', hsConDeclExists = es }

renameKindHsCon HsConDecl { .. } = do
    withSrcLoc hsConDeclSrcLoc $ do
    hsConDeclName <- renameName hsConDeclName
    unless (null hsConDeclExists) $
        addWarn InvalidDecl "kind declarations cannot have existential types"
    let bt e@HsBangedTy {} = do
            addWarn InvalidDecl "strictness annotations not relevant to kind declarations"
            return e
        bt (HsUnBangedTy e) = HsUnBangedTy `liftM` f e
        f (HsTyCon n) = HsTyCon `liftM` renameName n
        f e = addWarn InvalidDecl "invalid argument in kind declaration" >> return e
    hsConDeclConArg <- mapM bt hsConDeclConArg
    return HsConDecl { .. }
renameKindHsCon _ = error "Rename.renameKindHsCon: bad."

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
    rt (HsTyTuple []) = return $ HsTyTuple []
    rt (HsTyVar hsName) | dovar = do
        hsName' <- renameName hsName
        return (HsTyVar hsName')
    rt v@HsTyVar {} = return v
    rt (HsTyCon hsName) = do
        hsName' <- renameName hsName
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
        n' <- renameName n
        return tvb { hsTyVarBindName = n' }

-- note that for renameHsMatch, the 'wheres' dominate the 'pats'

instance Rename HsMatch where
    rename (HsMatch srcLoc hsName hsPats hsRhs {-where-} hsDecls) = do
        withSrcLoc srcLoc $ do
        hsName' <- local (envInPattern_s True) $ renameName hsName
        updateWithN Val hsPats  $ do
        hsPats' <- local (envInPattern_s True) $ rename hsPats
        updateWithN Val hsDecls $ do
        hsDecls' <- rename (expandTypeSigs hsDecls)
        mapM_ HsErrors.hsDeclLocal hsDecls'
        hsRhs' <- rename hsRhs
        return (HsMatch srcLoc hsName' hsPats' hsRhs' {-where-} hsDecls')

instance Rename HsPat where
    rename (HsPVar hsName) = HsPVar `fmap` renameName hsName
    rename (HsPInfixApp hsPat1 hsName hsPat2) = HsPInfixApp <$> rename hsPat1 <*> renameName hsName <*> rename hsPat2
    rename (HsPApp hsName hsPats) = HsPApp <$> renameName hsName <*> rename hsPats
    rename (HsPRec hsName hsPatFields) = do
        hsName' <- renameName hsName
        hsPatFields' <- rename hsPatFields
        fls <- asks envFieldLabels
        buildRecPat fls hsName' hsPatFields'
    rename (HsPAsPat hsName hsPat) = HsPAsPat <$> renameName hsName <*> rename hsPat
    rename (HsPTypeSig sl hsPat qt)  = HsPTypeSig sl <$> rename hsPat <*> rename qt
    rename (HsPatExp e) = HsPatExp <$> rename e
    rename p = traverseHsPat rename p

instance Rename HsPatField where
    rename (HsField hsName hsPat) = do
        hsName' <- renameName (toName FieldLabel hsName)
        hsPat' <- rename hsPat
        return (HsField hsName' hsPat')

instance Rename HsRhs where
    rename (HsUnGuardedRhs hsExp) = HsUnGuardedRhs <$> rename hsExp
    rename (HsGuardedRhss rs) = HsGuardedRhss <$> rename rs

instance Rename HsComp where
    rename (HsComp srcLoc e1 e2) = withSrcLoc srcLoc $
        HsComp srcLoc <$> rename e1 <*> rename e2

f_fromRational = HsVar (toUnqualified v_fromRational)

newVar = do
    unique <- newUniq
    mod <- getCurrentModule
    return $ mkComplexName emptyNameParts {
            nameModule = Just mod,
            nameLevel = termLevel,
            nameConstructor = False,
            nameUniquifier = Just unique,
            nameIdent = "var@"
        }

noDesugar s = do
    sugar <- flagOpt FO.Sugar
    unless sugar $ eWarn s

instance Rename HsExp where
    rename (HsVar hsName) = HsVar <$> renameName hsName
    rename (HsCon hsName) = HsCon <$> renameName hsName
    rename i@(HsLit HsInt {}) = do
        noDesugar "int desugaring disabled by -fno-sugar"
        return i
    rename i@(HsLit HsFrac {}) = do
        noDesugar "fractional desugaring disabled by -fno-sugar"
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
    rename orig@(HsDo hsStmts) = do
        (ss,()) <- renameHsStmts hsStmts (return ())
        sugar <- flagOpt FO.Sugar
        if sugar
        then doToExp newVar (v_bind) (v_bind_) (v_fail) ss
        else do eWarn "do desugaring disabled by -fno-sugar"
                return $ HsDo ss
    rename (HsRecConstr hsName hsFieldUpdates) = do
        hsName' <- renameName hsName
        hsFieldUpdates' <- rename hsFieldUpdates
        fls <- asks envFieldLabels
        buildRecConstr fls hsName' (hsFieldUpdates'::[HsFieldUpdate])
    rename (HsRecUpdate hsExp hsFieldUpdates) = do
        hsExp' <- rename hsExp
        hsFieldUpdates' <- rename hsFieldUpdates
        fls <- asks envFieldLabels
        buildRecUpdate fls hsExp' hsFieldUpdates' -- HsRecConstr hsName' hsFieldUpdates')
        --return (HsRecUpdate hsExp' hsFieldUpdates')
    rename (HsEnumFrom hsExp) = desugarEnum vu_enumFrom [hsExp]
    rename (HsEnumFromTo hsExp1 hsExp2) = desugarEnum vu_enumFromTo [hsExp1, hsExp2]
    rename (HsEnumFromThen hsExp1 hsExp2) = desugarEnum vu_enumFromThen [hsExp1, hsExp2]
    rename (HsEnumFromThenTo hsExp1 hsExp2 hsExp3) = desugarEnum vu_enumFromThenTo [hsExp1, hsExp2, hsExp3]
   -- rename (HsList []) = return $ HsCon dc_EmptyList
    rename orig@(HsListComp (HsComp sl hsStmts hsExp)) = withSrcLoc sl $ do
        (ss,e) <- renameHsStmts hsStmts (rename hsExp)
        sugar <- flagOpt FO.Sugar
        if sugar
        then listCompToExp newVar e ss
        else do eWarn "list comprehension desugaring disabled by -fno-sugar"
                return $ HsListComp $ HsComp sl ss e
    rename (HsExpTypeSig srcLoc hsExp hsQualType) = do
        hsExp' <- rename hsExp
        updateWith hsQualType $ do
            hsQualType' <- rename hsQualType
            return (HsExpTypeSig srcLoc hsExp' hsQualType')
    rename (HsAsPat hsName hsExp) = HsAsPat <$> renameName hsName <*> rename hsExp
    rename (HsWildCard sl) = do
        withSrcLoc sl $ do
            e <- createError HsErrorUnderscore ("_")
            return e
    rename p = traverseHsExp rename p

desugarEnum s as = do
        sugar <- flagOpt FO.Sugar
        if sugar
        then rename $ foldl HsApp (HsVar s) as
        else do eWarn "enum desugaring disabled by -fno-sugar"
                return $ foldl HsApp (HsVar s) as

createError et s = do
    sl <- getSrcLoc
    return $ HsError { hsExpSrcLoc = sl, hsExpErrorType = et, hsExpString = (show sl ++ ": " ++ s) }

failRename s = do
    sl <- getSrcLoc
    fail (show sl ++ ": " ++ s)

eWarn = addWarn InvalidExp

getRecInfo :: FieldMap -> Maybe Name -> [HsField a] -> RM [(Name,(Int,[(Int,(FieldName,a))]))]
getRecInfo (FieldMap amp fls) constructorName fields = do
    when (hasDuplicates fieldLabels) $ do
        failRename "Duplicate field labels in pattern"
    when hasDotDot $ do
        failRename "Wildcard pattern not yet supported"
    let f (HsField n _) | n == u_DotDot = return []
        f (HsField fieldName e) = case mlookup fieldName fls of
            Nothing -> failRename $ "Field Label does not exist: " ++ show fieldName
            Just cs -> return [ (x,(fromIntegral y,(fieldName,e))) | (x,y) <- cs, maybe True (x ==) constructorName]
    fm <- concat <$> mapM f fields
    let fm' = sortGroupUnderFG fst snd fm -- $ maybe fm (\t -> (t,[]):fm) constructorName
        fm'' =  maybe fm' (\t -> if t `elem` fsts fm' then fm' else (t,[]):fm') constructorName
    T.forM fm'' $ \ (t,v) -> case Map.lookup t amp of
        Nothing -> failRename $ "Unknown Constructor: " ++ show t
        Just arity -> return (t,(fromIntegral arity,v))
    where
    (dotDotList,fieldLabels) = partition (u_DotDot ==) [ n | HsField n _ <- fields ]
    hasDotDot = not $ null dotDotList

buildRecPat :: FieldMap -> Name -> [HsPatField] -> RM HsPat
buildRecPat fm n us = do
    [(_,(arity,fs))] <- getRecInfo fm (Just n) us
    let g i | Just (_,e) <- lookup i fs = return $ HsPParen e
            | otherwise = do
                v <- newVar
                return $ HsPVar v
    rs <- mapM g [0 .. arity - 1 ]
    return $ HsPApp n rs

buildRecConstr ::  FieldMap -> Name -> [HsFieldUpdate] -> RM HsExp
buildRecConstr fm n us = do
    undef <- createError HsErrorUninitializedField "Uninitialized Field"
    [(_,(arity,fs))] <- getRecInfo fm (Just n) us
    let rs = map g [0 .. arity - 1 ]
        g i | Just (_,e) <- lookup i fs = hsParen e
            | otherwise = undef
    return $ foldl HsApp (HsCon n) rs

buildRecUpdate ::  FieldMap -> HsExp -> [HsFieldUpdate] -> RM HsExp
buildRecUpdate fm n us = do
    rs <- getRecInfo fm Nothing us
    sl <- getSrcLoc
    let g (c,(arity,zs)) = do
            vars <- replicateM arity newVar
            let x = foldl HsApp (HsCon c) [ maybe (HsVar v) snd (lookup i zs) | v <- vars | i <- [ 0 .. arity - 1] ]
            return $ HsAlt sl (HsPApp c (map HsPVar vars))  (HsUnGuardedRhs x) []
    as <- mapM g rs
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
    rename (HsField hsName hsExp) = do
        hsName' <- renameName (toName FieldLabel hsName)
        hsExp' <- rename hsExp
        return (HsField hsName' hsExp')

renameName :: Name -> RM Name
-- a few hard coded cases
renameName hsName
    | Just n <- fromQuotedName hsName = return n
    | hsName `elem` [tc_Arrow,dc_Unit,tc_Unit] = return hsName
--    | (nt,Just m,i) <- nameParts hsName, '@':_ <- show m = return $ toName nt (m, i)
    | Just _ <- V.fromTupname hsName = return hsName
    | nameType hsName == UnknownType = return hsName
renameName hsName = do
    subTable <- asks envNameMap
    inPattern <- asks envInPattern
    case mlookup hsName subTable of
        Just (NameJust name) -> do
            tell (Map.singleton name hsName,mempty)
            return name
        Just (NameBound x _) | inPattern -> do
            tell (Map.singleton x hsName,mempty)
            return x
        Just (NameAmbig xs) -> do
            addWarn (AmbiguousName hsName xs) (ambig hsName xs)
            return hsName
        Just (NameBound x xs) -> do
            addWarn (AmbiguousName hsName (x:xs)) (ambig hsName $ x:xs)
            return hsName
        Nothing -> do
            addWarn (UndefinedName hsName) $ "Unknown name: " ++ show hsName
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
getUpdatesN nt x = unions `fmap` mapM clobberName [ n | n <- getNames x, nameType n == nt, n /= vu_sub]
--getUpdatesN nt x = unions `fmap` mapM clobberName (map (toName nt) $ getNames x)

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
    f (HsPatBind srcLoc (HsPVar hsName) _ _) = [(hsName, srcLoc)]
    f (HsPatBind sloc p _ _) = [ (n, sloc) | n <- (getNamesFromHsPat p) ]
--    f (HsPatBind sloc _ _ _) = error $ "non simple pattern binding found (sloc): " ++ show sloc
    f (HsFunBind (HsMatch { .. }:_)) = [(hsMatchName,hsMatchSrcLoc)]
    f (HsForeignDecl { .. }) = [(hsDeclName, hsDeclSrcLoc)]
    f _ = []

-- | Collect all names defined in a module as well as their declaration points
-- and any subnames they might have. In addition, collect the arities of any
-- constructors.

collectDefsHsModule :: HsModule -> ([(Name,SrcLoc,[Name])],[(Name,Int)])
collectDefsHsModule m = (\(x,y) -> (Seq.toList x,Seq.toList y)) $ execWriter (mapM_ f (hsModuleDecls m)) where
--    toName t n | nameType n == UnknownType = n
    toName t n  = Name.toName t (qualifyName (hsModuleName m) n)
             --  | otherwise = error $ show (t,n,nameType n)
    tellName sl n = tellF [(n,sl,[])]
    tellF xs = tell (Seq.fromList xs,Seq.empty) >> return ()
    tellS xs = tell (Seq.empty,Seq.fromList xs) >> return ()
    f (HsForeignDecl a _ n _)    = tellName a (toName Val n)
    f (HsForeignExport a e _ _)  = tellName a (ffiExportName e)
    f (HsFunBind (HsMatch a n _ _ _:_))  = tellName a (toName Val n)
    f (HsPatBind srcLoc (HsPVar n) _ _)  = tellName srcLoc (toName Val n)
    f (HsPatBind srcLoc p _ _)  = mapM_ (tellName srcLoc) [ toName Val n | n <- (getNamesFromHsPat p), n /= vu_sub, nameType n == Val ]
    f (HsActionDecl srcLoc p _) = mapM_ (tellName srcLoc) [ toName Val n | n <- (getNamesFromHsPat p), n /= vu_sub, nameType n == Val ]
    f (HsTypeDecl sl n _ _) = tellName sl (toName TypeConstructor n)
    f HsDataDecl { hsDeclDeclType = DeclTypeKind, hsDeclSrcLoc =sl, hsDeclName = n, hsDeclCons = cs } = do
        tellF $ (toName SortName n,sl,snub [ x |(x,_,_) <- cs']): cs' ; zup cs where
            cs' = concatMap (namesHsConDeclSort' toName) cs
    f HsDataDecl { hsDeclSrcLoc =sl, hsDeclName = n, hsDeclCons = cs } = do
        tellF $ (toName TypeConstructor n,sl,snub [ x |(x,_,_) <- cs']): cs' ; zup cs where
            cs' = nubBy (\ (x,_,_) (y,_,_) -> x == y) $ concatMap (namesHsConDecl' toName) cs
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

newtype RM a = RM { unRM :: RWS Env (Map.Map Name Name,[Warning]) ScopeState a }
    deriving(Applicative, Functor,MonadReader Env, MonadWriter (Map.Map Name Name,[Warning]), MonadState ScopeState)

instance Monad RM where
    RM x >> RM y = RM $ x >> y
    RM x >>= y = RM $ do
        r <- x
        unRM $ y r
    return x = RM $ return x
    fail s = do
        sl <- getSrcLoc
        addWarn ParseError s
        RM $ fail $ show sl ++ ":" ++ s

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
    withSrcLoc' sl a = local (\s -> s { envSrcLoc = sl `mappend` envSrcLoc s}) a
instance OptionMonad RM where
    getOptions = asks envOptions

class DeNameable a where
    deName :: Module -> a -> a

instance (Functor f,DeNameable a) => DeNameable (f a) where
    deName m fx = fmap (deName m) fx

instance DeNameable Name where
    deName mod name = mkComplexName np { nameUniquifier = Nothing, nameModule = fm nameModule } where
        np@NameParts { .. } = unMkName name
        fm x = do r <- x; guard (r `notElem` mod:removedMods); return r
        removedMods = [ mod_Prelude,mod_JhcBasics,mod_JhcPrimIO,mod_JhcTypeWord,mod_JhcTypeBasic]

instance DeNameable HsPat where
    deName mod p = f p where
        f (HsPVar v) = HsPVar (deName mod v)
        f (HsPApp cn pats) = HsPApp (deName mod cn) (deName mod pats)
        f (HsPAsPat n p) = HsPAsPat (deName mod n) (deName mod p)
        f p = runIdentity $ traverseHsPat (return . f) p

instance DeNameable HsAlt where
    deName mod (HsAlt sl p rhs ds) = HsAlt sl (dn p) (dn rhs) ds where
        dn x = deName mod x

instance DeNameable HsRhs where
    deName mod (HsUnGuardedRhs e) = HsUnGuardedRhs (deName mod e)
    deName mod (HsGuardedRhss es) = HsGuardedRhss (deName mod es)
instance DeNameable HsComp where
    deName mod c = runIdentity $ traverseHsExp (return . deName mod) c

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

doToExp :: Monad m
    => m HsName    -- ^ name generator
    -> HsName      -- ^ bind (>>=) to use
    -> HsName      -- ^ bind_ (>>) to use
    -> HsName      -- ^ fail to use
    -> [HsStmt]
    -> m HsExp
doToExp newName f_bind f_bind_ f_fail ss = hsParen `liftM` f ss where
    f [] = fail "doToExp: empty statements in do notation"
    f [HsQualifier e] = return e
    f [gen@(HsGenerator srcLoc _pat _e)] = fail $ "doToExp: last expression n do notation is a generator (srcLoc):" ++ show srcLoc
    f [letst@(HsLetStmt _decls)] = fail $ "doToExp: last expression n do notation is a let statement"
    f (HsQualifier e:ss) = do
        ss <- f ss
        return $ HsInfixApp (hsParen e) (HsVar f_bind_) (hsParen ss)
    f ((HsGenerator _srcLoc pat e):ss) | isSimplePat pat = do
        ss <- f ss
        return $ HsInfixApp (hsParen e) (HsVar f_bind) (HsLambda _srcLoc [pat] ss)
    f ((HsGenerator srcLoc pat e):ss) = do
        npvar <- newName
        ss <- f ss
        let kase = HsCase (HsVar npvar) [a1, a2 ]
            a1 =  HsAlt srcLoc pat (HsUnGuardedRhs ss) []
            a2 =  HsAlt srcLoc HsPWildCard (HsUnGuardedRhs (HsApp (HsVar f_fail) (HsLit $ HsString $ show srcLoc ++ " failed pattern match in do"))) []
        return $ HsInfixApp (hsParen e) (HsVar f_bind) (HsLambda srcLoc [HsPVar npvar] kase)  where
    f (HsLetStmt decls:ss) = do
        ss <- f ss
        return $ HsLet decls ss

hsApp e es = hsParen $ foldl HsApp (hsParen e) (map hsParen es)
hsIf e a b = hsParen $ HsIf e a b

listCompToExp :: Monad m => m HsName -> HsExp -> [HsStmt] -> m HsExp
listCompToExp newName exp ss = hsParen `liftM` f ss where
    f [] = return $ HsList [exp]
    f (gen:HsQualifier q1:HsQualifier q2:ss)  = f (gen:HsQualifier (hsApp (HsVar v_and) [q1,q2]):ss)
    f ((HsLetStmt ds):ss) = do ss' <- f ss; return $ hsParen (HsLet ds ss')
    f (HsQualifier e:ss) = do ss' <- f ss; return $ hsParen (HsIf e ss' (HsList []))
    f ((HsGenerator srcLoc pat e):ss) | isLazyPat pat, Just exp' <- g ss = do
        return $ hsParen $ HsVar v_map `app` HsLambda srcLoc [pat] exp' `app` e
    f ((HsGenerator srcLoc pat e):HsQualifier q:ss) | isLazyPat pat, Just exp' <- g ss = do
        npvar <- newName
        return $ hsApp (HsVar v_foldr)  [HsLambda srcLoc [pat,HsPVar npvar] $
            hsIf q (hsApp (HsCon dc_Cons) [exp',HsVar npvar]) (HsVar npvar), HsList [],e]
    f ((HsGenerator srcLoc pat e):ss) | isLazyPat pat = do
        ss' <- f ss
        return $ hsParen $ HsVar v_concatMap `app`  HsLambda srcLoc [pat] ss' `app` e
    f ((HsGenerator srcLoc pat e):HsQualifier q:ss) | isFailablePat pat || Nothing == g ss = do
        npvar <- newName
        ss' <- f ss
        let kase = HsCase (HsVar npvar) [a1, a2 ]
            a1 =  HsAlt srcLoc pat (HsGuardedRhss [HsComp srcLoc [HsQualifier q] ss']) []
            a2 =  HsAlt srcLoc HsPWildCard (HsUnGuardedRhs $ HsList []) []
        return $ hsParen $ HsVar v_concatMap `app`  HsLambda srcLoc [HsPVar npvar] kase `app`  e
    f ((HsGenerator srcLoc pat e):ss) | isFailablePat pat || Nothing == g ss = do
        npvar <- newName
        ss' <- f ss
        let kase = HsCase (HsVar npvar) [a1, a2 ]
            a1 =  HsAlt srcLoc pat (HsUnGuardedRhs ss') []
            a2 =  HsAlt srcLoc HsPWildCard (HsUnGuardedRhs $ HsList []) []
        return $ hsParen $ HsVar v_concatMap `app` HsLambda srcLoc [HsPVar npvar] kase `app` e
    f ((HsGenerator srcLoc pat e):ss) = do
        npvar <- newName
        let Just exp' = g ss
            kase = HsCase (HsVar npvar) [a1 ]
            a1 =  HsAlt srcLoc pat (HsUnGuardedRhs exp') []
        return $ hsParen $ HsVar v_map `app` HsLambda srcLoc [HsPVar npvar] kase `app` e
    g [] = return exp
    g (HsLetStmt ds:ss) = do
        e <- g ss
        return (hsParen (HsLet ds e))
    g _ = Nothing
    app x y = HsApp x (hsParen y)

-- patterns are
-- failable - strict and may fail to match
-- refutable or strict - may bottom out
-- irrefutable or lazy - match no matter what
-- simple, a wildcard or variable
-- failable is a subset of refutable

isFailablePat p | isStrictPat p = f (openPat p) where
    f (HsPTuple ps) = any isFailablePat ps
    f (HsPUnboxedTuple ps) = any isFailablePat ps
    f (HsPBangPat (Located _ p)) = isFailablePat p
    f _ = True
isFailablePat _ = False

isSimplePat p = f (openPat p) where
    f HsPVar {} = True
    f HsPWildCard = True
    f _ = False

isLazyPat pat = not (isStrictPat pat)
isStrictPat p = f (openPat p) where
    f HsPVar {} = False
    f HsPWildCard = False
    f (HsPAsPat _ p) = isStrictPat p
    f (HsPParen p) = isStrictPat p
    f (HsPIrrPat p) = False -- isStrictPat p  -- TODO irrefutable patterns
    f _ = True

openPat (HsPParen p) = openPat p
openPat (HsPNeg p) = openPat p
openPat (HsPAsPat _ p) = openPat p
openPat (HsPTypeSig _ p _) = openPat p
openPat (HsPInfixApp a n b) = HsPApp n [a,b]
openPat p = p

hasDuplicates xs = any ((> 1) . length) $ group (sort xs)
