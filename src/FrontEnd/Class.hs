{-# LANGUAGE NoMonoLocalBinds, NamedFieldPuns #-}
module FrontEnd.Class(
    printClassHierarchy,
    instanceToTopDecls,
    ClassHierarchy(),
    augmentClassHierarchy,
    ClassRecord(..),
    ClassType(..),
    instanceName,
    defaultInstanceName,
    printClassSummary,
    findClassInsts,
    findClassRecord,
    asksClassRecord,
    classRecords,
    addInstanceToHierarchy,
    makeClassHierarchy,
    scatterAliasInstances,
    derivableClasses,
    enumDerivableClasses,
    noNewtypeDerivable,
    makeInstanceEnv,
    emptyInstance,
    InstanceEnv(..),
    Inst(..)
    ) where

import Control.Monad.Identity
import Control.Monad.Writer
import Data.Generics(mkQ,something)
import Data.List(nub)
import Debug.Trace
import Text.PrettyPrint.HughesPJ(render,Doc())
import qualified Data.Map as Map
import qualified Text.PrettyPrint.HughesPJ as PPrint

import Data.Binary
import Doc.DocLike
import Doc.PPrint
import FrontEnd.HsSyn
import FrontEnd.KindInfer
import FrontEnd.SrcLoc
import FrontEnd.Tc.Kind
import FrontEnd.Tc.Type
import FrontEnd.Utils
import Maybe
import Name.Name
import Name.Names
import Options (verbose)
import Support.CanType
import Support.FreeVars
import Support.MapBinaryInstance
import Support.Tickle
import Util.Gen
import Util.HasSize
import Util.Inst()

type Assump = (Name,Sigma)

data Inst = Inst {
    instSrcLoc  :: SrcLoc,
    instDerived :: !Bool,   -- ^ whether this instance was derived
    instHead    :: Qual Pred,
    instAssocs  :: [(Tycon,[Tyvar],[Tyvar],Sigma)]
    } deriving(Eq,Ord,Show)
    {-! derive: Binary !-}

instance PPrint a (Qual Pred) => PPrint a Inst where
    pprint Inst { instHead = h, instAssocs = [], instDerived = d } = (if d then text "*" else text " ") <> pprint h
    pprint Inst { instHead = h, instAssocs = as, instDerived = d } = (if d then text "*" else text " ") <> pprint h <+> text "where" <$> vcat [ text "    type" <+> pprint n <+> text "_" <+> hsep (map pprint ts) <+> text "=" <+> pprint sigma  | (n,_,ts,sigma) <- as]

emptyInstance = Inst { instDerived = False, instSrcLoc = bogusASrcLoc, instHead = error "emptyInstance", instAssocs = [] }

-- augment heirarchy with just instances with full class definitions
augmentClassHierarchy :: ClassHierarchy -> ClassHierarchy -> ClassHierarchy
augmentClassHierarchy (CH full _) (CH res is) = ans where
    ans = CH (Map.mapWithKey f is) is
    f cn _ = r where Just r = Map.lookup cn (Map.union res full)

data ClassType = ClassNormal | ClassTypeFamily | ClassDataFamily | ClassAlias
        deriving(Eq,Ord)

-- Bool is true if data declaration instead of type declaration
data AssociatedType = Assoc !Tycon !Bool [Tyvar] Kind
    deriving(Eq,Show)
    {-! derive: Binary !-}

data ClassRecord = ClassRecord {
    className    :: !Class, -- ^ can be a TypeConstructor if we are a type or data family
    classSrcLoc  :: !SrcLoc,
    classArgs    :: [Tyvar],
    classSupers  :: [Class], -- TODO: should be Pred
    classAlias   :: Maybe (Qual [Pred]),
    classAssumps :: [(Name,Sigma)], -- ^ method signatures
    classAssocs  :: [AssociatedType]
    } deriving (Show,Eq)
    {-! derive: Binary !-}

newClassRecord c = ClassRecord {
    className = c,
    classSrcLoc = bogusASrcLoc,
    classSupers = [],
    classArgs = [],
    classAssumps = [],
    classAlias = Nothing,
    classAssocs = []
    }

newtype InstanceEnv = InstanceEnv {
    instanceEnv :: Map.Map (Name,Name) ([Tyvar],[Tyvar],Type) }

makeInstanceEnv :: ClassHierarchy -> InstanceEnv
makeInstanceEnv (CH _ is) = InstanceEnv $ Map.fromList (concatMap f (Map.toList is)) where
    f (cr,is) = concatMap (g cr) is
    g cr Inst { instHead = _ :=> IsIn _cname tt, instAssocs = as } | _cname == cr = ans where
        ans = [ ((tyconName tc,getTypeHead tt),(is,rs,e)) | (tc,is,rs,e) <- as]
    g cr x = error $  "makeInstanceEnv: " ++ show (cr,x)

getTypeHead th = case fromTAp th of
    (TArrow {},_) -> tc_Arrow
    (TCon c,_) -> tyconName c
    _ -> error $ "getTypeHead: " ++ show th

data ClassHierarchy = CH (Map.Map Class ClassRecord) (Map.Map Class [Inst])

instance Binary ClassHierarchy where
    get = do
        m1 <- getMap
        m2 <- getMap
        return (CH m1 m2)
    put (CH m1 m2) = do
        putMap m1
        putMap m2

instance Monoid ClassHierarchy where
    mempty = CH mempty mempty
    mappend (CH a b) (CH c d) =
        CH (Map.union a c) (Map.unionWith (++) b d)

classRecords :: ClassHierarchy -> [ClassRecord]
classRecords (CH ch _) = Map.elems ch

findClassRecord (CH ch _) cn = case Map.lookup cn ch of
    Nothing -> error $ "findClassRecord: " ++ show cn
    Just n -> n

asksClassRecord (CH ch _) cn f = case Map.lookup cn ch of
    Nothing -> error $ "asksClassRecord: " ++ show cn
    Just n -> f n
findClassInsts (CH _ is) cn = fromMaybe [] (Map.lookup cn is)

showInst :: Inst -> String
showInst = PPrint.render . pprint

aHsTypeSigToAssumps :: KindEnv -> HsDecl -> [(Name,Type)]
aHsTypeSigToAssumps kt sig@(HsTypeSig _ names qualType) = [ (toName Val n,typ) | n <- names] where
    Identity typ = hsQualTypeToSigma kt qualType

qualifyMethod :: [HsAsst] -> HsDecl -> HsDecl
qualifyMethod [HsAsst c [n]] (HsTypeSig sloc names (HsQualType oc t))
    = HsTypeSig sloc names (HsQualType (HsAsst c [n']:oc) t) where
        Just n' = (something (mkQ mzero f)) t
        f (HsTyVar n') | hsNameToOrig n' == hsNameToOrig n = return n'
        f _ = mzero

printClassSummary :: ClassHierarchy -> IO ()
printClassSummary (CH h is) = mapM_ f (Map.toList h) where
    --h' = [ (n,fromJust $ Map.lookup n h) | n <- (map fst [ (cn, classSupers ss) | (cn,ss) <- Map.toList h]) ]
    f (cname, ClassRecord { .. }) = do
        putStrLn $ "-- class: " ++ show cname
        let insts = fromMaybe [] (Map.lookup cname is)
        unless (null classSupers) $ putStrLn $ "super classes:" ++ unwords (map show classSupers)
        unless (null insts) $ putStrLn $ "instances: " ++ (intercalate ", " (map showInst insts))
--        putStrLn ""
--    f (cname, (ClassAliasRecord { classSupers = supers, classInsts = insts, classClasses = classes })) = do
--        putStrLn $ "-- class: " ++ show cname
--        unless (null supers) $ putStrLn $ "super classes:" ++ unwords (map show supers)
--        unless (null insts) $ putStrLn $ "instances: " ++ (intercalate ", " (map showInst insts))
--        unless (null classes) $ putStrLn $ "alias for: " ++ unwords (map show classes)
--        putStrLn ""

printClassHierarchy :: ClassHierarchy -> IO ()
printClassHierarchy (CH h is) = mapM_ printClassDetails $  Map.toList h where
    printClassDetails :: (Name, ClassRecord) -> IO ()
    printClassDetails (cname, cr) = do
        let args = classArgs cr; supers = classSupers cr;
            methodAssumps = classAssumps cr
            assocs = classAssocs cr
        let insts = fromMaybe [] (Map.lookup cname is)
        putStrLn "..........."
        putStrLn $ "class: " ++ hsep (pprint cname:map pprintParen args)
        putStr $ "super classes:"
        pnone supers $ do putStrLn $ " " ++ (intercalate " " (map show supers))
        putStr $ "instances:"
        pnone insts $  putStr $ "\n" ++ (showListAndSepInWidth showInst 80 ", " insts)
        when True $ do
            putStr $ "method signatures:"
            pnone methodAssumps $ putStr $ "\n" ++ (unlines $ map pretty methodAssumps)
            putStr $ "associated types:"
            pnone assocs $  putStrLn $ "\n" ++ (unlines $ map (render . passoc) assocs)
        when (isJust (classAlias cr)) $ do
            let Just x = classAlias cr
            putStr $ "alias for:"
            putStrLn (pprint x)
            --Just $ --pnone classes $ do putStrLn $ " " ++ (intercalate " " (map show classes))
        putStr "\n"
    pnone [] f = putStrLn " none"
    pnone xs f = f
    passoc (Assoc nk isData as kt) = text (if isData then "data" else "type") <+>
        pprint nk <+> hsep (map pprint as) <+> text "::" <+> pprint kt

addOneInstanceToHierarchy :: ClassHierarchy -> Inst -> ClassHierarchy
addOneInstanceToHierarchy (CH r i) inst@Inst { instHead = cntxt :=> IsIn className _ } =
    CH r (Map.insertWith (++) className [inst] i)

addInstanceToHierarchy :: Inst -> ClassHierarchy -> ClassHierarchy
addInstanceToHierarchy inst@Inst { instHead = cntxt :=> IsIn className _ } (CH r i) =
    CH r (Map.insertWith cd className [inst] i) where
        cd [inst] cs = runIdentity $ ensureNotDup (instSrcLoc inst) inst cs

hsInstDeclToInst :: Monad m => KindEnv -> HsDecl -> m [Inst]
hsInstDeclToInst kt (HsInstDecl sloc qType decls)
   | length classKind == length argTypeKind, and subsumptions
        = return [emptyInstance { instSrcLoc = sloc, instDerived = False,
        instHead = cntxt :=> IsIn className convertedArgType, instAssocs = assocs }]
   | otherwise = failSl sloc $ "hsInstDeclToInst: kind error, attempt to make\n" ++
                      show convertedArgType ++ " (with kind " ++ show argTypeKind ++ ")\n" ++
                      "an instance of class " ++ show className ++
                      " (with kind " ++ show classKind ++ ") " ++ show subsumptions
   where
   (cntxt, (className, cargs@[convertedArgType])) = chToClassHead kt qType
   classKind = kindOfClass className kt
   argTypeKind = map getType cargs
   subsumptions = zipWith isSubsumedBy classKind argTypeKind
   assocs = [ (tc,as,bs,s) | (tc,as,bs,~(Just s)) <- createInstAssocs kt decls ]
hsInstDeclToInst _ _ = return []

vtrace s v | False && verbose = trace s v
vtrace s v | otherwise = v

chToClassHead :: KindEnv -> HsClassHead -> ([Pred],(Name,[Type]))
chToClassHead kt qt@HsClassHead { .. }  =
    vtrace ("chToClassHead" <+> show qt) $
    let res = (map (hsAsstToPred kt) hsClassHeadContext,(hsClassHead,
            map (runIdentity . hsTypeToType (kiHsQualType kt (HsQualType hsClassHeadContext (HsTyTuple [])))) hsClassHeadArgs))
    in vtrace ("=" <+> show res) res

createClassAssocs kt decls = [ Assoc (ctc n) False (map ct as) (ctype t) | HsTypeDecl { hsDeclName = n, hsDeclTArgs = as, hsDeclType = t } <- decls ] where
    ctc n = let nn = toName TypeConstructor n in Tycon nn (kindOf nn kt)
    ct (HsTyVar n) = let nn = toName TypeVal n in tyvar nn (kindOf nn kt)
    ctype HsTyAssoc = kindStar
--    ctype t = Just $ runIdentity $ hsTypeToType kt t

createInstAssocs kt decls = [ (ctc n,map ct (czas ca),map ct as,ctype t) | HsTypeDecl { hsDeclName = n, hsDeclTArgs = (ca:as), hsDeclType = t } <- decls ] where
    ctc n = let nn = toName TypeConstructor n in Tycon nn (kindOf nn kt)
    ct (HsTyVar n) = let nn = toName TypeVal n in tyvar nn (kindOf nn kt)
    czas ca = let (HsTyCon {},zas) = fromHsTypeApp ca in zas
    ctype HsTyAssoc = Nothing
    ctype t = Just $ runIdentity $ hsTypeToType kt t

fromHsTypeApp t = f t [] where
    f (HsTyApp a b) rs = f a (b:rs)
    f t rs = (t,rs)

instanceToTopDecls :: KindEnv -> ClassHierarchy -> HsDecl -> (([HsDecl],[Assump]))
instanceToTopDecls kt ch@(CH classHierarchy _) (HsInstDecl _ qualType methods)
    = unzip $ map (methodToTopDecls ch kt [] crecord qualType) $ methodGroups where
    methodGroups = groupEquations (filter (not . isHsPragmaProps) methods)
    (_,(className,_)) = chToClassHead kt qualType
    crecord = case Map.lookup className classHierarchy  of
        Nothing -> error $ "instanceToTopDecls: could not find class " ++ show className ++ "in class hierarchy"
        Just crecord -> crecord
    tsubst na vv v = applyTyvarMap [(na,vv)] v
instanceToTopDecls kt ch@(CH classHierarchy _) (HsClassDecl _ chead methods)
   = unzip $ map (defaultMethodToTopDecls kt methodSigs chead) $ methodGroups where
   className = hsClassHead chead
   --HsQualType _ (HsTyApp (HsTyCon className) _) = qualType
   methodGroups = groupEquations (filter (\x -> isHsPatBind x || isHsFunBind x)  methods)
   methodSigs = case Map.lookup (toName ClassName className) classHierarchy  of
           Nothing -> error $ "defaultInstanceToTopDecls: could not find class " ++ show className ++ "in class hierarchy"
           Just sigs -> classAssumps sigs
instanceToTopDecls _ _ _ = mempty

instanceName n t = toName Val ("Instance@",'i':show n ++ "." ++ show t)
defaultInstanceName n = toName Val ("Instance@",'i':show n ++ ".default")
aliasDefaultInstanceName :: Name -> Class -> Name
aliasDefaultInstanceName n ca = toName Val ("Instance@",'i':show n ++ ".default."++show ca)

methodToTopDecls ::
    ClassHierarchy
    -> KindEnv         -- ^ the kindenv
    -> [Pred]          -- ^ random extra predicates to add
    -> ClassRecord     -- ^ the class we are lifting methods from
    -> HsClassHead
    -> (Name, HsDecl)
    -> (HsDecl,Assump)

methodToTopDecls _  kt preds crecord qt (methodName, methodDecls)
   = (renamedMethodDecls,(newMethodName, instantiatedSig)) where
    (cntxt,(className,[argType])) = chToClassHead kt qt
    newMethodName = instanceName methodName (getTypeHead argType)
    sigFromClass = case [ s | (n, s) <- classAssumps crecord, n == methodName] of
        [x] -> x
        _ -> error $ "sigFromClass: " ++ (pprint className <+> pprint (classAssumps crecord))
                                      ++ " " ++ show  methodName
    instantiatedSig = newMethodSig' kt methodName (preds ++ cntxt) sigFromClass argType
    renamedMethodDecls = renameOneDecl newMethodName methodDecls

defaultMethodToTopDecls :: KindEnv -> [Assump] -> HsClassHead -> (Name, HsDecl) -> (HsDecl,Assump)
defaultMethodToTopDecls kt methodSigs HsClassHead { .. } (methodName, methodDecls)
   = (renamedMethodDecls,(newMethodName,sigFromClass)) where
    --(HsTyApp (HsTyCon className) _) = classApp
    className = hsClassHead
    newMethodName = defaultInstanceName methodName
    sigFromClass = case [ s | (n, s) <- methodSigs, n == methodName] of
        [x] -> x
        _ -> error $ "sigFromClass: " ++ show methodSigs ++ " " ++ show  methodName
     --  = newMethodSig cntxt newMethodName sigFromClass argType
    renamedMethodDecls = renameOneDecl newMethodName methodDecls

aliasDefaultMethodToTopDecls :: KindEnv -> [Assump] -> Class -> (Name, HsDecl) -> (HsDecl,Assump)
aliasDefaultMethodToTopDecls kt methodSigs aliasName (methodName, methodDecls)
   = (renamedMethodDecls,(newMethodName,sigFromClass)) where
     newMethodName = aliasDefaultInstanceName methodName aliasName
     sigFromClass = case [ s | (n, s) <- methodSigs, n == methodName] of
         [x] -> x
         _ -> error $ "sigFromClass: " ++ show methodSigs ++ " " ++ show  methodName
      --  = newMethodSig cntxt newMethodName sigFromClass argType
     renamedMethodDecls = renameOneDecl newMethodName methodDecls

renameOneDecl :: Name -> HsDecl -> HsDecl
renameOneDecl newName (HsFunBind matches)
   = HsFunBind  (map (renameOneMatch newName) matches)
-- all pattern bindings are simple by this stage
-- (ie no compound patterns)
renameOneDecl newName (HsPatBind sloc (HsPVar patName) rhs wheres)
   = HsPatBind sloc (HsPVar (nameName newName)) rhs wheres

renameOneMatch :: Name -> HsMatch -> HsMatch
renameOneMatch newName (HsMatch sloc oldName pats rhs wheres)
   = HsMatch sloc (nameName newName) pats rhs wheres

newMethodSig' :: KindEnv -> Name -> [Pred] -> Sigma -> Type -> Sigma
newMethodSig' kt methodName newCntxt qt' instanceType  = newQualType where
    TForAll _ ((IsIn _ classArg:restContext) :=> t) = qt'
    -- the assumption is that the context is non-empty and that
    -- the class and variable that we are interested in are at the
    -- front of the old context - the method of inserting instance types into
    -- the class hierarchy should ensure this
    --((className, classArg):restContxt) = cntxt
    foo = "_" ++ (show methodName ++ show (getTypeHead instanceType)) ++ "@@"
--    newQualType = everywhere (mkT at) $ tForAll (nub $ freeVars qt) qt

    newQualType = tForAll vs nqt where
        vs = nub $ freeVars nqt
        nqt = map (tickle f) (newCntxt ++ restContext) :=> f t
        f t | t == classArg = f instanceType
        f (TVar t) = TVar (at t)
        f (TForAll ta (ps :=> t)) = tickle f (TForAll (map at ta) (ps :=> t))
        f (TExists ta (ps :=> t)) = tickle f (TExists (map at ta) (ps :=> t))
        f t = tickle f t

    at (Tyvar n k) =  tyvar (updateName (++ foo) n) k
    updateName f n = toName nt (md,f nm) where
         (nt,(md::String,nm)) = fromName n
--    qt = (newCntxt ++ restContext) :=> t
    {-
    qt = (newCntxt ++ restContext) :=> (everywhere (mkT ct) t)
    ct n | n == classArg = instanceType
    ct n =  n
    -}

-- collect assumptions of all class methods

classMethodAssumps :: ClassHierarchy -> [Assump]
classMethodAssumps hierarchy = concatMap classAssumps $ classRecords hierarchy

--------------------------------------------------------------------------------

scatterAliasInstances :: ClassHierarchy -> ClassHierarchy
scatterAliasInstances = id
{-
scatterAliasInstances ch =
    let cas = [cr | cr@(ClassAliasRecord {}) <- classRecords ch]
    --ch `seq` liftIO $ putStrLn ("scatterAliasInstances: " ++ show cas)
        instances = concatMap scatterInstancesOf cas
        ret = foldr (modifyClassRecord $ \cr -> cr
                     { classInsts = [],
                       classMethodMap = Map.fromList [(meth, cls) | cls <- classClasses cr,
                                                                    (meth,_) <- classAssumps (findClassRecord ch cls)]
                     })
                    (ch `mappend` classHierarchyFromRecords instances)
                    (map className cas)
    -- liftIO $ mapM_ print (classRecords ret)
    in ret

scatterInstancesOf :: ClassRecord -> [ClassRecord]
scatterInstancesOf cr = map extract (classClasses cr)
    where
      extract c =
          (newClassRecord c) { classInsts =
                                   [Inst sl d ((cxt ++ [IsIn c2 xs | c2 <- classClasses cr, c2 /= c]) :=> IsIn c xs) []
                                        | Inst sl d (cxt :=> IsIn _ xs) [] <- classInsts cr] }

-}
--------------------------------------------------------------------------------

failSl sl m = fail $ show sl ++ ": " ++ m

classHierarchyFromRecords rs =
    CH (Map.fromList [ (className x,x)| x <- rs ]) mempty

fromHsTyVar (HsTyVar v) = return v
fromHsTyVar (HsTyExpKind (Located _ t) _) = fromHsTyVar t
fromHsTyVar _ = fail "fromHsTyVar"

makeClassHierarchy :: ClassHierarchy -> KindEnv -> [HsDecl] -> ClassHierarchy
makeClassHierarchy (CH ch _is) kt ds = ans where
    ans =  execWriter (mapM_ f ds) -- Map.fromListWith combineClassRecords [  (className x,x)| x <- execWriter (mapM_ f ds) ]
    f (HsClassDecl sl chead decls) = do
            let qualifiedMethodAssumps = concatMap (aHsTypeSigToAssumps kt . qualifyMethod newClassContext) (filter isHsTypeSig decls)
                newClassContext = [HsAsst className [argName]] -- hsContextToContext [(className, argName)]                                                                                                                                                                   -- TODO
                className = hsClassHead chead
                [Just argName] = map fromHsTyVar (hsClassHeadArgs chead)
            tell $ classHierarchyFromRecords [ClassRecord {
                classArgs,
                classAssocs,
                classAlias = Nothing,
                className = toName ClassName className,
                classSrcLoc = sl,
                classSupers = [ toName ClassName x | HsAsst x _ <- cntxt],
                --classInsts = [ emptyInstance { instHead = i } | i@(_ :=> IsIn n _) <- [], nameName n == className],
                classAssumps = qualifiedMethodAssumps }]
        where
        cntxt = hsClassHeadContext chead
        --HsQualType cntxt tbody = t
        classAssocs = createClassAssocs kt decls
        (_,(_,classArgs')) = chToClassHead kt chead
        classArgs = [ v | ~(TVar v) <- classArgs' ]
--    f decl@(HsClassAliasDecl {}) = trace ("makeClassHierarchy: "++show decl) $ do
--        tell [ClassAliasRecord { className = toName ClassName (hsDeclName decl),
--                                 classArgs = [v | ~(TVar v) <- map (runIdentity . hsTypeToType kt) (hsDeclTypeArgs decl)],
--                                 classSrcLoc = hsDeclSrcLoc decl,
--                                 classSupers = [toName ClassName n | HsAsst n _ <- (hsDeclContext decl)],
--                                 classClasses = [toName ClassName n | HsAsst n _ <- (hsDeclClasses decl)],
--                                 classInsts = [],
--                                 classMethodMap = Map.empty
--                               }]

    f decl@(HsInstDecl {}) = hsInstDeclToInst kt decl >>= \insts -> do
        --crs <- flip mapM [ (cn,i) | i@Inst { instHead = _ :=> IsIn cn _} <- insts] $ \ (x,inst) -> case Map.lookup x ch of
        --    Just cr -> ensureNotDup (srcLoc decl) inst (classInsts cr) >> return [cr { classInsts = mempty }]
        --    Nothing -> return [] -- case Map.lookup x ans of
                -- Just _ -> return []
               --  Nothing -> return [] -- failSl (srcLoc decl) "Invalid Instance"
        tell $ foldl addOneInstanceToHierarchy mempty insts
    f _ = return ()

ensureNotDup :: Monad m => SrcLoc -> Inst -> [Inst] -> m [Inst]
ensureNotDup sl i is = f i is where
    f i (i':is') | instHead i == instHead i' = case instDerived i && instDerived i' of
        True -> return is
        False -> failSl sl $ "Duplicate Instance: " ++ show i ++ "\nPrevious instance declared at " ++ show (instSrcLoc i')
    f i (_:is') = f i is'
    f i [] = return $ i:is

accLen :: Int -> [[a]] -> [(Int, [a])]
accLen width [] = []
accLen width (x:xs) = let newWidth = length x + width in (newWidth, x) : accLen newWidth xs

groupStringsToWidth :: Int -> [String] -> [String]
groupStringsToWidth width ss
   = groupStringsToWidth' width (accLen 0 ss)
   where
   groupStringsToWidth' :: Int -> [(Int,String)] -> [String]
   groupStringsToWidth' width [] = []
   groupStringsToWidth' width xs
      = headString : groupStringsToWidth' width (accLen 0 $ map snd rest)
      where
      (headSegments, rest)
         = case span ((<=width).fst) xs of
              ([], ss)     -> ([head ss], tail ss)
              anythingElse -> anythingElse
      headString = concatMap snd headSegments

showListAndSepInWidth :: (a -> String) -> Int -> String -> [a] -> String
showListAndSepInWidth _ _ _ [] = []
showListAndSepInWidth f width sep things = unlines $ groupStringsToWidth width newThings where
   newThings = (map ((\t -> t ++ sep).f) (init things)) ++ [f (last things)]

pretty  :: PPrint Doc a => a -> String
pretty   = render . pprint

nameOfTyCon :: NameType -> HsType -> Name
nameOfTyCon t (HsTyCon n) = toName t n
nameOfTyCon t (HsTyTuple xs) = nameTuple t (length xs)
nameOfTyCon t (HsTyFun _ _) = tc_Arrow
nameOfTyCon _ t = error $ "nameOfTyCon: " ++ show t

groupEquations :: [HsDecl] -> [(Name, HsDecl)]
groupEquations [] = []
groupEquations (HsTypeDecl {}:ds) = groupEquations ds
groupEquations (d:ds) = (getDeclName d, d) : groupEquations ds

derivableClasses ::  [Name]
derivableClasses = [
    class_Eq,
    class_Ord,
    class_Enum,
    class_Bounded,
    class_Show,
    class_Read,
    class_Ix
    ]

-- can be automatically derived when
-- the class is an enumeration
enumDerivableClasses ::  [Name]
enumDerivableClasses = [
    class_Eq,
    class_Ord,
    class_Enum,
    class_Ix
    ]

-- classes that cannot be derived by the generalized
-- newtype deriving mechanism.
noNewtypeDerivable :: [Name]
noNewtypeDerivable = [
    class_Show,
    class_Read
    ]

-- classes that behave identically to their component when they have a single
-- unary constructor but are not newtypes
unaryPassDerivable :: [Name]
unaryPassDerivable = [
    class_Ix,
    class_Eq,
    class_Ord,
    class_Bounded
    ]
