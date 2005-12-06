{-------------------------------------------------------------------------------

        Copyright:              Mark Jones and The Hatchet Team
                                (see file Contributors)

        Module:                 Class

        Description:            Code for manipulating the class hierarchy and
                                qualified types.

                                The main tasks implemented by this module are:
                                        - context reduction
                                        - context spliting
                                        - defaulting
                                        - entailment of class constraints
                                        - class hierarchy representation and
                                          manipulation

        Primary Authors:        Mark Jones, Bernie Pope

        Notes:                  See the files License and License.thih
                                for license information.

                                Large parts of this module were derived from
                                the work of Mark Jones' "Typing Haskell in
                                Haskell", (http://www.cse.ogi.edu/~mpj/thih/)

-------------------------------------------------------------------------------}

-- TODO this, of everything desperatly needs to be rewritten the most.

module Class(
    addClassToHierarchy,
    addInstancesToHierarchy,
    printClassHierarchy,
    instanceToTopDecls,
    entails,
    ClassHierarchy,
    ClassRecord(..),
    reduce,
    split,
    instanceName,
    defaultInstanceName,
    printClassSummary,
    addOneInstanceToHierarchy,
    findClassRecord,
    asksClassRecord,
    classRecords,
    makeClassHierarchy,
    splitReduce,
    topDefaults,
    derivableClasses,
    stdClasses,
    numClasses
    ) where

import Control.Monad.Identity
import Control.Monad.Writer
import Data.Generics
import Data.Monoid
import List((\\), partition)
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJ as PPrint

import Binary
import Doc.PPrint
import GenUtil(snub,concatInter)
import HsSyn
import FrontEnd.KindInfer
import MapBinaryInstance()
import Maybe
import Monad
import Name.Name
import Name.Names
import Name.VConsts
import Representation
import Type
import TypeUtils
import Util.HasSize
import Util.Inst()
import FrontEnd.Utils

--------------------------------------------------------------------------------

-- Instance
type Inst  = Qual Pred

listToFM = Map.fromList

bySuper :: ClassHierarchy -> Pred -> [Pred]
bySuper h p@(IsIn c t)
 = p : concat (map (bySuper h) supers)
   where supers = [ IsIn c' t | c' <- supersOf h c ]

byInst             :: Monad m => Pred -> Inst -> m [Pred]
byInst p (ps :=> h) = do u <- matchPred h p
                         return (map (apply u) ps)

matchPred :: Monad m => Pred -> Pred -> m Subst
matchPred x@(IsIn c t) y@(IsIn c' t')
      | c == c'   = match t t'
      | otherwise = fail $ "Classes do not match: " ++ show (x,y)

reducePred :: Monad m => ClassHierarchy -> Pred -> m [Pred]
reducePred h p@(IsIn c t)
    | Just x <- foldr (|||) Nothing poss = return x
    | otherwise = fail "reducePred"
 where poss = map (byInst p) (instsOf h c)
       Nothing ||| y = y
       Just x  ||| y = Just x

-----------------------------------------------------------------------------

entails :: ClassHierarchy -> [Pred] -> Pred -> Bool
entails h ps p = any (p `elem`) (map (bySuper h) ps) ||
           case reducePred h p of
             Nothing -> False
             Just qs -> all (entails h ps) qs

-----------------------------------------------------------------------------

-- the new class hierarchy


-- classname (superclasses, instances, properly qualified type-sigs of methods)

data ClassRecord = ClassRecord {
    className :: Class,
    classSrcLoc :: SrcLoc,
    classSupers :: [Class],
    classInsts :: [Inst],
    classAssumps :: [Assump],
    classDerives :: [Inst]
    } deriving(Typeable,Data)
    {-! derive: GhcBinary !-}

newClassRecord c = ClassRecord {
    className = c,
    classSrcLoc = bogusASrcLoc,
    classSupers = [],
    classInsts = [],
    classAssumps = [],
    classDerives = []
    }

combineClassRecords cra crb | className cra == className crb = ClassRecord {
    className = className cra,
    classSrcLoc = if classSrcLoc cra == bogusASrcLoc then classSrcLoc crb else classSrcLoc cra,
    classSupers = snub $ classSupers cra ++ classSupers crb,
    classInsts = snub $ classInsts cra ++ classInsts crb,
    classAssumps = snub $ classAssumps cra ++ classAssumps crb,
    classDerives = snub $ classDerives cra ++ classDerives crb
    }

--([Class], [Inst], [Assump])

newtype ClassHierarchy = ClassHierarchy (Map.Map Class ClassRecord)
    deriving (Binary,HasSize)

instance Monoid ClassHierarchy where
    mempty = ClassHierarchy mempty
    mappend (ClassHierarchy a) (ClassHierarchy b) = ClassHierarchy $ Map.unionWith combineClassRecords a b

classRecords (ClassHierarchy ch) = Map.elems ch

findClassRecord (ClassHierarchy ch) cn = case Map.lookup cn ch of
    Nothing -> error $ "findClassRecord: " ++ show cn
    Just n -> n

asksClassRecord (ClassHierarchy ch) cn f = case Map.lookup cn ch of
    Nothing -> error $ "asksClassRecord: " ++ show cn
    Just n -> f n

supersOf :: ClassHierarchy -> Class -> [Class]
supersOf ch c = asksClassRecord ch c classSupers

instsOf :: ClassHierarchy -> Class -> [Inst]
instsOf ch c = asksClassRecord ch c classInsts


showInst :: Inst -> String
showInst = PPrint.render . pprint

showPred :: Pred -> String
showPred (IsIn c t) = show c ++ " " ++ (pretty t)


--makeDeriveInstances :: [Pred] -> Type -> [Class] -> [Inst]
--makeDeriveInstances context t [] = []
--makeDeriveInstances context t (c:cs)
--   | c `elem` derivableClasses
--        = (context :=> IsIn c t) : makeDeriveInstances context t cs
--   | otherwise
--        = error $ "makeDeriveInstances: attempt to make type " ++ pretty t ++
--                  "\nan instance of a non-derivable class " ++ show c

{-
toHsName (x,y) = Qual (Module x) (HsIdent y)
instance ClassNames HsName where
    classEq = toHsName classEq
    classOrd = toHsName classOrd
    classEnum = toHsName classEnum
    classBounded = toHsName classBounded
    classShow = toHsName classShow
    classRead = toHsName classRead
    classIx = toHsName classIx
    classFunctor = toHsName classFunctor
    classMonad = toHsName classMonad
    classNum = toHsName classNum
    classReal = toHsName classReal
    classIntegral = toHsName classIntegral
    classFractional = toHsName classFractional
    classFloating = toHsName classFloating
    classRealFrac = toHsName classRealFrac
    classRealFloat = toHsName classRealFloat
-}


toHsQualType (HsUnQualType t) = HsQualType [] t
toHsQualType qt = qt

addClassToHierarchy :: Monad m =>  KindEnv -> HsDecl -> ClassHierarchy -> m ClassHierarchy
addClassToHierarchy  kt (HsClassDecl _ t decls) (ClassHierarchy h) |   (HsQualType cntxt (HsTyApp (HsTyCon className') (HsTyVar argName')))  <- toHsQualType t = let
   qualifiedMethodAssumps = concatMap (aHsTypeSigToAssumps kt . qualifyMethod newClassContext) (filter isHsTypeSig decls)
   newClassContext = [(className, argName)]
   className = toName ClassName className'
   argName = toName TypeVal argName'
   in return $ ClassHierarchy $ Map.insertWith combineClassRecords  className ClassRecord { classSrcLoc = bogusASrcLoc, className = className, classSupers = map fst (hsContextToContext cntxt), classInsts = [], classDerives = [], classAssumps = qualifiedMethodAssumps } h


addClassToHierarchy  _ _ ch = return ch

--addClassToHierarchy mod kt (HsClassDecl _sloc (HsUnQualType (HsTyApp (HsTyCon className) (HsTyVar argName))) decls) h
--   = addToEnv (className, ([], [], qualifiedMethodAssumps)) h
--   where
--   qualifiedMethodAssumps
--      = concatMap (aHsTypeSigToAssumps kt . qualifyMethod newClassContext) (filter isSigDecl decls)
--   newClassContext
--      = [(className, argName)]
--qualifyMethod cntxt (HsTypeSig sloc names (HsUnQualType t))
--   = HsTypeSig sloc names (HsQualType cntxt t)

qualifyMethod :: Context -> (HsDecl) -> (HsDecl)
qualifyMethod [(c,n)] (HsTypeSig sloc names (HsQualType oc t))
    = HsTypeSig sloc names (HsQualType ((nameName c,n'):oc) t) where
        --n' = fromJust $ applyTU (once_tdTU $ adhocTU failTU f) t
        --f (HsTyVar n') | hsNameToOrig n' == hsNameToOrig n = return n'
        --f (HsTyVar n')  = return n'
        --f _ = mzero
        Just n' = (something (mkQ mzero f)) t
        f (HsTyVar n') | hsNameToOrig n' == hsNameToOrig (nameName n) = return n'
        f _ = mzero



printClassSummary :: ClassHierarchy -> IO ()
printClassSummary (ClassHierarchy h) = mapM_ f $  h' where
    h' = [ (n,runIdentity $ Map.lookup n h) | n <- (map fst [ (cn, classSupers ss) | (cn,ss) <- Map.toList h]) ]
    f (cname, (ClassRecord { classSupers = supers, classInsts = insts, classAssumps = ma})) = do
        putStrLn $ "-- class: " ++ show cname
        unless (null supers) $ putStrLn $ "super classes:" ++ unwords (map show supers)
        unless (null insts) $ putStrLn $ "instances: " ++ (concatInter ", " (map showInst insts))
        putStrLn ""



printClassHierarchy :: ClassHierarchy -> IO ()
printClassHierarchy (ClassHierarchy h)
   = mapM_ printClassDetails $  Map.toList h
   where
   printClassDetails :: (Name, ClassRecord) -> IO ()
   printClassDetails (cname, (ClassRecord { classSupers = supers, classInsts = insts, classAssumps = methodAssumps}))
      = do
            putStrLn "..........."
            putStrLn $ "class: " ++ show cname
            putStr $ "super classes:"
            case supers of
               [] -> putStrLn $ " none"
               _  -> putStrLn $ " " ++ (showListAndSep id " " (map show supers))
            putStr $ "instances:"
            case insts of
               [] -> putStrLn $ " none"
               _  -> putStrLn $ "\n" ++ (showListAndSepInWidth showInst 80 ", " insts)
            putStr $ "method signatures:"
            case methodAssumps of

               [] -> putStrLn $ " none"
               _  -> putStrLn $ "\n" ++
                        (unlines $ map pretty methodAssumps)


            putStr "\n"

{-
genClassHierarchy :: [(HsDecl)] -> ClassHierarchy
genClassHierarchy classes
   = foldl (flip addClassToHierarchy) stdClassHierarchy classes
   where
   -- stdClassHierarchy = classListToHierarchy stdClasses
   stdClassHierarchy = listToFM preludeClasses
-}

--------------------------------------------------------------------------------

addInstancesToHierarchy :: Monad m => KindEnv -> ClassHierarchy -> [HsDecl] -> m ClassHierarchy
addInstancesToHierarchy kt ch decls = do
    insts <- mapM (hsInstDeclToInst kt) decls
    return $ foldl addOneInstanceToHierarchy ch (concat insts)
   --where
   --instances = concatMap (hsInstDeclToInst kt) decls


modifyClassRecord ::  (ClassRecord -> ClassRecord) -> Class -> ClassHierarchy -> ClassHierarchy
modifyClassRecord f c (ClassHierarchy h) = case Map.lookup c h of
           --Nothing -> error $ "modifyClassRecord: " ++ show c
           Nothing -> ClassHierarchy $ Map.insert c (f (newClassRecord c)) h
           Just r -> ClassHierarchy $ Map.insert c (f r) h

addOneInstanceToHierarchy :: ClassHierarchy -> (Bool,Inst) -> ClassHierarchy
addOneInstanceToHierarchy ch (x,inst@(cntxt :=> IsIn className _)) = modifyClassRecord f className ch where
    f c
        | x = c { classInsts = inst:classInsts c, classDerives = inst:classDerives c }
        | otherwise = c { classInsts = inst:classInsts c  }

{-
   = newHierarchy
   where
   newHierarchy
      -- check to make sure the class exists
      -- = case lookupFM ch className of
      = case lookupEnv className ch of
           Nothing
              -> error $ "addInstanceToHierarchy: attempt to add instance decl: " ++ showInst inst ++
                         ", to non-existent class: " ++ show className
           Just _ -> addToCombFM nodeCombiner className newElement ch
   newElement = ([], [inst], [])
   nodeCombiner :: ([HsName], [Inst], [Assump]) -> ([HsName], [Inst], [Assump]) -> ([HsName], [Inst], [Assump])
   nodeCombiner (_, [newInst], _) (supers, oldInsts, oldMethodSigs) = (supers, newInst:oldInsts, oldMethodSigs)


from section 4.3.2 of the Haskell 98 report

instance decls look like:
   instance cx' => C (T u1 ... uk) where { d }

where u_i are simple variables and are distinct

XXX
currently hsInstDeclToInst does not check whether the context of
an instance declaration is legal, for example it allows:

instance (Eq a, Functor a) => Eq (Tree a) where ...
 the kind of Functor, and Eq are different (the Functor is wrong here)

-}

hsInstDeclToInst :: Monad m => KindEnv -> (HsDecl) -> m [(Bool,Inst)]
hsInstDeclToInst kt (HsInstDecl _sloc qType _decls)
   | classKind == argTypeKind
        = return [(False,cntxt :=> IsIn className convertedArgType)]
   | otherwise
        = failSl _sloc $ "hsInstDeclToInst: kind error, attempt to make\n" ++
                  show argType ++ " (with kind " ++
                  show argTypeKind ++ ")\n" ++
                  "an instance of class " ++ show className ++
                  " (with kind " ++ show classKind ++ ")"
   where
   (cntxt, classType, argType)
      = case toHsQualType qType of
           HsQualType context (HsTyApp cType@(HsTyCon _) aType)
              -> (map (hsAsstToPred kt) context, cType, aType)
   {-
      Note:
      kind (Either) = *->*->*
      kind (Either a) = *->*
      kind (Either a b) = *

      the kind of the argument type (argTypeKind) is the remaining
      kind after droping the kinds of the supplied arguments from
      the kind of the type constructor
   -}
   argTypeKind :: Kind
   convertedArgType :: Type
   (argTypeKind, convertedArgType)
      = case argType of
           HsTyTuple args -> (Star, tTTuple $ map toType $ zip args $ repeat Star)
           _anythingElse
              -> let tyConName = nameOfTyCon TypeConstructor tyCon
                     numArgs = (length flatType) - 1
                     flatType = flattenLeftTypeApplication argType
                     flatTyConKind = unfoldKind tyConKind
                     tyConKind = kindOf tyConName kt
                     tyCon = head flatType
                     typeKindPairs = (tyCon, tyConKind) : (zip (tail flatType) flatTyConKind)
                     in (foldr1 Kfun $ drop numArgs flatTyConKind,
                         convType typeKindPairs)
   className = nameOfTyCon ClassName classType
   [classKind] = kindOfClass className kt

-- derive statements
hsInstDeclToInst kt (HsDataDecl _sloc _cntxt tyConName argNames _condecls derives@(_:_))
   = return $ map ((,) True) newInstances
   where
   tyConKind = kindOf (toName TypeConstructor tyConName) kt
   flatTyConKind = unfoldKind tyConKind
   argTypeKind = foldr1 Kfun $ drop (length argNames) flatTyConKind
   argsAsTypeList = map (\n -> HsTyVar n) argNames
   typeKindPairs :: [(HsType, Kind)]
   typeKindPairs = (HsTyCon tyConName, tyConKind) : zip argsAsTypeList flatTyConKind
   convertedType :: Type
   convertedType = convType typeKindPairs
   --newContext = map (hsAsstToPred kt) cntxt
   --newInstances = makeDeriveInstances newContext convertedType derives
   newInstances = mempty

hsInstDeclToInst kt (HsNewTypeDecl _sloc _cntxt tyConName argNames _condecls derives@(_:_))
   = return $ map ((,) True) newInstances
   where
   tyConKind = kindOf (toName TypeConstructor tyConName) kt
   flatTyConKind = unfoldKind tyConKind
   argTypeKind = foldr1 Kfun $ drop (length argNames) flatTyConKind
   argsAsTypeList = map (\n -> HsTyVar n) argNames
   typeKindPairs :: [(HsType, Kind)]
   typeKindPairs = (HsTyCon tyConName, tyConKind) : zip argsAsTypeList flatTyConKind
   convertedType :: Type
   convertedType = convType typeKindPairs
   --newContext = map (hsAsstToPred kt) cntxt
   --newInstances = makeDeriveInstances newContext convertedType derives
   newInstances = mempty

hsInstDeclToInst _ _ = return []

-- the types will only ever be constructors or vars

convType :: [(HsType, Kind)] -> Type
convType tsks
   = foldl1 TAp (map toType tsks)

toType :: (HsType, Kind) -> Type
toType (HsTyCon n, k) = TCon $ Tycon (toName TypeConstructor n) k
toType (HsTyVar n, k) = TVar $ tyvar (toName TypeVal n) k Nothing
toType (HsTyFun x y, Star) = TArrow (toType (x,Star)) (toType (y,Star))
toType x = error $ "toType: " ++ show x

{-
makeDeriveInstances :: [Pred] -> Type -> [Class] -> [Inst]
makeDeriveInstances context t [] = []
makeDeriveInstances context t (c:cs)
   | c `elem` deriveableClasses
        = (context :=> IsIn c t) : makeDeriveInstances context t cs
   | otherwise
        = error $ "makeDeriveInstances: attempt to make type " ++ pretty t ++
                  "\nan instance of a non-deriveable class " ++ c
-}

-- as defined by section 4.3.3 of the haskell report
{-
deriveableClasses :: [Class]
deriveableClasses = ["Eq", "Ord", "Enum", "Bounded", "Show", "Read"]
-}

{-

   converts leftmost type applications into lists

   (((TC v1) v2) v3) => [TC, v1, v2, v3]

-}


--------------------------------------------------------------------------------

-- code for making instance methods into top level decls
-- by adding a (instantiated) type signature from the corresponding class
-- decl
--   className
--      = case qualType of
--           HsQualType _cntxt (HsTyApp (HsTyCon className) _argType) -> className
--           HsUnQualType (HsTyApp (HsTyCon className) _argType) -> className

-- {-




makeDerivation kt ch name args cs ds = ([],[])
makeDerivation kt ch name args cs ds = ([],concatMap f ds) where
    f n
        | n == class_Enum = [cia $  v_toEnum, cia $ v_fromEnum]
        | n == class_Bounded = [cia  ( v_minBound), cia $  v_maxBound]
        | otherwise = error "cannot derive"
        where
        cia = createInstanceAssump kt methodSigs [] n arg
        methodSigs = asksClassRecord ch n classAssumps
    arg = foldr HsTyApp (HsTyCon name) (map HsTyVar args)



instanceToTopDecls :: KindEnv -> ClassHierarchy -> HsDecl -> (([HsDecl],[Assump]))
instanceToTopDecls kt (ClassHierarchy classHierarchy) (HsInstDecl _ qualType methods)
   = unzip $ map (methodToTopDecls kt methodSigs qualType) $ methodGroups where
   HsQualType _ (HsTyApp (HsTyCon className) _) = qualType
   methodGroups = groupEquations methods
   methodSigs = case Map.lookup (toName ClassName className) classHierarchy  of
           Nothing -> error $ "instanceToTopDecls: could not find class " ++ show className ++ "in class hierarchy"
           Just sigs -> classAssumps sigs
instanceToTopDecls kt classHierarchy decl@HsDataDecl {} =
     (makeDerivation kt classHierarchy (hsDeclName decl) (hsDeclArgs decl) (hsDeclCons decl)) (map (toName ClassName) $ hsDeclDerives decl)
instanceToTopDecls kt classHierarchy decl@HsNewTypeDecl {} =
    (makeDerivation kt classHierarchy (hsDeclName decl) (hsDeclArgs decl) [(hsDeclCon decl)]) (map (toName ClassName) $ hsDeclDerives decl)


instanceToTopDecls _ _ _ = mempty



getHsTypeCons (HsTyCon n) = n
getHsTypeCons (HsTyApp a _) = getHsTypeCons a
getHsTypeCons (HsTyFun {}) = nameName (tc_Arrow)
getHsTypeCons (HsTyTuple xs) = toTuple (length xs)
getHsTypeCons x = error $ "getHsTypeCons: " ++ show x


instanceName n t = toName Val $ Qual (Module "Instance@") $ HsIdent ('i':show n ++ "." ++ show t)
defaultInstanceName n = toName Val $ Qual (Module "Instance@") $ HsIdent ('i':show n ++ ".default")

createInstanceAssump :: KindEnv -> [Assump] -> HsContext -> Class -> HsType -> Name -> Assump
createInstanceAssump kt methodSigs cntxt className argType methodName
   = newMethodName :>: instantiatedSig where
    newMethodName = instanceName methodName (getHsTypeCons argType)
    [sigFromClass] = [ s | n :>: s <- methodSigs, n == methodName]
    instantiatedSig = newMethodSig' kt methodName cntxt sigFromClass argType

methodToTopDecls :: KindEnv -> [Assump] -> HsQualType -> (Name, HsDecl) -> (HsDecl,Assump)

methodToTopDecls kt methodSigs (HsQualType cntxt classApp) (methodName, methodDecls)
   = (renamedMethodDecls,newMethodName :>: instantiatedSig) where
    (HsTyApp (HsTyCon className) argType) = classApp
    newMethodName = instanceName methodName (getHsTypeCons argType)
    sigFromClass = case [ s | n :>: s <- methodSigs, n == methodName] of
        [x] -> x
        _ -> error $ "sigFromClass: " ++ show methodSigs ++ " " ++ show  methodName
    --instantiatedSig = newMethodSig' (kiHsQualTypePredPred kt qt) cntxt sigFromClass argType
    instantiatedSig = newMethodSig' kt methodName cntxt sigFromClass argType
     --  = newMethodSig cntxt newMethodName sigFromClass argType
    renamedMethodDecls
       = renameOneDecl newMethodName methodDecls

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



newMethodSig' :: KindEnv -> Name -> HsContext -> Scheme -> HsType -> Scheme
newMethodSig' kt methodName newCntxt qt' instanceType  = newQualType where
   ((IsIn _ classArg:restContext) :=> t) = unQuantify qt'
   -- the assumption is that the context is non-empty and that
   -- the class and variable that we are interested in are at the
   -- front of the old context - the method of inserting instance types into
   -- the class hierarchy should ensure this
   --((className, classArg):restContxt) = cntxt
   foo = "_" ++ (show methodName ++ show (getHsTypeCons instanceType)) ++ "@@"

   --newQualType = runIdentity $ (applyTP $ full_tdTP (adhocTP idTP at)) $ quantify (tv qt) qt
   --at (Tyvar n k) = return $ Tyvar (hsNameIdent_u (hsIdentString_u (++ foo)) n) k
   --qt = (map (aHsAsstToPred kt) newCntxt ++ restContext) :=> (runIdentity $ applyTP (full_tdTP $ adhocTP idTP ct) t)
   --ct n | n == classArg = return $ aHsTypeToType kt instanceType
   --ct n = return n
   newQualType = everywhere (mkT at) $ quantify (tv qt) qt
   at (Tyvar _ n k r) =  tyvar (updateName (++ foo) n) k r
   updateName f n = toName nt (md,f nm) where
        (nt,(md::String,nm)) = fromName n
   qt = (map (hsAsstToPred kt) newCntxt ++ restContext) :=> (everywhere (mkT ct) t)
   ct n | n == classArg =  runIdentity $ hsTypeToType kt instanceType
   ct n =  n

{-
newMethodSig :: HsContext -> HsName -> HsDecl -> HsType -> HsDecl
newMethodSig newCntxt newName (HsTypeSig _sloc methodName (HsQualType cntxt t)) instanceType
   = HsTypeSig bogusASrcLoc [newName] newQualType
   where
   -- the assumption is that the context is non-empty and that
   -- the class and variable that we are interested in are at the
   -- front of the old context - the method of inserting instance types into
   -- the class hierarchy should ensure this
   ((className, classArg):restContxt) = cntxt
   newT = oneTypeReplace (HsTyVar classArg, instanceType) t
   newQualType
      = let finalCntxt = newCntxt++restContxt
           in case finalCntxt of
                 []    -> HsUnQualType newT
                 (_:_) -> HsQualType finalCntxt newT
-- -}

-- collect assumptions of all class methods

classMethodAssumps :: ClassHierarchy -> [Assump]
classMethodAssumps hierarchy = concatMap classAssumps $ classRecords hierarchy

--------------------------------------------------------------------------------

splitReduce :: Monad m => ClassHierarchy -> [Tyvar] -> [Tyvar] -> [Pred] -> m ([Pred], [Pred], [(Tyvar,Type)])

splitReduce h fs gs ps = do
    (ds, rs) <- split h fs ps
    (rs',sub) <- genDefaults h (fs++gs) rs
    return (ds,rs',sub)

-- context reduction
-- This is the 'split' from THIH


reduce :: Monad m => ClassHierarchy -> [Tyvar] -> [Tyvar] -> [Pred] -> m ([Pred], [Pred])

reduce h fs gs ps = do
    (ds, rs) <- split h fs ps
    rs' <-   useDefaults h (fs++gs) rs
    return (ds,rs')

--------------------------------------------------------------------------------

-- context splitting
-- This is equivalant to a 'reduce' then a 'partition' in THIH

split       :: Monad m => ClassHierarchy -> [Tyvar] -> [Pred] -> m ([Pred], [Pred])
split h fs ps  = do
    ps' <- (toHnfs h ps)
    return $ partition (all (`elem` fs) . tv) $ simplify h  $ ps'

toHnfs      :: Monad m => ClassHierarchy -> [Pred] -> m [Pred]
toHnfs h ps =  mapM (toHnf h) ps >>= return . concat

toHnf :: Monad m => ClassHierarchy -> Pred -> m [Pred]
toHnf h p
    | inHnf p = return [p]
    | otherwise =  case reducePred h p of
         Nothing -> fail $ "context reduction, no instance for: "  ++ render (pprint  p)
         Just ps -> toHnfs h ps

inHnf       :: Pred -> Bool
inHnf (IsIn c t) = hnf t
 where hnf (TVar v)  = True
       hnf (TCon tc) = False
       hnf (TAp t _) = hnf t
       hnf (TArrow _t1 _t2) = False
--       hnf (TTuple _args) = False

--simplify          :: ClassHierarchy -> [Pred] -> [Pred] -> [Pred]
--simplify h rs []     = rs
--simplify h rs (p:ps) = simplify h (p:(rs\\qs)) (ps\\qs)
-- where qs       = bySuper h p
--       rs \\ qs = [ r | r<-rs, r `notElem` qs ]

simplify :: ClassHierarchy -> [Pred] -> [Pred]
simplify h ps = loop [] ps where
    loop rs []     = rs
    loop rs (p:ps)
        | entails h (rs ++ ps) p = loop rs ps
        | otherwise = loop (p:rs) ps
--     where qs       = bySuper h p
--           rs \\ qs = [ r | r<-rs, r `notElem` qs ]
-----------------------------------------------------------------------------

-- defaulting ambiguous constraints


-- ambiguities from THIH + call to candidates
ambig :: ClassHierarchy -> [Tyvar] -> [Pred] -> [(Tyvar,[Pred],[Type])]

ambig h vs ps
  = [ (v, qs, defs h v qs) |
         v <- tv ps \\ vs,
         let qs = [ p | p<-ps, v `elem` tv p ] ]

-- 'candidates' from THIH
defs     :: ClassHierarchy -> Tyvar -> [Pred] -> [Type]
defs h v qs = [ t | all ((TVar v)==) ts,
                  all (`elem` stdClasses) cs, -- XXX needs fixing
                  any (`elem` numClasses) cs, -- XXX needs fixing
                  -- False, -- XXX
                  t <- defaults, -- XXX needs fixing
                  and [ entails h [] (IsIn c t) | c <- cs ]]
 where cs = [ c | (IsIn c t) <- qs ]
       ts = [ t | (IsIn c t) <- qs ]

withDefaults     :: Monad m => ClassHierarchy ->  [Tyvar] -> [Pred] -> m [(Tyvar, [Pred], Type)]
withDefaults h vs ps
  | any null tss = fail $ "withDefaults.ambiguity: " ++ (render $ pprint ps) ++ show vs ++ show ps
--  | otherwise = fail $ "Zambiguity: " ++ (render $ pprint ps) ++  show (ps,ps',ams)
  | otherwise    = return $ [ (v,qs,head ts) | (v,qs,ts) <- ams ]
    where ams = ambig h vs ps
          tss = [ ts | (v,qs,ts) <- ams ]

-- Return retained predicates and a defaulting substitution
genDefaults :: Monad m => ClassHierarchy ->  [Tyvar] -> [Pred] -> m ([Pred],[(Tyvar,Type)])
genDefaults h vs ps = do
    ams <- withDefaults h vs ps
    let ps' = [ p | (v,qs,ts) <- ams, p<-qs ]
        vs  = [ (v,t)  | (v,qs,t) <- ams ]
    return (ps \\ ps',  vs)

useDefaults     :: Monad m => ClassHierarchy -> [Tyvar] -> [Pred] -> m [Pred]
useDefaults h vs ps
  | any null tss = fail $ "useDefaults.ambiguity: " ++ (render $ pprint ps) ++  show ps
  | otherwise = fail $ "Zambiguity: " ++ (render $ pprint ps) ++  show (ps,ps',ams)
  | otherwise    = return $ ps \\ ps'
    where ams = ambig h vs ps
          tss = [ ts | (v,qs,ts) <- ams ]
          ps' = [ p | (v,qs,ts) <- ams, p<-qs ]

topDefaults     :: Monad m => ClassHierarchy -> [Pred] -> m Subst
topDefaults h ps
  | any null tss = fail $ "topDefaults: ambiguity " ++ (render $ pprint ps)
  | otherwise    = return $ listToFM (zip vs (map head tss))
    where ams = ambig h [] ps
          tss = [ ts | (v,qs,ts) <- ams ]
          vs  = [ v  | (v,qs,ts) <- ams ]

defaults    :: [Type]
defaults     = map (\name -> TCon (Tycon name Star))
                   [tc_Integer, tc_Double]



failSl sl m = fail $ show sl ++ ": " ++ m

classHierarchyFromRecords rs =  ClassHierarchy $ Map.fromListWith combineClassRecords [  (className x,x)| x <- rs ]

-- I love tying el knot.
makeClassHierarchy :: Monad m => ClassHierarchy -> KindEnv -> [HsDecl] -> m ClassHierarchy
makeClassHierarchy (ClassHierarchy ch) kt ds = return (ClassHierarchy ans) where
    ans =  Map.fromListWith combineClassRecords [  (className x,x)| x <- execWriter (mapM_ f ds) ]
    f (HsClassDecl sl t decls)
        | HsTyApp (HsTyCon className) (HsTyVar argName)  <- tbody = do
            let qualifiedMethodAssumps = concatMap (aHsTypeSigToAssumps kt . qualifyMethod newClassContext) (filter isHsTypeSig decls)
                newClassContext = hsContextToContext [(className, argName)]
            tell [ClassRecord { className = toName ClassName className, classSrcLoc = sl, classSupers = map fst $ hsContextToContext cntxt, classInsts = [], classDerives = [], classAssumps = qualifiedMethodAssumps }]
        | otherwise = failSl sl "Invalid Class declaration."
        where
        HsQualType cntxt tbody = toHsQualType t
    f decl = hsInstDeclToInst kt decl >>= \insts -> do
        crs <- flip mapM [ (cn,i) | (_,i@(_ :=> IsIn cn _)) <- insts] $ \ (x,inst) -> case Map.lookup x ch of
            Just cr -> ensureNotDup (srcLoc decl) inst (classInsts cr) >> return [cr { classInsts = mempty }]
            Nothing -> return [] -- case Map.lookup x ans of
                -- Just _ -> return []
               --  Nothing -> return [] -- failSl (srcLoc decl) "Invalid Instance"
        case foldl addOneInstanceToHierarchy (classHierarchyFromRecords (concat crs)) insts of
                ClassHierarchy ch -> tell $ Map.elems ch
    f _ = return ()


ensureNotDup :: Monad m => SrcLoc -> Inst -> [Inst] -> m ()
ensureNotDup sl i is | i `elem` is = failSl sl $ "Duplicate Instance: " ++ show i
                     | otherwise = return ()

-- takes a list of things and puts a seperator string after each elem
-- except the last, first arg is a function to convert the things into
-- strings
showListAndSep :: (a -> String) -> String -> [a] -> String
showListAndSep f sep [] = []
showListAndSep f sep [s] = f s
showListAndSep f sep (s:ss) = f s ++ sep ++ showListAndSep f sep ss

accLen :: Int -> [[a]] -> [(Int, [a])]
accLen width [] = []
accLen width (x:xs)
   = let newWidth
           = length x + width
     in (newWidth, x) : accLen newWidth xs

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
showListAndSepInWidth f width sep things
   = unlines $ groupStringsToWidth width newThings
   where
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
groupEquations (d:ds) = (getDeclName d, d) : groupEquations ds



derivableClasses,numClasses,stdClasses ::  [Name]

stdClasses = [
    class_Eq,
    class_Ord,
    class_Enum,
    class_Bounded,
    class_Show,
    class_Read,
    class_Ix,
    class_Functor,
    class_Monad,
    class_Num ,
    class_Real,
    class_Integral,
    class_Fractional,
    class_Floating,
    class_RealFrac,
    class_RealFloat
    ]

numClasses = [
    class_Num ,
    class_Real,
    class_Integral,
    class_Fractional,
    class_Floating,
    class_RealFrac,
    class_RealFloat
    ]


derivableClasses = [
    class_Eq,
    class_Ord,
    class_Enum,
    class_Bounded,
    class_Show,
    class_Read
    ]
