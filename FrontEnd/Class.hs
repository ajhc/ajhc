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

module FrontEnd.Class(
    printClassHierarchy,
    instanceToTopDecls,
    ClassHierarchy,
    ClassRecord(..),
    instanceName,
    defaultInstanceName,
    printClassSummary,
    findClassRecord,
    asksClassRecord,
    classRecords,
    makeClassHierarchy,
    derivableClasses,
    stdClasses,
    Inst(..),
    numClasses
    ) where

import Control.Monad.Identity
import Control.Monad.Writer
import Data.Generics
import Data.Monoid
import List((\\), partition)
import Text.PrettyPrint.HughesPJ as PPrint
import qualified Data.Map as Map

import Binary
import Doc.PPrint
import FrontEnd.KindInfer
import FrontEnd.SrcLoc
import FrontEnd.Tc.Type
import FrontEnd.Utils
import HsSyn
import MapBinaryInstance()
import Maybe
import Monad
import Name.Name
import Name.Names
import Name.VConsts
import Options
import PrimitiveOperators(primitiveInsts)
import Representation
import Type
import Util.HasSize
import Util.Gen
import Util.Inst()
import qualified FlagOpts as FO

--------------------------------------------------------------------------------

type Assump = (Name,Sigma)

newtype Inst = Inst {
    instHead :: Qual Pred
    } deriving(Typeable,Data,Eq,Ord,PPrint Doc,Show)
    {-! derive: GhcBinary !-}

emptyInstance = Inst { instHead = error "emptyInstance" }

data ClassRecord = ClassRecord {
    className :: Class,
    classSrcLoc :: SrcLoc,
    classSupers :: [Class],
    classInsts :: [Inst],
    classAssumps :: [(Name,Sigma)],
    classAssocs :: [((Name,Kind),[(Name,Kind)],Maybe Sigma)],
    classDerives :: [Inst]
    } deriving(Typeable,Data)
    {-! derive: GhcBinary !-}

newClassRecord c = ClassRecord {
    className = c,
    classSrcLoc = bogusASrcLoc,
    classSupers = [],
    classInsts = [],
    classAssumps = [],
    classAssocs = [],
    classDerives = []
    }

combineClassRecords cra crb | className cra == className crb = ClassRecord {
    className = className cra,
    classSrcLoc = if classSrcLoc cra == bogusASrcLoc then classSrcLoc crb else classSrcLoc cra,
    classSupers = snub $ classSupers cra ++ classSupers crb,
    classInsts = snub $ classInsts cra ++ classInsts crb,
    classAssumps = snubFst $ classAssumps cra ++ classAssumps crb,
    classAssocs = snubUnder fst3 $ classAssocs cra ++ classAssocs crb,
    classDerives = snub $ classDerives cra ++ classDerives crb
    }

fst3 (x,_,_) = x

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

showInst :: Inst -> String
showInst = PPrint.render . pprint

showPred :: Pred -> String
showPred (IsIn c t) = show c ++ " " ++ (pretty t)



toHsQualType qt = qt

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
printClassSummary (ClassHierarchy h) = mapM_ f $  h' where
    h' = [ (n,runIdentity $ Map.lookup n h) | n <- (map fst [ (cn, classSupers ss) | (cn,ss) <- Map.toList h]) ]
    f (cname, (ClassRecord { classSupers = supers, classInsts = insts, classAssumps = ma})) = do
        putStrLn $ "-- class: " ++ show cname
        unless (null supers) $ putStrLn $ "super classes:" ++ unwords (map show supers)
        unless (null insts) $ putStrLn $ "instances: " ++ (concatInter ", " (map showInst insts))
        putStrLn ""



printClassHierarchy :: ClassHierarchy -> IO ()
printClassHierarchy (ClassHierarchy h) = mapM_ printClassDetails $  Map.toList h where
    printClassDetails :: (Name, ClassRecord) -> IO ()
    printClassDetails (cname, (ClassRecord { classSupers = supers, classInsts = insts, classAssumps = methodAssumps, classAssocs = classAssocs})) = do
        putStrLn "..........."
        putStrLn $ "class: " ++ show cname
        putStr $ "super classes:"
        pnone supers $ do putStrLn $ " " ++ (concatInter " " (map show supers))
        putStr $ "instances:"
        pnone insts $  putStr $ "\n" ++ (showListAndSepInWidth showInst 80 ", " insts)
        putStr $ "method signatures:"
        pnone methodAssumps $ putStr $ "\n" ++ (unlines $ map pretty methodAssumps)
        putStr $ "associated types:"
        pnone classAssocs $  putStrLn $ "\n" ++ (unlines $ map (render . passoc) classAssocs)
        putStr "\n"
    pnone [] f = putStrLn " none"
    pnone xs f = f
    passoc (nk,as,mt) = text "type" <+> pb nk <+> hsep (map pb as) <> case mt of
        Nothing -> empty
        Just s -> text " = " <> pprint s
    pb (n,Star) = pprint n
    pb (n,k) = parens (pprint n <+> text "::" <+> pprint k)



--------------------------------------------------------------------------------



modifyClassRecord ::  (ClassRecord -> ClassRecord) -> Class -> ClassHierarchy -> ClassHierarchy
modifyClassRecord f c (ClassHierarchy h) = case Map.lookup c h of
           --Nothing -> error $ "modifyClassRecord: " ++ show c
           Nothing -> ClassHierarchy $ Map.insert c (f (newClassRecord c)) h
           Just r -> ClassHierarchy $ Map.insert c (f r) h

addOneInstanceToHierarchy :: ClassHierarchy -> (Bool,Inst) -> ClassHierarchy
addOneInstanceToHierarchy ch (x,inst@Inst { instHead = cntxt :=> IsIn className _ }) = modifyClassRecord f className ch where
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
        = return [(False,emptyInstance { instHead = cntxt :=> IsIn className convertedArgType })]
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
toType (HsTyVar n, k) = TVar $ tyvar (toName TypeVal n) k
toType (HsTyFun x y, Star) = TArrow (toType (x,Star)) (toType (y,Star))
toType x = error $ "toType: " ++ show x

flattenLeftTypeApplication :: HsType -> [HsType]
flattenLeftTypeApplication t
   = flatTypeAcc t []
   where
   flatTypeAcc (HsTyApp t1 t2) acc
      = flatTypeAcc t1 (t2:acc)
   flatTypeAcc nonTypApp acc
      = nonTypApp:acc

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



qtToClassHead :: KindEnv -> HsQualType -> ([Pred],(Name,[Type]))
qtToClassHead kt (HsQualType cntx (HsTyApp (HsTyCon className) ty)) = (map (hsAsstToPred kt) cntx,(toName ClassName className,[runIdentity $ hsTypeToType kt ty]))

createClassAssocs kt decls = [ (ct TypeConstructor n,map (ct TypeVal) as,ctype t)| HsTypeDecl { hsDeclName = n, hsDeclArgs = as, hsDeclType = t } <- decls ] where
    ct nameType n = let nn = toName nameType n in (nn,kindOf nn kt)
    ctype HsTyAssoc = Nothing
    ctype t = Just $ runIdentity $ hsTypeToType kt t

instanceToTopDecls :: KindEnv -> ClassHierarchy -> HsDecl -> (([HsDecl],[Assump]))
instanceToTopDecls kt (ClassHierarchy classHierarchy) (HsInstDecl _ qualType methods)
    = unzip $ map (methodToTopDecls kt cacntxt crecord methodSigs qualType) $ methodGroups where
    methodGroups = groupEquations methods
    cacntxt = [ IsEq (TAp (TCon (Tycon n k)) th) v | ((n,k),[_],~(Just v)) <- createClassAssocs kt methods]
    (_,(className,[th])) = qtToClassHead kt qualType
    crecord = case Map.lookup className classHierarchy  of
        Nothing -> error $ "instanceToTopDecls: could not find class " ++ show className ++ "in class hierarchy"
        Just crecord -> crecord
    methodSigs = classAssumps crecord
instanceToTopDecls kt classHierarchy decl@HsDataDecl {} =
     (makeDerivation kt classHierarchy (hsDeclName decl) (hsDeclArgs decl) (hsDeclCons decl)) (map (toName ClassName) $ hsDeclDerives decl)
instanceToTopDecls kt classHierarchy decl@HsNewTypeDecl {} =
    (makeDerivation kt classHierarchy (hsDeclName decl) (hsDeclArgs decl) [(hsDeclCon decl)]) (map (toName ClassName) $ hsDeclDerives decl)
instanceToTopDecls kt (ClassHierarchy classHierarchy) (HsClassDecl _ qualType methods)
   = unzip $ map (defaultMethodToTopDecls kt methodSigs qualType) $ methodGroups where
   HsQualType _ (HsTyApp (HsTyCon className) _) = qualType
   methodGroups = groupEquations (filter (\x -> isHsPatBind x || isHsFunBind x)  methods)
   methodSigs = case Map.lookup (toName ClassName className) classHierarchy  of
           Nothing -> error $ "defaultInstanceToTopDecls: could not find class " ++ show className ++ "in class hierarchy"
           Just sigs -> classAssumps sigs


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
   = (newMethodName,instantiatedSig) where
    newMethodName = instanceName methodName (getHsTypeCons argType)
    [sigFromClass] = [ s | (n,s) <- methodSigs, n == methodName]
    instantiatedSig = newMethodSig' kt methodName (map (hsAsstToPred kt) cntxt) sigFromClass argType

methodToTopDecls :: KindEnv -> [Pred] -> ClassRecord -> [Assump] -> HsQualType -> (Name, HsDecl) -> (HsDecl,Assump)

methodToTopDecls kt preds crecord methodSigs qt@(HsQualType cntxt classApp) (methodName, methodDecls)
   = (renamedMethodDecls,(newMethodName, instantiatedSig)) where
    (HsTyApp (HsTyCon className) argType) = classApp
--    (cntxt,(className,[argType])) = qtToClassHead qt
    newMethodName = instanceName methodName (getHsTypeCons argType)
    sigFromClass = case [ s | (n, s) <- methodSigs, n == methodName] of
        [x] -> x
        _ -> error $ "sigFromClass: " ++ pprint methodSigs ++ " " ++ show  methodName
    instantiatedSig = newMethodSig' kt methodName (preds ++ map (hsAsstToPred kt) cntxt) sigFromClass argType
    renamedMethodDecls = renameOneDecl newMethodName methodDecls

defaultMethodToTopDecls :: KindEnv -> [Assump] -> HsQualType -> (Name, HsDecl) -> (HsDecl,Assump)

defaultMethodToTopDecls kt methodSigs (HsQualType cntxt classApp) (methodName, methodDecls)
   = (renamedMethodDecls,(newMethodName,sigFromClass)) where
    (HsTyApp (HsTyCon className) _) = classApp
    newMethodName = defaultInstanceName methodName
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



newMethodSig' :: KindEnv -> Name -> [Pred] -> Sigma -> HsType -> Sigma
newMethodSig' kt methodName newCntxt qt' instanceType  = newQualType where
   TForAll _ ((IsIn _ classArg:restContext) :=> t) = qt'
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
   newQualType = everywhere (mkT at) $ tForAll (tv qt) qt
   at (Tyvar _ n k) =  tyvar (updateName (++ foo) n) k
   updateName f n = toName nt (md,f nm) where
        (nt,(md::String,nm)) = fromName n
   qt = (newCntxt ++ restContext) :=> (everywhere (mkT ct) t)
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


failSl sl m = fail $ show sl ++ ": " ++ m

classHierarchyFromRecords rs =  ClassHierarchy $ Map.fromListWith combineClassRecords [  (className x,x)| x <- rs ]

-- I love tying el knot.
makeClassHierarchy :: Monad m => ClassHierarchy -> KindEnv -> [HsDecl] -> m ClassHierarchy
makeClassHierarchy (ClassHierarchy ch) kt ds = return (ClassHierarchy ans) where
    ans =  Map.fromListWith combineClassRecords [  (className x,x)| x <- execWriter (mapM_ f ds) ]
    f (HsClassDecl sl t decls)
        | HsTyApp (HsTyCon className) (HsTyVar argName)  <- tbody = do
            let qualifiedMethodAssumps = concatMap (aHsTypeSigToAssumps kt . qualifyMethod newClassContext) (filter isHsTypeSig decls)
                newClassContext = [HsAsst className [argName]] -- hsContextToContext [(className, argName)]
            tell [ClassRecord { classAssocs = classAssocs, className = toName ClassName className, classSrcLoc = sl, classSupers = [ toName ClassName x | HsAsst x _ <- cntxt], classInsts = [ emptyInstance { instHead = i } | i@(_ :=> IsIn n _) <- primitiveInsts, nameName n == className], classDerives = [], classAssumps = qualifiedMethodAssumps }]

        | otherwise = failSl sl "Invalid Class declaration."
        where
        HsQualType cntxt tbody = toHsQualType t
        classAssocs = [ (ct TypeConstructor n,map (ct TypeVal) as,ctype t)| HsTypeDecl { hsDeclName = n, hsDeclArgs = as, hsDeclType = t } <- decls ] where
            ct nameType n = let nn = toName nameType n in (nn,kindOf nn kt)
            ctype HsTyAssoc = Nothing
            ctype t = Just $ runIdentity $ hsTypeToType kt t
    f decl = hsInstDeclToInst kt decl >>= \insts -> do
        crs <- flip mapM [ (cn,i) | (_,i@Inst { instHead = _ :=> IsIn cn _}) <- insts] $ \ (x,inst) -> case Map.lookup x ch of
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
