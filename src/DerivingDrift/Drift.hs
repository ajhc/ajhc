module DerivingDrift.Drift(driftDerive,driftResolvedNames) where

import Data.Char
import Deriving.Derive
import Deriving.Ix
import Deriving.Ord
import Deriving.Text
import DerivingDrift.DataP
import DerivingDrift.StandardRules
import FrontEnd.Class
import FrontEnd.HsSyn
import FrontEnd.Syn.Q(runQ)
import FrontEnd.Warning
import Name.Names
import Text.PrettyPrint.HughesPJ(render)
import Util.Std
import qualified Data.Map as Map
import qualified Data.Map as Map
import qualified Deriving.Type as D
import qualified FrontEnd.Lex.Parse as NP

declIsEnum HsDataDecl { .. } =
            length hsDeclCons > 1 && null (concatMap hsConDeclArgs hsDeclCons)
declIsEnum d = False

driftDerive :: MonadWarn m => HsModule -> m [HsDecl]
driftDerive mod@HsModule { .. } = do
    (nds,lds) <- derivingDerive mod
    let ss = unlines [ n | n <- map f lds, any (not . isSpace) n ]
        f D.Derive { deriveData = Just d, .. } =
            derive isEnum (unrenameTyVars $ dataDtoData d) (hsClassHead deriveHead) where
                isEnum = length (D.body d) > 1 && all null (map D.types (D.body d))
        f _ = ""
    pdecs <- if null ss then return [] else do
        res <- NP.parseM hsModuleOpt ("derived:" ++ show hsModuleName) ss
        case res of
            Just HsModule { hsModuleDecls } -> return  hsModuleDecls
            Nothing -> return []
    return $ pdecs ++ nds

driftDerive' :: Monad m => HsDecl -> m String
driftDerive' decl@HsDataDecl { .. } = do
        let d = unrenameTyVars $ toData  hsDeclName hsDeclArgs hsDeclCons hsDeclDerives
            isEnum = declIsEnum decl
        xs <- return $  map (derive isEnum d) hsDeclDerives
        return $ unlines xs
{-
driftDerive :: MonadWarn m => HsModule -> m [HsDecl]
driftDerive HsModule { .. } = do
    let f d@HsDataDecl { .. } = do mapM g hsDeclDerives where
            g n | n == class_Eq, not isEnum = return ([nds deriveEq])
                | n == class_Ord, not isEnum = return ([nds deriveOrd])
                | n == class_Bounded = return ([nds deriveBounded])
                | n == class_Show = return ([nds deriveShow])
                where
                nds dfunc = runQ $ dfunc hsDeclSrcLoc hsModuleName $
                    toDataD  hsDeclName hsDeclArgs hsDeclCons hsDeclDerives
            g n = return []
            isEnum = declIsEnum d
        f d = return []
    ds <- mapM f hsModuleDecls

    let instDecls =concat $ concat ds
        newDecls = map (hsDeclDerives_u (\x -> x \\ [class_Eq, class_Ord, class_Bounded, class_Show])) hsModuleDecls
        hsDeclDerives_u fn d = f d where
            f d@HsDataDecl { .. } | not $ declIsEnum d = HsDataDecl { hsDeclDerives = fn hsDeclDerives, .. }
            f d@HsDataDecl { .. }  = HsDataDecl { hsDeclDerives = fn hsDeclDerives \\ [class_Bounded, class_Show], .. }
            f d = d

    let ss = unlines [ n | Just n <- map driftDerive' $ newDecls, any (not . isSpace) n ]
    pdecs <- if null ss then return [] else do
        res <- NP.parseM hsModuleOpt ("derived:" ++ show hsModuleName) ss
        case res of
            Just HsModule { hsModuleDecls } -> return  hsModuleDecls
            Nothing -> return []
    return $ pdecs ++ instDecls
    -}

{-
driftDerive' :: Monad m => HsDecl -> m String
driftDerive' decl@HsDataDecl { .. } = do
        let d = unrenameTyVars $ toData  hsDeclName hsDeclArgs hsDeclCons hsDeclDerives
            isEnum = declIsEnum decl
        xs <- return $  map (derive isEnum d) hsDeclDerives
        return $ unlines xs

driftDerive' _ = fail "Nothing to derive"
-}

unrenameTyVars :: Data -> Data
unrenameTyVars d = d{
    vars = map (m Map.!) (vars d),
    constraints = map (\(c,v) -> (c, m Map.! v)) (constraints d)
  }
 where m = Map.fromList $ zip (vars d) tyVars
       tyVars = map (('a':) . show) [1::Int ..]

derive True d wh | wh `elem` enumDerivableClasses ++ map toUnqualified enumDerivableClasses = "-- generated instance  " ++ show wh ++ " " ++ getIdent (name d)
derive _ d wh | Just fn <- Map.lookup wh standardRules = render $ fn d
              | Just _  <- Map.lookup (show wh) shortRuleNames = error (msg ++ " " ++ show wh ++ " not in scope.")
              | wh == class_Ix = ""
              | otherwise  = error msg
  where msg = "Can't make a derived instance '" ++ show wh ++ " " ++ getIdent (name d) ++ "'."

shortRuleNames = Map.mapKeys getIdent standardRules

cNameToString n = pp $ getIdent n where
    pp xs@(x:_) | isAlpha x = xs
    pp xs = '(':xs++")"

toData :: HsName -> [HsName] -> [HsConDecl] -> [HsName] -> Data
toData name args cons derives = ans where
    f c = Body { constructor = cNameToString $ hsConDeclName c, types = hsConDeclArgs c, labels = lb c }
    lb HsConDecl {} = []
    lb  r = concatMap fst (hsConDeclRecArg r)
    ans = D { statement = DataStmt, vars = map show args, constraints = [], name = name,  derives = map show derives, body = map f cons }

dataDtoData :: D.Data -> Data
dataDtoData D.D { .. } = runIdentity $ do
    let constraints = []
        f D.Body { .. } = do
            constructor <- return $ cNameToString constructor
            return Body { .. }
    vars <- return $ map show vars
    body <- mapM f body
    derives <- return $ map show derives
    let statement = DataStmt
    return D { .. }
