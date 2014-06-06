module DerivingDrift.Drift(driftDerive,driftResolvedNames) where

import Data.Char
import DerivingDrift.DataP
import DerivingDrift.StandardRules
import FrontEnd.Class
import FrontEnd.HsSyn
import FrontEnd.Warning
import Name.Names
import Text.PrettyPrint.HughesPJ(render)
import Util.Std
import qualified Data.Map as Map
import qualified Deriving.Type as D
import qualified FrontEnd.Lex.Parse as NP

driftDerive :: MonadWarn m => HsModule -> [D.Derive] -> m [HsDecl]
driftDerive mod@HsModule { .. } lds = do
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
    return $ pdecs

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
              | otherwise  = error msg
  where msg = "Can't make a derived instance '" ++ show wh ++ " " ++ getIdent (name d) ++ "'."

shortRuleNames = Map.mapKeys getIdent standardRules

cNameToString n = pp $ getIdent n where
    pp xs@(x:_) | isAlpha x = xs
    pp xs = '(':xs++")"

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
