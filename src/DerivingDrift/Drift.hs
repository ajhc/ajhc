module DerivingDrift.Drift(driftDerive,driftResolvedNames) where

import Data.Char
import qualified Data.Map as Map

import FrontEnd.Warning
import DerivingDrift.DataP
import DerivingDrift.StandardRules
import FrontEnd.Class
import FrontEnd.HsSyn
import Name.Name
import Text.PrettyPrint.HughesPJ(render)
import qualified FrontEnd.Lex.Parse as NP

driftDerive :: MonadWarn m => HsModule -> m [HsDecl]
driftDerive hsModule@HsModule { hsModuleOpt, hsModuleName } = do
    let ss = unlines [ n | Just n <- map driftDerive' $ hsModuleDecls hsModule, any (not . isSpace) n ]
    if null ss then return [] else do
        res <- NP.parseM hsModuleOpt ("derived:" ++ show hsModuleName) ss
        case res of
            Just hs -> return $ hsModuleDecls hs
            Nothing -> return $ []

driftDerive' :: Monad m => HsDecl -> m String
driftDerive' HsDataDecl { hsDeclName = name, hsDeclArgs = args, hsDeclCons = condecls, hsDeclDerives = derives } = do
        let d = unrenameTyVars $ toData  name args condecls derives
            isEnum = length condecls > 1 && null (concatMap hsConDeclArgs condecls)
        xs <- return $  map (derive isEnum d) derives
        return $ unlines xs
--driftDerive' (HsNewTypeDecl sloc cntxt name args condecl derives) = do
--        let d =  unrenameTyVars $ toData  name args [condecl] derives
--        xs <- return $ map (derive False d) derives
--        return $ unlines xs

driftDerive' _ = fail "Nothing to derive"

unrenameTyVars :: Data -> Data
unrenameTyVars d = d{
    vars = map (m Map.!) (vars d),
    constraints = map (\(c,v) -> (c, m Map.! v)) (constraints d)
  }
 where m = Map.fromList $ zip (vars d) tyVars
       tyVars = map (('a':) . show) [1::Int ..]

toData :: HsName -> [HsName] -> [HsConDecl] -> [HsName] -> Data
toData name args cons derives = ans where
    f c = Body { constructor = pp (getIdent $ hsConDeclName c), types = hsConDeclArgs c, labels = lb c }
    pp xs@(x:_) | isAlpha x = xs
    pp xs = '(':xs++")"
    lb HsConDecl {} = []
    lb  r = concatMap fst (hsConDeclRecArg r)
    ans = D { statement = DataStmt, vars = map show args, constraints = [], name = name,  derives = map show derives, body = map f cons }

derive True d wh | wh `elem` enumDerivableClasses ++ map toUnqualified enumDerivableClasses = "-- generated instance  " ++ show wh ++ " " ++ getIdent (name d)
derive _ d wh | Just fn <- Map.lookup wh standardRules = render $ fn d
              | Just _  <- Map.lookup (show wh) shortRuleNames = error (msg ++ " " ++ show wh ++ " not in scope.")
              | otherwise  = error msg
  where msg = "Can't make a derived instance '" ++ show wh ++ " " ++ getIdent (name d) ++ "'."

shortRuleNames = Map.mapKeys getIdent standardRules
