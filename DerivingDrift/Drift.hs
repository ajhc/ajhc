module DerivingDrift.Drift(driftDerive) where


import HsSyn
import DerivingDrift.DataP
import DerivingDrift.StandardRules
import qualified Data.Map as Map
import FrontEnd.HsParser
import FrontEnd.ParseMonad
import Text.PrettyPrint.HughesPJ(render)
--import DerivingDrift.Pretty(render)
import CharIO
import Char
import qualified FlagDump as FD
import Options

driftDerive :: HsModule -> IO HsModule
driftDerive hsModule = ans where
    ans | null ss = return hsModule
        | otherwise = do
            wdump FD.Derived $ do
                CharIO.print $ hsModuleName hsModule
                mapM_ CharIO.putErrLn ss
            return hsMod'
    hsMod' = hsModule { hsModuleDecls = hsModuleDecls hsModule ++ ndcls }
    --hsMod = case parse (unlines ss) (SrcLoc (show $ hsModuleName hsModule) 1 1) 0 [] of
    hsMod = case runParser parse (unlines ss)  of
        ParseOk _ e -> e
        ParseFailed sl err -> error $ "driftDerive: " ++ show sl ++ err
    ndcls = hsModuleDecls hsMod
    ss = [ n | Just n <- map driftDerive' $ hsModuleDecls hsModule, any (not . isSpace) n ]

driftDerive' :: Monad m => HsDecl -> m String
driftDerive' (HsDataDecl sloc cntxt name args condecls derives) = do
        let d =  toData  name args condecls derives
        xs <- return $  map (derive d . show) derives
        return $ unlines xs
driftDerive' (HsNewTypeDecl sloc cntxt name args condecl derives) = do
        let d =  toData  name args [condecl] derives
        xs <- return $ map (derive d . show) derives
        return $ unlines xs

driftDerive' _ = fail "Nothing to derive"

toData :: HsName -> [HsName] -> [HsConDecl] -> [HsName] -> Data
toData name args cons derives = ans where
    f c = Body { constructor = pp (show $ hsConDeclName c), types = hsConDeclArgs c, labels = lb c }
    pp xs@(x:_) | isAlpha x = xs
    pp xs = '(':xs++")"
    lb HsConDecl {} = []
    lb r = concat [map show xs | (xs,_) <- hsConDeclRecArg r ]
    ans = D { statement = DataStmt, vars = map show args, constraints = [], name = show name,  derives = map show derives, body = map f cons }

rulesMap = Map.fromList [ (t,f) | (t,f,_,_,_) <- standardRules]

derive d wh | Just fn <- Map.lookup wh rulesMap = render $ fn d


