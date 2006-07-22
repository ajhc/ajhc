module DerivingDrift.Drift(driftDerive) where

import Char
import List

import CharIO
import DerivingDrift.DataP
import DerivingDrift.StandardRules
import FrontEnd.HsParser
import FrontEnd.ParseMonad
import HsSyn
import Name.Name
import Name.Names
import Options
import Text.PrettyPrint.HughesPJ(render)
import qualified Data.Map as Map
import qualified FlagDump as FD

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

enumDontDerive :: [HsName]
enumDontDerive = [nameName class_Eq,nameName class_Ord,nameName class_Enum]

driftDerive' :: Monad m => HsDecl -> m String
driftDerive' (HsDataDecl sloc cntxt name args condecls derives) = do
        let d =  toData  name args condecls derives
            isEnum = length condecls > 1 && null (concatMap hsConDeclArgs condecls)
        xs <- return $  map (derive d . show) derives -- (if isEnum then derives List.\\ enumDontDerive else derives )
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
            | otherwise                         =
  error ("derive: Tried to use non-existing rule "++wh++" for "++name d)

