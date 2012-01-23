module DerivingDrift.Drift(driftDerive) where

import Data.Char
import qualified Data.Map as Map

import DerivingDrift.DataP
import DerivingDrift.StandardRules
import FrontEnd.Class
import FrontEnd.HsParser
import FrontEnd.HsSyn
import FrontEnd.ParseMonad
import Name.Name
import Options
import Text.PrettyPrint.HughesPJ(render)
import Util.Gen
import qualified FlagDump as FD

driftDerive :: HsModule -> IO HsModule
driftDerive hsModule = ans where
    ans | null ss = return hsModule
        | otherwise = do
            wdump FD.Derived $ do
                print $ hsModuleName hsModule
                mapM_ putErrLn ss
            return hsMod'
    hsMod' = hsModule { hsModuleDecls = hsModuleDecls hsModule ++ ndcls }
    --hsMod = case parse (unlines ss) (SrcLoc (show $ hsModuleName hsModule) 1 1) 0 [] of
    hsMod = case snd $ runParser parse (unlines ss)  of
        ParseOk e -> e
        ParseFailed sl err -> error $ "driftDerive: " ++ show sl ++ err
    ndcls = hsModuleDecls hsMod
    ss = [ n | Just n <- map driftDerive' $ hsModuleDecls hsModule, any (not . isSpace) n ]

driftDerive' :: Monad m => HsDecl -> m String
driftDerive' HsDataDecl { hsDeclName = name, hsDeclArgs = args, hsDeclCons = condecls, hsDeclDerives = derives } = do
        let d = toData  name args condecls derives
            isEnum = length condecls > 1 && null (concatMap hsConDeclArgs condecls)
        xs <- return $  map (derive isEnum d) derives
        return $ unlines xs
driftDerive' (HsNewTypeDecl sloc cntxt name args condecl derives) = do
        let d =  toData  name args [condecl] derives
        xs <- return $ map (derive False d) derives
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

derive True d (toName ClassName -> wh) | wh `elem` enumDerivableClasses ++ map toUnqualified enumDerivableClasses = "-- generated instance  " ++ show wh ++ " " ++ name d
derive _ d wh | Just fn <- Map.lookup (toName ClassName wh) (Map.mapKeys (nameName . toUnqualified) standardRules) = render $ fn d
              | otherwise  = error ("derive: Tried to use non-existing rule "++show wh++" for "++name d)
