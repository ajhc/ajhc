module DerivingDrift.Drift(driftDerive) where

import Char
import List
import Control.Monad.Identity
import qualified Data.Traversable as T

import CharIO
import DerivingDrift.DataP
import DerivingDrift.StandardRules
import FrontEnd.HsParser
import FrontEnd.ParseMonad
import FrontEnd.HsSyn
import Name.Name
import Name.Names
import Name.VConsts
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

enumDontDerive :: [(HsName,[HsName])]
enumDontDerive = [
    (f class_Eq, [func_equals fns]),
    (f class_Ord, [func_geq fns, func_leq fns, func_lt fns, func_gt fns]),
    (f class_Enum, [func_toEnum fns,func_fromEnum fns])
    ]  where
        Identity fns = T.mapM (return . f) sFuncNames
        f n = nameName (toUnqualified n)



driftDerive' :: Monad m => HsDecl -> m String
driftDerive' HsDataDecl { hsDeclName = name, hsDeclArgs = args, hsDeclCons = condecls, hsDeclDerives = derives } = do
        let d =  toData  name args condecls derives
            isEnum = length condecls > 1 && null (concatMap hsConDeclArgs condecls)
        xs <- return $  map (derive isEnum d) derives -- (if isEnum then derives List.\\ enumDontDerive else derives )
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


derive True d wh | Just fns <- lookup wh enumDontDerive = inst fns where
    dummy = "{- This is a dummy instance, it will be rewritten internally -}\n"
    inst fns = dummy ++ "instance " ++ show wh ++ " " ++ name d ++ " where\n" ++ concat (intersperse "\n" (map f fns))
    f n = "    " ++ g (show n) ++ " = " ++ g (show n)
    g (c:cs) | c == '_' || c == '\'' || isAlpha c = c:cs
    g x = "(" ++ x ++ ")"

derive _ d wh | Just fn <- Map.lookup wh (Map.mapKeys (nameName . toUnqualified) standardRules) = render $ fn d
              | otherwise  = error ("derive: Tried to use non-existing rule "++show wh++" for "++name d)

