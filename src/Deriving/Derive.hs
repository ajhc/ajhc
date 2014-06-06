module Deriving.Derive(derivingDerive) where

import Util.Std
import FrontEnd.Syn.Q(runQ)
import Deriving.Ix
import Deriving.Ord
import Deriving.Typeable
import Deriving.Text
import Deriving.Type as D
import FrontEnd.HsSyn
import FrontEnd.Warning
import Name.Names
import qualified Data.Map as Map

{-
declIsEnum HsDataDecl { .. } = length hsDeclCons > 1
    && null (concatMap hsConDeclArgs hsDeclCons)
declIsEnum d = False
-}

collectDerives :: [HsDecl] -> [Derive]
collectDerives xs = f xs (Map.empty) where
    f [] m = concat $ map postproc (Map.elems m)
    f (x@HsDataDecl { .. }:xs) oldMap =  f xs (Map.unionWith (++) oldMap newMap) where
        newMap = Map.fromListWith (++) [ (hsDeclName,[d]) | d <- derives]
        placeholder = Derive { deriveHead = HsClassHead { hsClassHead = u_placeholder, .. }, .. }
        deriveSrcLoc = hsDeclSrcLoc
        deriveData = Just $ toDataD  hsDeclName hsDeclArgs hsDeclCons hsDeclDerives
        standAlone = False
        derives = placeholder:map g hsDeclDerives
        hsClassHeadContext = []
        hsClassHeadArgs = []
        g w = Derive { .. } where
            deriveHead = HsClassHead { .. } where
                hsClassHead = w
    f (x@HsDeclDeriving { .. }:xs) oldMap =  f xs (Map.unionWith (++) oldMap newMap) where
        newMap = Map.singleton theType [Derive { .. }]
        deriveSrcLoc = hsDeclSrcLoc
        deriveData = Nothing
        standAlone = True
        deriveHead = hsDeclClassHead
        [fst . fromHsTypeApp -> HsTyCon theType] = hsClassHeadArgs deriveHead
    f (_:xs) om = f xs om
    fromHsTypeApp t = f t [] where
        f (HsTyApp a b) rs = f a (b:rs)
        f t rs = (t,rs)
    postproc xsys = ff $ case [ d | Derive { deriveData = Just d } <- xsys ] of
        (d:_) -> map (deriveData_u (const $ Just d)) xsys
        [] -> xsys
    ff xs = [ d | d <- xs, hsClassHead (deriveHead d) /= u_placeholder]

{-# NOINLINE derivingDerive #-}
-- new declarations, leftover derives.
derivingDerive :: MonadWarn m => HsModule -> m ([HsDecl],[Derive])
derivingDerive HsModule { .. } = mconcat <$> runQ (mapM g derives) where
    derives = collectDerives hsModuleDecls
--    g d = do warn hsModuleSrcLoc ParseInfo $ show d  ; f d
    g d = f d
    f d@Derive { .. }
        | Just fn <- lookup ch normClasses = g False fn
        | Just fn <- lookup ch enumClasses = g True fn where
        Just dat@D { .. } = deriveData
        ch = hsClassHead deriveHead
        isEnum = length body > 1 && all null (map types body)
        g True fn | isEnum  = return ([],[d])
        g _ _ | deriveData == Nothing = do
            warn deriveSrcLoc ParseInfo $ "Cannot derive class without definition " ++ show (hsClassHead deriveHead)
            return ([],[d])
        g _ fn = do
            nds <- fn deriveSrcLoc hsModuleName dat
            return ([nds],[])
    f d = do
        warn (deriveSrcLoc d) InvalidDecl $ "No rule to derive class " ++ show (hsClassHead $ deriveHead d)
        return ([],[d])

    enumClasses =
        [(class_Eq, deriveEq)
        ,(class_Ord, deriveOrd)
        ,(class_Enum, deriveEnum)
        ,(class_Ix, deriveIx)]
    normClasses =
        [(class_Bounded, deriveBounded)
        ,(class_Show, deriveShow)
        ,(class_Read, deriveRead)
        ,(class_Typeable, deriveTypeable  0 class_Typeable)
        ,(class_Typeable1, deriveTypeable 1 class_Typeable1)
        ,(class_Typeable2, deriveTypeable 2 class_Typeable2)
        ,(class_Typeable3, deriveTypeable 3 class_Typeable3)]

deriveData_u f Derive { .. } = Derive { deriveData = f deriveData, .. }

toDataD :: Name -> [Name] -> [HsConDecl] -> [Name] -> D.Data
toDataD name args cons derives = ans where
    f c = D.Body { constructor = hsConDeclName c, types = hsConDeclArgs c, labels = lb c }
    lb HsConDecl {} = []
    lb  r = concatMap fst (hsConDeclRecArg r)
    ans = D.D { statement = DeclTypeData , vars = args, constraints = [], name = name,  derives = derives, body = map f cons }
