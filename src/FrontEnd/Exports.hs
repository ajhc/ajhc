
-- | determine export\/imports for modules via fixpoint recursion

module FrontEnd.Exports(determineExports,ModInfo(..),modInfoHsModule_s) where

import Control.Monad.Identity
import Data.Monoid
import List
import Maybe
import Prelude hiding (putStr,putStrLn)
import qualified Data.Map as Map
import qualified Data.Set as Set

import CharIO
import Doc.DocLike
import FindFixpoint
import FlagDump as FD
import FrontEnd.SrcLoc
import FrontEnd.HsSyn
import Name.Name as Name
import Options
import Util.Relation as R
import FrontEnd.Warning

data ModInfo = ModInfo {
    modInfoName :: Module,
    modInfoDefs :: [(Name,SrcLoc,[Name])],
    modInfoConsArity :: [(Name,Int)],
    modInfoExport :: [Name],
    modInfoImport :: [(Name,[Name])],
    modInfoHsModule :: HsModule,
    modInfoOptions :: Opt
    }
   {-! derive: update !-}

instance Eq ModInfo where
    a == b = modInfoName a == modInfoName b

instance Ord ModInfo where
    compare a b = compare (modInfoName a) (modInfoName b)

modInfoModImports m =  mp  [ i | i <- hsModuleImports (modInfoHsModule m)] where
    mp xs
        | any ((== Module "Prelude") . hsImportDeclModule) xs = xs
        | optPrelude (modInfoOptions m) = (prelude:xs)
        | otherwise = xs
    prelude = HsImportDecl { hsImportDeclSrcLoc = bogusASrcLoc, hsImportDeclModule = Module "Prelude", hsImportDeclSpec = Nothing, hsImportDeclAs = Nothing, hsImportDeclQualified = False }

--doExports :: [(Module,[Name])] -> [[ModInfo]] -> [[ModInfo]] -> IO [[ModInfo]]

determineExports :: [(Name,SrcLoc,[Name])] -> [(Module,[Name])] -> [ModInfo]  -> IO [ModInfo]
determineExports defs ae ms = do
    wdump FD.Progress $ do
        putErrLn $ "Determining Exports/Imports: " ++ show (sort [ m | m <- map modInfoName ms])
        --mapM_ CharIO.print [ (modInfoName m, map hsImportDeclModule $ modInfoModImports m) | m <- ms]
    let ds = [ (n,cs) | (n,_,cs) <- defs ++ concatMap modInfoDefs ms]
    ms <- determineExports' ds ae ms
    let g m = do
            when (dump FD.Imports) $ do
                putStrLn $ " -- Imports -- " ++  show (modInfoName m)
                putStr $ unlines  (map show $ sort (modInfoImport m))
            when (dump FD.Exports) $ do
                putStrLn $ " -- Exports -- " ++  show (modInfoName m)
                putStr $ unlines (map show $ sort (modInfoExport m))
    mapM_ g ms
    processIOErrors
    return ms

determineExports' :: [(Name,[Name])] -> [(Module,[Name])] -> [ModInfo] -> IO [ModInfo]
determineExports' owns doneMods todoMods = mdo
    rs <- solve Nothing  mempty [ x |(_,_,x) <- ms]
    let lf m = maybe (fail $ "determineExports'.lf: " ++ show m) return $  Map.lookup m  $ dmodMap `mappend` Map.fromList [ (modInfoName x,Set.fromList [(toUnqualified x,x) | x <- modInfoExport x]) |  x  <- xs]
    let g  (mi,ne) = do
            ne' <- ce mi ne
            return mi { modInfoExport = ne', modInfoImport = toRelationList $ runIdentity $  getImports mi lf  }
    xs <- mapM g $ zip todoMods rs
    return xs
    where
    ms = [ (i,mi, getExports mi le ) | mi <- todoMods | i <- [0..]]
    dmodMap = Map.fromList  [ ( x,Set.fromList [(toUnqualified n,n) | n <- xs]) |  (x,xs) <- doneMods ]
    modMap = fmap return dmodMap `mappend` (Map.fromList [ (modInfoName n,getVal i) | (i,n,_) <- ms])
    ownsMap = Map.fromList owns
    le m = runIdentity $ maybe (fail $ "determineExports'.le: " ++ show m) return $ Map.lookup m modMap
    ce m x = mapM f (toRelationList x) where
        f (x,[y]) = return y
        f (_,[]) = error "can't happen"
        f (x,ys) = warn bogusASrcLoc "ambiguous-export" ("module " <> fromModule (modInfoName m) <> " has ambiguous exports: " ++ show ys) >> return (head ys)

    getExports :: Monad m => ModInfo -> (Module -> m (Rel Name Name)) -> m (Rel Name Name)
    getExports mi@ModInfo { modInfoHsModule = m@HsModule { hsModuleExports = Nothing } } _ = return $ defsToRel (modInfoDefs mi)
    getExports mi le | HsModule { hsModuleExports = Just es } <- modInfoHsModule mi = do
        is <- getImports mi le
        let f (HsEModuleContents m) = mapDomain g unqs `R.intersection` qs where
                (qs,unqs) = partitionDomain (isJust . getModule ) is
                g x = Name.qualifyName m x
            f z = entSpec False is z
        return $ mapDomain toUnqualified (R.unions $ map f es)

    -- | determine what is visible in a module
    getImports :: Monad m => ModInfo -> (Module -> m (Rel Name Name)) -> m (Rel Name Name)
    getImports mi le = mapM f is >>= \xs -> return (mconcat (ls:xs))  where
        f x = do
            es <- le (hsImportDeclModule x)
            Just as <- return $  hsImportDeclAs x `mplus` Just (hsImportDeclModule x)
            es' <- case hsImportDeclSpec x of
                Nothing -> return es -- return $ (mapDomain ((Name.qualifyName as)) es `mappend` if hsImportDeclQualified x then mempty else es)
                Just (isHiding,xs) -> do
                    let listed = mconcat $ map (entSpec isHiding es . importToExport) xs
                    return $ if isHiding then es Set.\\ listed else listed
            return $ (mapDomain ((Name.qualifyName as)) es' `mappend` if hsImportDeclQualified x then mempty else es')
        is = modInfoModImports mi
        ls = R.fromList $  concat [ [(toUnqualified z,z),(z,z)]| (z, _, _) <- modInfoDefs mi]

    entSpec ::
        Bool     -- ^ is it a hiding import?
        -> Rel Name Name  -- ^ the original relation
        -> HsExportSpec   -- ^ the specification
        -> Rel Name Name  -- ^ the subset satisfying the specification
    entSpec isHiding rel (HsEVar n) = restrictDomain (== toName Val n) rel
    entSpec isHiding rel (HsEAbs n) = restrictDomain (`elem` [ toName x n | x <- ts]) rel  where
        ts = TypeConstructor:ClassName:if isHiding then [DataConstructor] else []
    entSpec isHiding rel (HsEThingWith n xs) = restrictDomain (\x -> x `elem` concat (ct:(map (cd) xs)))  rel where
        ct = [toName TypeConstructor n, toName ClassName n]
        cd n =  [toName DataConstructor n, toName Val n, toName FieldLabel n ]
    entSpec isHiding rel (HsEThingAll n) = restrictDomain (`elem` ct ) rel `mappend` restrictRange (`elem` ss) rel where
        ct = [toName TypeConstructor n, toName ClassName n]
        ss = concat $ concat [ maybeToList (Map.lookup x ownsMap) | x <- Set.toList $ range (restrictDomain (`elem` ct) rel)]
        cd n =  [toName DataConstructor n, toName Val n, toName FieldLabel n ]


defsToRel xs = R.fromList $ map f xs where
    f (n,_,_) = (toUnqualified n,n)

importToExport :: HsImportSpec -> HsExportSpec
importToExport x = f x where
    f (HsIVar n) = HsEVar n
    f (HsIAbs n) = HsEAbs n
    f (HsIThingAll n) = HsEThingAll n
    f (HsIThingWith n xs) = HsEThingWith n xs

