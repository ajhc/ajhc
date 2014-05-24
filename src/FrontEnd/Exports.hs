{-# LANGUAGE RecursiveDo,NoMonoLocalBinds #-}
-- | determine export\/imports for modules via fixpoint recursion

module FrontEnd.Exports(determineExports,ModInfo(..)) where

import Control.Monad.Identity
import Data.List
import Data.Maybe
import Data.Monoid(Monoid(..))
import qualified Data.Map as Map
import qualified Data.Set as Set

import Doc.DocLike
import FindFixpoint
import FlagDump as FD
import FlagOpts as FO
import FrontEnd.HsSyn
import FrontEnd.SrcLoc
import FrontEnd.Warning
import Name.Names as Name
import Options
import Util.Relation as R
import Util.SetLike as SL

data ModInfo = ModInfo {
    modInfoName       :: Module,
    modInfoDefs       :: [(Name,SrcLoc,[Name])],
    modInfoConsArity  :: [(Name,Int)],
    modInfoExport     :: [Name],
    modInfoImport     :: [(Name,[Name])],
    modInfoHsModule   :: HsModule,
    modInfoReverseMap :: Map.Map Name Name,
    modInfoOptions    :: Opt
    }

instance Eq ModInfo where
    a == b = modInfoName a == modInfoName b

instance Ord ModInfo where
    compare a b = compare (modInfoName a) (modInfoName b)

modInfoModImports m =  mp  [ i | i <- hsModuleImports (modInfoHsModule m)] where
    mp xs
        | any ((== mod_Prelude) . hsImportDeclModule) xs = xs
        | FO.Prelude `Set.member` (optFOptsSet $ modInfoOptions m) = (prelude:xs)
        | otherwise = xs
    prelude = HsImportDecl {
        hsImportDeclSrcLoc = bogusASrcLoc,
        hsImportDeclModule = mod_Prelude,
        hsImportDeclSpec = Nothing,
        hsImportDeclAs = Nothing,
        hsImportDeclQualified = False }

--doExports :: [(Module,[Name])] -> [[ModInfo]] -> [[ModInfo]] -> IO [[ModInfo]]

determineExports :: [(Name,SrcLoc,[Name])] -> [(Module,[Name])] -> [ModInfo]  -> IO [ModInfo]
determineExports defs ae ms = do
    --wdump FD.Progress $ do
    --    putErrLn $ "Determining Exports/Imports: " ++ show (sort [ m | m <- map modInfoName ms])
        --mapM_ CharIO.print [ (modInfoName m, map hsImportDeclModule $ modInfoModImports m) | m <- ms]
    let ds = [ (n,cs) | (n,_,cs) <- defs ++ concatMap modInfoDefs ms]
    ms <- determineExports' ds ae ms
    let g m = do
            when (dump FD.Imports) $ do
                putStrLn $ " -- Imports: " ++ show (modInfoName m)
                putStr $ unlines  (sort $ map show (modInfoImport m))
            when (dump FD.Exports) $ do
                putStrLn $ " -- Exports: " ++ show (modInfoName m)
                mapM_ putStrLn (sort [ show (nameType n) ++ " " ++ show n | n <- modInfoExport m])
    mapM_ g ms
    processIOErrors "import/export processing"
    return ms

determineExports' :: [(Name,[Name])] -> [(Module,[Name])] -> [ModInfo] -> IO [ModInfo]
determineExports' owns doneMods todoMods = mdo
    rs <- solve Nothing  mempty [ x |(_,_,x) <- ms]
    let lf m = maybe (fail $ "determineExports'.lf: " ++ show m) return $  Map.lookup m  $ dmodMap `mappend` Map.fromList [ (modInfoName x,fromList [(toUnqualified x,x) | x <- modInfoExport x]) |  x  <- xs]
    let g  (mi,ne) = do
            ne' <- ce mi ne
            return mi { modInfoExport = ne', modInfoImport = toRelationList $ runIdentity $  getImports mi lf  }
    xs <- mapM g $ zip todoMods rs
    return xs
    where
    ms = [ (i,mi, getExports mi le ) | mi <- todoMods | i <- [0..]]
    dmodMap = Map.fromList  [ ( x,fromList [(toUnqualified n,n) | n <- xs]) |  (x,xs) <- doneMods ]
    modMap = fmap return dmodMap `mappend` (Map.fromList [ (modInfoName n,getVal i) | (i,n,_) <- ms])
    ownsMap = Map.fromList owns
    le m = runIdentity $ maybe (fail $ "determineExports'.le: " ++ show m) return $ Map.lookup m modMap
    ce m x = mapM f (toRelationList x) where
        f (x,[y]) = return y
        f (_,[]) = error "can't happen"
        f (x,ys) = warn bogusASrcLoc (AmbiguousExport (modInfoName m) ys) ("module " <> show (modInfoName m) <> " has ambiguous exports: " ++ show ys) >> return (head ys)

    getExports :: Monad m => ModInfo -> (Module -> m (Rel Name Name)) -> m (Rel Name Name)
    getExports mi@ModInfo { modInfoHsModule = m@HsModule { hsModuleExports = Nothing } } _ = return $ defsToRel (modInfoDefs mi)
    getExports mi le | HsModule { hsModuleExports = Just es } <- modInfoHsModule mi = do
        is <- getImports mi le
        let f (HsEModuleContents m) = mapDomain g unqs `intersection` qs where
                (qs,unqs) = partitionDomain (isJust . getModule ) is
                g x = Name.qualifyName m x
            f z = entSpec False is z
        return $ mapDomain toUnqualified (unions $ map f es)
    getExports _ _ = error "Exports.getExports: bad."

    -- | determine what is visible in a module
    getImports :: Monad m => ModInfo -> (Module -> m (Rel Name Name)) -> m (Rel Name Name)
    getImports mi le = mapM f is >>= \xs -> return (mconcat (ls:xs))  where
        f x = do
            es <- le (hsImportDeclModule x)
            Just as <- return $  hsImportDeclAs x `mplus` Just (hsImportDeclModule x)
            es' <- case hsImportDeclSpec x of
                Nothing -> return es -- return $ (mapDomain ((Name.qualifyName as)) es `mappend` if hsImportDeclQualified x then mempty else es)
                Just (isHiding,xs) -> do
                    let listed = mconcat $ map (entSpec isHiding es) xs
                    return $ if isHiding then es SL.\\ listed else listed
            return $ (mapDomain ((Name.qualifyName as)) es' `mappend` if hsImportDeclQualified x then mempty else es')
        is = modInfoModImports mi
        ls = fromList $  concat [ [(toUnqualified z,z),(z,z)]| (z, _, _) <- modInfoDefs mi]

    entSpec ::
        Bool              -- ^ is it a hiding import?
        -> Rel Name Name  -- ^ the original relation
        -> HsExportSpec   -- ^ the specification
        -> Rel Name Name  -- ^ the subset satisfying the specification
    entSpec isHiding rel e = f Nothing e where
	f _ (HsEVar n) = restrictDomainS (toName Val n) rel
	f Nothing (HsEAbs n) = restrictDomainSet (Set.fromList [ toName x n | x <- ts]) rel  where
	    ts = TypeConstructor:ClassName:if isHiding then [DataConstructor] else []
	f (Just nt) (HsEAbs n) = restrictDomainSet (Set.singleton (toName nt n)) rel  where
	f mnt (HsEThingWith n xs) = restrictDomainSet (fromList (concat (map (`toName` n) ct:(map cd xs)))) rel where
	    ct = case mnt of
		Nothing -> [TypeConstructor,ClassName]
		Just nt -> [nt]
	    cd n =  [toName DataConstructor n, toName Val n, toName FieldLabel n ]
	f mnt (HsEThingAll n) = rdl `mappend` restrictRange (`elem` ss) rel where
	    ct = case mnt of
		Nothing -> [TypeConstructor,ClassName]
		Just nt -> [nt]
	    ss = concat $ concat [ maybeToList (Map.lookup x ownsMap) | x <- Set.toList $ range rdl ]
	    --cd n = [toName DataConstructor n, toName Val n, toName FieldLabel n ]
	    rdl = (restrictDomain (`elem` map (`toName` n) ct) rel)
	f _ (HsEQualified t n) = f (Just t) n
        f _ _ = error "Export.determineExports': bad."

defsToRel xs = fromList $ map f xs where
    f (n,_,_) = (toUnqualified n,n)
