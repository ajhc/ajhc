{-# LANGUAGE RecursiveDo #-}
module Ho.Build (
    module Ho.Type,
    dumpHoFile,
    parseFiles,
    preprocess,
    preprocessHs,
    buildLibrary
    ) where

import Control.Concurrent
import Control.Monad.Identity
import Data.Char
import Data.IORef
import Data.List hiding(union)
import Data.Maybe
import Data.Monoid
import Data.Tree
import Data.Version(Version,parseVersion,showVersion)
import Data.Yaml.Syck
import System.Directory (removeFile)
import System.FilePath as FP
import System.Mem
import Text.Printf
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBSU
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.PrettyPrint.HughesPJ as PPrint

import DataConstructors
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.E
import E.Rules
import E.Show
import E.Traverse(emapE)
import E.TypeCheck()
import FrontEnd.Class
import FrontEnd.FrontEnd
import FrontEnd.HsParser
import FrontEnd.HsSyn
import FrontEnd.Infix
import FrontEnd.ParseMonad
import FrontEnd.SrcLoc
import FrontEnd.Unlit
import FrontEnd.Warning
import Ho.Binary
import Ho.Collected()
import Ho.Library
import Ho.ReadSource
import Ho.Type
import Name.Name
import Options
import PackedString(PackedString,packString,unpackPS)
import Util.FilterInput
import Util.Gen
import Util.SetLike
import Util.YAML
import Version.Config(revision,version)
import Version.Version(versionString)
import qualified FlagDump as FD
import qualified FlagOpts as FO
import qualified Support.MD5 as MD5
import qualified Util.Graph as G

-- Ho File Format
--
-- ho files are standard CFF format files (PNG-like) as described in the Support.CFF modules.
--
-- the CFF magic for the files is the string "JHC"
--
-- JHDR - header info, contains a list of modules contained and dependencies that need to be checked to read the file
-- LIBR - only present if this is a library, contains library metainfo
-- IDEP - immutable import information, needed to tell if ho files are up to date
-- LINK - redirect to another file for systems without symlinks
-- DEFS - definitions type checking information
-- CORE - compiled core and associated data
-- LDEF - library map of module group name to DEFS
-- LCOR - library map of module group name to CORE
-- GRIN - compiled grin code

{-
 - We separate the data into various chunks for logical layout as well as the important property that
 - each chunk is individually compressed and accessable. What this means is
 - that we can skip chunks we don't need. for instance, during the final link
 - we have no need of the haskell type checking information, we are only
 - interested in the compiled code, so we can jump directly to it. If we relied on straight
 - serialization, we would have to parse all preceding information just to discard it right away.
 - We also lay them out so that we can generate error messages quickly. for instance, we can determine
 - if a symbol is undefined quickly, before it has to load the typechecking data.
 -}

type LibraryName = PackedString

findFirstFile :: String -> [(FilePath,a)] -> IO (LBS.ByteString,FilePath,a)
findFirstFile err [] = FrontEnd.Warning.err "missing-dep" ("Module not found: " ++ err) >> fail ("Module not found: " ++ err) -- return (error "findFirstFile not found","",undefined)
findFirstFile err ((x,a):xs) = flip catch (\e ->   findFirstFile err xs) $ do
    bs <- LBS.readFile x
    return (bs,x,a)

data ModDone
    = ModNotFound
    | ModLibrary !Bool ModuleGroup Library
    | Found SourceCode

data Done = Done {
    hoCache         :: Maybe FilePath,
    knownSourceMap  :: Map.Map SourceHash (Module,[Module]),
    validSources    :: Set.Set SourceHash,
    loadedLibraries :: Map.Map LibraryName Library,
    hosEncountered  :: Map.Map HoHash     (FilePath,HoHeader,HoIDeps,Ho),
    modEncountered  :: Map.Map Module     ModDone
    }
    {-! derive: update !-}

replaceSuffix suffix fp = reverse (dropWhile ('.' /=) (reverse fp)) ++ suffix

hoFile :: Maybe FilePath -> FilePath -> Maybe Module -> SourceHash -> FilePath
hoFile cacheDir fp mm sh = case (cacheDir,optHoDir options) of
    (Nothing,Nothing) -> replaceSuffix "ho" fp
    (Nothing,Just hdir) -> case mm of
        Nothing -> hdir ++ "/" ++ MD5.md5show32 sh ++ ".ho"
        Just m -> hdir ++ "/" ++ map ft (show m) ++ ".ho" where
            ft '/' = '.'
            ft x = x
    (Just hdir,_) -> hdir ++ "/" ++ MD5.md5show32 sh ++ ".ho"

findHoFile :: IORef Done -> FilePath -> Maybe Module -> SourceHash -> IO (Bool,FilePath)
findHoFile done_ref fp mm sh = do
    done <- readIORef done_ref
    let honame = hoFile (hoCache done) fp mm sh
    writeIORef done_ref (done { validSources = Set.insert sh (validSources done) })
    if sh `Set.member` validSources done || optIgnoreHo options then return (False,honame) else do
    onErr (return (False,honame)) (readHoFile honame) $ \ (hoh,hidep,ho) ->
        case hohHash hoh `Map.lookup` hosEncountered done of
            Just (fn,_,_,a) -> return (True,fn)
            Nothing -> do
                modifyIORef done_ref (knownSourceMap_u $ mappend (hoIDeps hidep))
                modifyIORef done_ref (validSources_u $ Set.union (Set.fromList . map snd $ hoDepends hidep))
                modifyIORef done_ref (hosEncountered_u $ Map.insert (hohHash hoh) (honame,hoh,hidep,ho))
                return (True,honame)

onErr :: IO a -> IO b -> (b -> IO a) -> IO a
onErr err good cont = join $ catch (good >>= return . cont) (\_ -> return err)

fetchSource :: Opt -> IORef Done -> [FilePath] -> Maybe Module -> IO Module
fetchSource _ _ [] _ = fail "No files to load"
fetchSource modOpt done_ref fs mm = do
    let mod = maybe (head fs) show mm
        killMod = case mm of
            Nothing -> fail $ "Could not load file: " ++ show fs
            Just m -> modifyIORef done_ref (modEncountered_u $ Map.insert m ModNotFound) >> return m
    onErr killMod (findFirstFile mod [ (f,undefined) | f <- fs]) $ \ (lbs,fn,_) -> do
    let hash = MD5.md5lazy $ (LBSU.fromString version) `mappend` lbs
    (foundho,mho) <- findHoFile done_ref fn mm hash
    done <- readIORef done_ref
    (mod,m,ds) <- case mlookup hash (knownSourceMap done) of
        Just (m,ds) -> return (Left lbs,m,ds)
        Nothing -> do
            (hmod,_) <- parseHsSource modOpt  fn lbs
            let m = hsModuleName hmod
                ds = hsModuleRequires hmod
            writeIORef done_ref (knownSourceMap_u (Map.insert hash (m,ds)) done)
            case optAnnotate options of
                Just _ -> return (Left lbs,m,ds)
                _ -> return (Right hmod,m,ds)
    case mm of
        Just m' | m /= m' -> do
            putErrLn $ "Skipping file" <+> fn <+> "because its module declaration of" <+> show m <+> "does not equal the expected" <+> show m'
            killMod
        _ -> do
            let sc (Right mod) = SourceParsed sinfo mod
                sc (Left lbs) = SourceRaw sinfo lbs
                sinfo = SI { sourceHash = hash, sourceDeps = ds, sourceFP = fn, sourceHoName = mho, sourceModName = m }
            modifyIORef done_ref (modEncountered_u $ Map.insert m (Found (sc mod)))
            fn' <- shortenPath fn
            mho' <- shortenPath mho
            putProgressLn $ if foundho
                then printf "%-23s [%s] <%s>" (show m) fn' mho'
                else printf "%-23s [%s]" (show m) fn'
            mapM_ (resolveDeps modOpt done_ref) ds
            return m

resolveDeps :: Opt -> IORef Done -> Module -> IO ()
resolveDeps modOpt done_ref m = do
    done <- readIORef done_ref
    case m `mlookup` modEncountered done of
        Just (ModLibrary False _ lib) | not ("jhc-prim-" `isPrefixOf` libName lib)  -> putErrDie $ printf  "ERROR: Attempt to import module '%s' which is a member of the library '%s'." (show m) (libName lib)
        Just _ -> return ()
        Nothing -> fetchSource modOpt done_ref (map fst $ searchPaths modOpt (show m)) (Just m) >> return ()

type LibInfo = (Map.Map Module ModuleGroup, Map.Map ModuleGroup [ModuleGroup], Set.Set Module,Map.Map ModuleGroup HoBuild,Map.Map ModuleGroup HoTcInfo)

data CompNode = CompNode !HoHash [CompNode] {-# UNPACK #-} !(IORef CompLink)
data CompLink
    = CompLinkUnit CompUnit
    | CompCollected CollectedHo CompUnit
    | CompTcCollected HoTcInfo CompUnit
    | CompLinkLib (ModuleGroup,LibInfo) CompUnit

compLinkCompUnit (CompLinkUnit cu) = cu
compLinkCompUnit (CompCollected _ cu) = cu
compLinkCompUnit (CompTcCollected _ cu) = cu
compLinkCompUnit (CompLinkLib _ cu) = cu

instance MapKey Module where
    showMapKey = show
instance MapKey MD5.Hash where
    showMapKey = show

dumpDeps targets memap cug = case optDeps options of
    Nothing -> return ()
    Just fp -> do
        let (sfps,sdps,ls) = collectDeps memap cug
        let yaml = Map.fromList [
                ("Target",toNode targets),
                ("LibraryDesc",toNode [ fp | BuildHl fp  <- [optMode options]]),
                ("LibraryDeps",toNode ls),
                ("ModuleSource",toNode sfps),
                ("ModuleDeps",toNode sdps)
                ]
        writeFile fp (showYAML yaml)

collectDeps memap cs = mconcatMap f [ cu | (_,(_,cu)) <- cs] where
    f (CompSources ss) = mconcat [ (Map.singleton (sourceModName s) (sourceFP s),Map.singleton (sourceModName s) (sourceDeps s),mempty) | s <- map sourceInfo ss ]
    f (CompLibrary _ lib) = (mempty,mempty,Map.singleton (libHash lib) (libFileName lib))
    f (CompHo _hoh idep _ho) =  (Map.fromList [ (sourceModName $ sourceInfo src, sourceFP $ sourceInfo src) | s <- fsts ss, Just (Found src) <- [Map.lookup s memap] ],Map.fromList [ mms | s <- snds ss, Just mms <- [Map.lookup s (hoIDeps idep)] ],mempty) where
        ss = [ s | s <- hoDepends idep ]
    f _ = mempty

type CompUnitGraph = [(HoHash,([HoHash],CompUnit))]

data CompUnit
    = CompHo HoHeader HoIDeps Ho
    | CompSources [SourceCode]
    | CompTCed ((HoTcInfo,TiData,[(HoHash,HsModule)],[String]))
    | CompDummy
    | CompLibrary Ho Library

instance Show CompUnit where
    showsPrec _ = shows . providesModules

data SourceInfo = SI {
    sourceHash :: SourceHash,
    sourceDeps :: [Module],
    sourceFP :: FilePath,
    sourceModName :: Module,
    sourceHoName :: FilePath
    }

data SourceCode
    = SourceParsed     { sourceInfo :: !SourceInfo, sourceModule :: HsModule }
    | SourceRaw        { sourceInfo :: !SourceInfo, sourceLBS :: LBS.ByteString }

sourceIdent = show . sourceModName . sourceInfo

class ProvidesModules a where
    providesModules :: a -> [Module]
    providesModules _ = []

instance ProvidesModules HoIDeps where
    providesModules = fsts . hoDepends

instance ProvidesModules HoLib where
    providesModules = Map.keys . hoModuleMap

instance ProvidesModules CompUnit where
    providesModules (CompHo _ hoh _)   = providesModules hoh
    providesModules (CompSources ss) = concatMap providesModules ss
    providesModules (CompLibrary ho libr) = libProvides (hoModuleGroup ho) libr
    providesModules CompDummy = []

instance ProvidesModules CompLink where
    providesModules (CompLinkUnit cu) = providesModules cu
    providesModules (CompCollected _ cu) = providesModules cu
    providesModules (CompTcCollected _ cu) = providesModules cu

instance ProvidesModules SourceCode where
    providesModules sp = [sourceModName (sourceInfo sp)]

-- | this walks the loaded modules and ho files, discarding out of
-- date ho files and organizing modules into their binding groups.
-- the result is an acyclic graph where the nodes are ho files, sets
-- of mutually recursive modules, or libraries.
-- there is a strict ordering of
-- source >= ho >= library
-- in terms of dependencies

toCompUnitGraph :: Done -> [Module] -> IO (HoHash,CompUnitGraph)
toCompUnitGraph done roots = do
    let fs m = map inject $ maybe (error $ "can't find deps for: " ++ show m) snd (Map.lookup m (knownSourceMap done))
        fs' m libr = fromMaybe (error $ "can't find deps for: " ++ show m) (Map.lookup m (hoModuleDeps $ libHoLib libr))
        foundMods = [ ((m,Left (sourceHash $ sourceInfo sc)),fs (sourceHash $ sourceInfo sc)) | (m,Found sc) <- Map.toList (modEncountered done)]
        foundMods' = Map.elems $ Map.fromList [ (mg,((mg,Right lib),fs' mg lib)) | (_,ModLibrary _ mg lib) <- Map.toList (modEncountered done)]
        fullModMap = Map.unions (map libModMap $ Map.elems (loadedLibraries done))
        inject m = Map.findWithDefault m m fullModMap
        gr = G.newGraph  (foundMods ++ foundMods') (fst . fst) snd
        gr' = G.sccGroups gr
        phomap = Map.fromListWith (++) (concat [  [ (m,[hh]) | (m,_) <- hoDepends idep ] | (hh,(_,_,idep,_)) <- Map.toList (hosEncountered done)])
        sources = Map.fromList [ (m,sourceHash $ sourceInfo sc) | (m,Found sc) <- Map.toList (modEncountered done)]

    when (dump FD.SccModules) $ do
        mapM_ (putErrLn . show) $ map (map $ fst . fst) gr'
        putErrLn $ drawForest (map (fmap (show . fst . fst)) (G.dff gr))

    cug_ref <- newIORef []
    hom_ref <- newIORef (Map.map ((,) False) $ hosEncountered done)
    ms <- forM gr' $ \ns -> do
        r <- newIORef (Left ns)
        return (Map.fromList [ (m,r) | ((m,_),_) <- ns ])
    let mods = Map.unions ms
        lmods m = fromMaybe (error $ "modsLookup: " ++ show m) (Map.lookup m mods)
    let f m = do
            rr <- readIORef (lmods m)
            case rr of
                Right hh -> return hh
                Left ns -> g ns

        g ms@(((m,Left _),_):_) = do
            let amods = map (fst . fst) ms
            pm (fromMaybe [] (Map.lookup m phomap)) $ do
                let deps = Set.toList $ Set.fromList (concat $ snds ms) `Set.difference` (Set.fromList amods)
                deps' <- snub `fmap` mapM f deps
                let mhash = MD5.md5String (concatMap (show . fst) ms ++ show deps')
                writeIORef (lmods m) (Right mhash)
                modifyIORef cug_ref ((mhash,(deps',CompSources $ map fs amods)):)
                return mhash
        g [((mg,Right lib),ds)] = do
                let Just hob = Map.lookup mg $ libBuildMap lib
                    Just hot = Map.lookup mg $ libTcMap lib
                    ho = Ho { hoModuleGroup = mg, hoBuild = hob, hoTcInfo = hot }
                    myHash = libMgHash mg lib
                deps <- snub `fmap` mapM f ds
                writeIORef (lmods mg) (Right myHash)
                modifyIORef cug_ref ((myHash,(deps,CompLibrary ho lib)):)
                return myHash
        pm :: [HoHash] -> IO HoHash -> IO HoHash
        pm [] els = els
        pm (h:hs) els = hvalid h `catch` (\_ -> pm hs els)
        hvalid h = do
            ll <- Map.lookup h `fmap` readIORef hom_ref
            case ll of
                Nothing -> fail "Don't know anything about this hash"
                Just (True,_) -> return h
                Just (False,af@(fp,hoh,idep,ho)) -> do
                    fp <- shortenPath fp
                    isGood <- catch ( mapM_ cdep (hoDepends idep) >> mapM_ hvalid (hoModDepends idep) >> return True) (\_ -> return False)
                    let isStale = not . null $ map (show . fst) (hoDepends idep) `intersect` optStale options
                        libsGood = all (\ (p,h) -> fmap (libHash) (Map.lookup p (loadedLibraries done)) == Just h) (hohLibDeps hoh)
                        noGood forced = do
                            putProgressLn $ printf "Stale: <%s>%s" fp forced
                            modifyIORef hom_ref (Map.delete h)
                            fail "stale file"
                    case (isStale,isGood && libsGood) of
                        (False,True) -> do
                            putProgressLn $ printf "Fresh: <%s>" fp
                            hs <- mapM f (hoModuleGroupNeeds idep)
                            modifyIORef cug_ref ((h,(hs ++ hoModDepends idep,CompHo hoh idep ho)):)
                            modifyIORef hom_ref (Map.insert h (True,af))
                            return h
                        (True,_) -> noGood " (forced)"
                        (_,False) -> noGood ""
        cdep (_,hash) | hash == MD5.emptyHash = return ()
        cdep (mod,hash) = case Map.lookup mod sources of
            Just hash' | hash == hash' -> return ()
            _ -> fail "Can't verify module up to date"
        fs m = case Map.lookup m (modEncountered done) of
            Just (Found sc) -> sc
            _ -> error $ "fs: " ++ show m
    mapM_ f (map inject roots)
    cug <- readIORef cug_ref
    let (rhash,cug') = mkPhonyCompUnit roots cug
    let gr = G.newGraph cug'  fst (fst . snd)
        gr' = G.transitiveReduction gr
    when (dump FD.SccModules) $ do
        putErrLn "ComponentsDeps:"
        mapM_ (putErrLn . show) [ (snd $ snd v, map (snd . snd) vs) | (v,vs) <- G.fromGraph gr']
    return (rhash,[ (h,([ d | (d,_) <- ns ],cu)) | ((h,(_,cu)),ns) <- G.fromGraph gr' ])

parseFiles
    :: Opt                                                  -- ^ Options to use when parsing files
    -> [FilePath]                                           -- ^ Targets we are building, used when dumping dependencies
    -> [String]                                             -- ^ Extra libraries to load
    -> [Either Module FilePath]                             -- ^ Either a module or filename to find
    -> (CollectedHo -> Ho -> IO CollectedHo)                -- ^ Process initial ho loaded from file
    -> (CollectedHo -> Ho -> TiData -> IO (CollectedHo,Ho)) -- ^ Process set of mutually recursive modules to produce final Ho
    -> IO (CompNode,CollectedHo)                            -- ^ Final accumulated ho
parseFiles options targets elibs need ifunc func = do
    putProgressLn "Finding Dependencies..."
    (ksm,chash,cug) <- loadModules options targets (snub $
        if optNoAuto options then optHls options ++ elibs else optAutoLoads options ++ optHls options ++ elibs) need
    cnode <- processCug cug chash
    when (optMode options == StopParse) exitSuccess
    performGC
    putProgressLn "Typechecking..."
    typeCheckGraph options cnode
    if isJust (optAnnotate options) then exitSuccess else do
    when (optMode options  == StopTypeCheck) exitSuccess
    performGC
    putProgressLn "Compiling..."
    cho <- compileCompNode ifunc func ksm cnode
    return (cnode,cho)

-- this takes a list of modules or files to load, and produces a compunit graph
loadModules
    :: Opt                      -- ^ Options to use when parsing files
    -> [FilePath]               -- ^ targets
    -> [String]                 -- ^ libraries to load
    -> [Either Module String]   -- ^ a list of modules or filenames
    -> IO (Map.Map SourceHash (Module,[Module]),HoHash,CompUnitGraph)  -- ^ the resulting acyclic graph of compilation units
loadModules modOpt targets libs need = do
    theCache <- findHoCache
    case theCache of
        Just s -> putProgressLn $ printf "Using Ho Cache: '%s'" s
        Nothing -> return ()
    done_ref <- newIORef Done {
        hoCache = theCache,
        knownSourceMap = Map.empty,
        validSources = Set.empty,
        loadedLibraries = Map.empty,
        hosEncountered = Map.empty,
        modEncountered = Map.empty
        }
    (es,is) <- collectLibraries libs
    let combModMap es = Map.unions [ Map.map ((,) l) (hoModuleMap $ libHoLib l) | l <- es]
        explicitModMap = combModMap es
        implicitModMap = combModMap is
        reexported  = Set.fromList [ m | l <- es, (m,_) <- Map.toList $ hoReexports (libHoLib l) ]
        modEnc exp emap = Map.fromList [ (m,ModLibrary (exp || Set.member m reexported)  mg l) | (m,(l,mg)) <- Map.toList emap ]

    modifyIORef done_ref (loadedLibraries_u $ Map.union $ Map.fromList [ (libBaseName lib,lib) | lib <- es ++ is])
    modifyIORef done_ref (modEncountered_u $ Map.union (modEnc True explicitModMap))
    modifyIORef done_ref (modEncountered_u $ Map.union (modEnc False implicitModMap))

    done <- readIORef done_ref
    forM_ (Map.elems $ loadedLibraries done) $ \ lib -> do
        let libsBad = filter (\ (p,h) -> fmap (libHash) (Map.lookup p (loadedLibraries done)) /= Just h) (hohLibDeps $ libHoHeader lib)
        unless (null libsBad) $ do
            putErr $ printf "Library Dependencies not met. %s needs\n" (libName lib)
            forM_ libsBad $ \ (p,h) -> putErr $ printf "    %s (hash:%s)\n" (unpackPS p) (show h)
            putErrDie "\n"
    ms1 <- forM (rights need) $ \fn -> do
        fetchSource modOpt done_ref [fn] Nothing
    forM_ (lefts need) $ resolveDeps modOpt done_ref
    processIOErrors
    done <- readIORef done_ref
    let needed = (ms1 ++ lefts need)
    (chash,cug) <- toCompUnitGraph done needed
    dumpDeps targets (modEncountered done) cug
    return (Map.filterWithKey (\k _ -> k `Set.member` validSources done) (knownSourceMap done),chash,cug)

-- turn the list of CompUnits into a true mutable graph.
processCug :: CompUnitGraph -> HoHash -> IO CompNode
processCug cug root = mdo
    let mmap = Map.fromList xs
        lup x = maybe (error $ "processCug: " ++ show x) id (Map.lookup x mmap)
        f (h,(ds,cu)) = do
            cur <- newIORef (CompLinkUnit cu)
            return $ (h,CompNode h (map lup ds) cur)
    xs <- mapM f cug
    Just x <- return $ Map.lookup root mmap
    return $ x

mkPhonyCompUnit :: [Module] -> CompUnitGraph -> (HoHash,CompUnitGraph)
mkPhonyCompUnit need cs = (fhash,(fhash,(fdeps,CompDummy)):cs) where
        fhash = MD5.md5String $ show (sort fdeps)
        fdeps = [ h | (h,(_,cu)) <- cs, not . null $ providesModules cu `intersect` need ]

printModProgress :: Int -> Int -> IO Int -> [HsModule] -> IO ()
printModProgress _ _ _ [] = return ()
printModProgress _ _ tickProgress ms | not progress = mapM_ (const tickProgress) ms
printModProgress fmtLen maxModules tickProgress ms = f "[" ms where
    f bl ms = do
        curModule <- tickProgress
        case ms of
            [x] -> g curModule bl "]" x
            (x:xs) -> do g curModule bl "-" x; putErrLn ""; f "-" xs
    g curModule bl el modName = putErr $ printf "%s%*d of %*d%s %-17s" bl fmtLen curModule fmtLen maxModules el (show $ hsModuleName modName)

countNodes cn = do
    seen <- newIORef Set.empty
    let h (CompNode hh deps ref) = do
            s <- readIORef seen
            if hh `Set.member` s then return Set.empty else do
                writeIORef seen (Set.insert hh s)
                ds <- mapM h deps
                cm <- readIORef ref >>= g
                return (Set.unions (cm:ds))
        g cn = case cn of
            CompLinkUnit cu      -> return $ f cu
            CompTcCollected _ cu -> return $ f cu
            CompCollected _ cu   -> return $ f cu
        f cu = case cu of
            CompTCed (_,_,_,ss) -> Set.fromList ss
            CompSources sc      -> Set.fromList (map sourceIdent sc)
            _                   -> Set.empty
    h cn

typeCheckGraph :: Opt -> CompNode -> IO ()
typeCheckGraph modOpt cn = do
    cur <- newMVar (1::Int)
    maxModules <- Set.size `fmap` countNodes cn
    let f (CompNode hh deps ref) = readIORef ref >>= \cn -> case cn of
            CompTcCollected ctc _ -> return ctc
            CompLinkUnit lu -> do
                deps' <- randomPermuteIO deps
                ctc <- mconcat `fmap` mapM f deps'
                case lu of
                    CompDummy -> do
                        writeIORef ref (CompTcCollected ctc CompDummy)
                        return ctc
                    CompHo hoh idep ho  -> do
                        let ctc' = hoTcInfo ho `mappend` ctc
                        writeIORef ref (CompTcCollected ctc' lu)
                        return ctc'
                    CompLibrary ho _libr  -> do
                        let ctc' = hoTcInfo ho `mappend` ctc
                        writeIORef ref (CompTcCollected ctc' lu)
                        return ctc'
                    CompSources sc -> do
                        let mods = sort $ map (sourceModName . sourceInfo) sc
                        modules <- forM sc $ \x -> case x of
                            SourceParsed { sourceInfo = si, sourceModule = sm } ->
                                return (sourceHash si, sm, error "SourceParsed in AnnotateSource")
                            SourceRaw { sourceInfo = si, sourceLBS = lbs } -> do
                                (mod,lbs') <- parseHsSource modOpt (sourceFP si) lbs
                                case optAnnotate modOpt of
                                    Just fp -> do
                                        let ann = LBSU.fromString $ unlines [
                                                "{- --ANNOTATE--",
                                                "Module: " ++ show (sourceModName si),
                                                "Deps: " ++ show (sort $ sourceDeps si),
                                                "Siblings: " ++ show mods,
                                                "-}"]
                                        LBS.writeFile (fp ++ "/" ++ show (hsModuleName mod) ++ ".hs") (ann `LBS.append` lbs')
                                    _ -> return ()
                                return (sourceHash si,mod,lbs')
                        showProgress (map snd3 modules)
                        (htc,tidata) <- doModules ctc (map snd3 modules)
                        let ctc' = htc `mappend` ctc
                        writeIORef ref (CompTcCollected ctc' (CompTCed ((htc,tidata,[ (x,y) | (x,y,_) <- modules],map (sourceHoName . sourceInfo) sc))))
                        return ctc'
        showProgress ms = printModProgress fmtLen maxModules tickProgress ms
        fmtLen = ceiling (logBase 10 (fromIntegral maxModules+1) :: Double) :: Int
        tickProgress = modifyMVar cur $ \val -> return (val+1,val)
    f cn
    return ()

compileCompNode :: (CollectedHo -> Ho -> IO CollectedHo)                 -- ^ Process initial ho loaded from file
                -> (CollectedHo -> Ho -> TiData  -> IO (CollectedHo,Ho)) -- ^ Process set of mutually recursive modules to produce final Ho
                -> Map.Map SourceHash (Module,[Module])
                -> CompNode
                -> IO CollectedHo
compileCompNode ifunc func ksm cn = do
    cur <- newMVar (1::Int)
    ksm_r <- newIORef ksm
    let tickProgress = modifyMVar cur $ \val -> return (val+1,val)
    maxModules <- Set.size `fmap` countNodes cn
    let showProgress ms = printModProgress fmtLen maxModules tickProgress ms
        fmtLen = ceiling (logBase 10 (fromIntegral maxModules+1) :: Double) :: Int
    let f (CompNode hh deps ref) = readIORef ref >>= g where
            g cn = case cn of
                CompCollected ch _ -> return ch
                CompTcCollected _ cl -> h cl
                CompLinkUnit cu -> h cu
            h cu = do
                deps' <- randomPermuteIO deps
                cho <- mconcat `fmap` mapM f deps'

                case cu of
                    CompDummy -> do
                        writeIORef ref (CompCollected cho CompDummy)
                        return cho
                    (CompHo hoh idep ho) -> do
                        cho <- choLibDeps_u (Map.union $ Map.fromList (hohLibDeps hoh)) `fmap` ifunc cho ho
                        writeIORef ref (CompCollected cho cu)
                        return cho
                    (CompLibrary ho Library { libHoHeader = hoh }) -> do
                        cho <- ifunc cho ho
                        let Right (ln,_) = hohName hoh
                            lh = hohHash hoh
                            cho' = (choLibDeps_u (Map.insert ln lh) cho)
                        writeIORef ref (CompCollected cho' cu)
                        return cho'
                    CompTCed ((htc,tidata,modules,shns))  -> do
                        (hdep,ldep) <- fmap mconcat . forM deps $ \ (CompNode h _ ref) -> do
                            cl <- readIORef ref
                            case compLinkCompUnit cl of
                                CompLibrary ho _ -> return ([],[hoModuleGroup ho])
                                CompDummy {} -> return ([],[])
                                _ -> return ([h],[])
                        showProgress (snds modules)
                        let (mgName:_) = sort $ map (hsModuleName . snd) modules
                        (cho',newHo) <- func cho mempty { hoModuleGroup = mgName, hoTcInfo = htc } tidata
                        modifyIORef ksm_r (Map.union $ Map.fromList [ (h,(hsModuleName mod,hsModuleRequires mod)) | (h,mod) <- modules])
                        ksm <- readIORef ksm_r
                        let hoh = HoHeader {
                                     hohVersion = error "hohVersion",
                                     hohName = Left mgName,
                                     hohHash       = hh,
                                     hohArchDeps = [],
                                     hohLibDeps   = Map.toList (choLibDeps cho')
                                     }
                            idep = HoIDeps {
                                    hoIDeps =    ksm,
                                    hoDepends    = [ (hsModuleName mod,h) | (h,mod) <- modules],
                                    hoModDepends = hdep,
                                    hoModuleGroupNeeds = ldep
                                    }
                        recordHoFile (mapHoBodies eraseE newHo) idep shns hoh
                        writeIORef ref (CompCollected cho' (CompHo hoh idep newHo))
                        return cho'
                    CompSources _ -> error "sources still exist!?"
    f cn

--hsModuleRequires x = snub (Module "Jhc.Prim":ans) where
hsModuleRequires x = snub (Module "Jhc.Prim.Prim":ans) where
--hsModuleRequires x = snub ans where
    noPrelude = FO.Prelude `Set.notMember` optFOptsSet (hsModuleOpt x)
    ans = (if noPrelude then id else  (Module "Prelude":)) [  hsImportDeclModule y | y <- hsModuleImports x]

searchPaths :: Opt -> String -> [(String,String)]
searchPaths modOpt m = ans where
    f m | (xs,'.':ys) <- span (/= '.') m = let n = (xs ++ "/" ++ ys) in m:f n
        | otherwise = [m]
    ans = [ (root ++ suf,root ++ ".ho") | i <- optIncdirs modOpt, n <- f m, suf <- [".hs",".lhs",".hsc"], let root = i ++ "/" ++ n]

mapHoBodies  :: (E -> E) -> Ho -> Ho
mapHoBodies sm ho = ho { hoBuild = g (hoBuild ho) } where
    g ho = ho { hoEs = map f (hoEs ho) , hoRules =  runIdentity (E.Rules.mapBodies (return . sm) (hoRules ho)) }
    f (t,e) = (t,sm e)

eraseE :: E -> E
eraseE e = runIdentity $ f e where
    f (EVar tv) = return $ EVar  tvr { tvrIdent = tvrIdent tv }
    f e = emapE f e

---------------------------------
-- library specific routines
---------------------------------

buildLibrary :: (CollectedHo -> Ho -> IO CollectedHo)
             -> (CollectedHo -> Ho -> TiData -> IO (CollectedHo,Ho)) -- ^ Process set of mutually recursive modules to produce final Ho
             -> FilePath
             -> IO ()
buildLibrary ifunc func = ans where
    ans fp = do
        (desc,name,vers,hmods,emods, modOpts) <- parse fp
        vers <- runReadP parseVersion vers
        let allMods = emodSet `Set.union` hmodSet
            emodSet = Set.fromList emods
            hmodSet = Set.fromList hmods
        let outName = case optOutName modOpts of
                Nothing -> name ++ "-" ++ showVersion vers ++ ".hl"
                Just fn -> fn
        -- TODO - must check we depend only on libraries
        (rnode@(CompNode lhash _ _),cho) <- parseFiles modOpts [outName] [] (map Left $ Set.toList allMods) ifunc func
        (_,(mmap,mdeps,prvds,lcor,ldef)) <- let
            f (CompNode hs cd ref) = do
                cl <- readIORef ref
                case cl of
                    CompLinkLib l _ -> return l
                    CompCollected _ y -> g hs cd ref y
            g hh deps ref cn = do
                deps <- mapM f deps
                let (mg,mll) = case cn of
                        CompDummy -> (error "modgroup of dummy",mempty)
                        CompLibrary ho lib -> (hoModuleGroup ho,mempty)
                        CompHo hoh hidep ho -> (mg,(
                                    Map.fromList $ zip (providesModules hidep) (repeat mg),
                                    Map.singleton mg (sort $ fsts deps),
                                    Set.fromList $ providesModules hidep,
                                    Map.singleton mg (hoBuild ho'),
                                    Map.singleton mg (hoTcInfo ho')
                                    )) where
                                        mg = hoModuleGroup ho
                                        ho' = mapHoBodies eraseE ho
                    res = (mg,mconcat (snds deps) `mappend` mll)
                writeIORef ref (CompLinkLib res cn)
                return res
          in f rnode
        let unknownMods = Set.toList $ Set.filter (`Set.notMember` allMods) prvds
        mapM_ ((putStrLn . ("*** Module depended on in library that is not in export list: " ++)) . show) unknownMods
        mapM_ ((putStrLn . ("*** We are re-exporting the following modules from other libraries: " ++)) . show) $ Set.toList (allMods Set.\\ prvds)
        let hoh =  HoHeader {
                hohHash = lhash,
                hohName = Right (packString name,vers),
                hohLibDeps = Map.toList (choLibDeps cho),
                hohArchDeps = [],
                hohVersion = error "hohVersion"
                }
        let pdesc = [(packString n, packString v) | (n,v) <- ("jhc-hl-filename",outName):("jhc-description-file",fp):("jhc-compiled-by",versionString):desc, n /= "exposed-modules" ]
            libr = HoLib {
                hoReexports = Map.fromList [ (m,m) | m <- Set.toList $ allMods Set.\\ prvds ],
                hoMetaInfo = pdesc,
                hoModuleMap = mmap,
                hoModuleDeps = mdeps
                }
        putProgressLn $ "Writing Library: " ++ outName
        recordHlFile Library { libHoHeader = hoh, libHoLib =  libr, libTcMap = ldef, libBuildMap = lcor, libFileName = outName }
    -- parse library description file
    parse fp = do
        putProgressLn $ "Creating library from description file: " ++ show fp
        LibDesc dlist dsing <- readDescFile fp
        when verbose2 $ do
            mapM_ print (Map.toList dlist)
            mapM_ print (Map.toList dsing)
        let jfield x = maybe (fail $ "createLibrary: description lacks required field " ++ show x) return $ Map.lookup x dsing
            mfield x = maybe [] id $ Map.lookup x dlist
            --mfield x = maybe [] (words . map (\c -> if c == ',' then ' ' else c)) $ Map.lookup x dlist
        name <- jfield "name"
        vers <- jfield "version"
        let (modOpts,flags) = (lproc bopt,modOptions) where
                Just bopt = fileOptions options modOptions `mplus` Just options
                (pfs,nfs,_) = languageFlags (mfield "extensions")
                lproc opt = opt { optFOptsSet = Set.union pfs (optFOptsSet opt) Set.\\ nfs }
                dirs = [ "-i" ++ dd x | x <- mfield "hs-source-dirs" ]
                    ++ [ "-I" ++ dd x | x <- mfield "include-dirs" ]
                    ++ [ "-p" ++ x | x <- mfield "build-depends" ]
                modOptions =  (mfield "options" ++ dirs)
                dd "." = FP.takeDirectory fp
                dd ('.':'/':x) = dd x
                dd x = FP.takeDirectory fp FP.</> x
        when verbose $
            print (flags,optFOptsSet modOpts)
        let hmods = map Module $ snub $ mfield "hidden-modules"
            emods = map Module $ snub $ mfield "exposed-modules"
        return (Map.toList dsing,name,vers,hmods,emods, modOpts)

------------------------------------
-- dumping contents of a ho file
------------------------------------

instance DocLike d => PPrint d MD5.Hash where
    pprint h = tshow h

instance DocLike d => PPrint d SrcLoc where
    pprint sl = tshow sl

instance DocLike d => PPrint d Version where
    pprint sl = text $ showVersion sl

instance DocLike d => PPrint d PackedString where
    pprint sl = text (unpackPS sl)

{-# NOINLINE dumpHoFile #-}
dumpHoFile :: String -> IO ()
dumpHoFile fn = ans where
    ans = do
        putStrLn fn
        case reverse fn of
            'l':'h':'.':_ -> doHl fn
            'o':'h':'.':_ -> doHo fn
            _ -> putErrDie "Error: --show-ho requires a .hl or .ho file"
    vindent xs = vcat (map ("    " ++) xs)
    showList nm xs = when (not $ null xs) $ putStrLn $ (nm ++ ":\n") <>  vindent xs
    doHoh hoh = do
        putStrLn $ "Version:" <+> pprint (hohVersion hoh)
        putStrLn $ "Hash:" <+> pprint (hohHash hoh)
        putStrLn $ "Name:" <+> pprint (hohName hoh)
        showList "LibDeps" (map pprint . sortUnder fst $ hohLibDeps hoh)
        showList "ArchDeps" (map pprint . sortUnder fst $ hohArchDeps hoh)
    doHl fn = do
        l <- readHlFile fn
        doHoh $ libHoHeader l
        showList "MetaInfo" (sort [text (unpackPS k) <> char ':' <+> show v |
                                   (k,v) <- hoMetaInfo (libHoLib l)])
        showList "ModuleMap" (map pprint . sortUnder fst $ Map.toList $ hoModuleMap $ libHoLib l)
        showList "ModuleDeps" (map pprint . sortUnder fst $ Map.toList $ hoModuleDeps $ libHoLib l)
        showList "ModuleReexports" (map pprint . sortUnder fst $ Map.toList $ hoReexports $ libHoLib l)
        forM_ (Map.toList $ libBuildMap l) $ \ (g,hoB) -> do
            print g
            doHoB hoB
    doHo fn = do
        (hoh,idep,ho) <- readHoFile fn
        doHoh hoh
        let hoB = hoBuild ho
            hoE = hoTcInfo ho
        showList "Dependencies" (map pprint . sortUnder fst $ hoDepends idep)
        showList "ModDependencies" (map pprint $ hoModDepends idep)
        showList "IDepCache" (map pprint . sortUnder fst $ Map.toList $ hoIDeps idep)
        putStrLn $ "Modules contained:" <+> tshow (keys $ hoExports hoE)
        putStrLn $ "number of definitions:" <+> tshow (size $ hoDefs hoE)
        putStrLn $ "hoAssumps:" <+> tshow (size $ hoAssumps hoE)
        putStrLn $ "hoFixities:" <+> tshow (size $  hoFixities hoE)
        putStrLn $ "hoKinds:" <+> tshow (size $  hoKinds hoE)
        putStrLn $ "hoClassHierarchy:" <+> tshow (size $  hoClassHierarchy hoE)
        putStrLn $ "hoTypeSynonyms:" <+> tshow (size $  hoTypeSynonyms hoE)
        wdump FD.Exports $ do
            putStrLn "---- exports information ----";
            putStrLn $  (pprint $ hoExports hoE :: String)
        wdump FD.Defs $ do
            putStrLn "---- defs information ----";
            putStrLn $  (pprint $ hoDefs hoE :: String)
        when (dump FD.Kind) $ do
            putStrLn "---- kind information ----";
            putStrLn $  (pprint $ hoKinds hoE :: String)
        when (dump FD.ClassSummary) $ do
            putStrLn "---- class summary ---- "
            printClassSummary (hoClassHierarchy hoE)
        when (dump FD.Class) $
             do {putStrLn "---- class hierarchy ---- ";
                 printClassHierarchy (hoClassHierarchy hoE)}
        wdump FD.Types $ do
            putStrLn " ---- the types of identifiers ---- "
            putStrLn $ PPrint.render $ pprint (hoAssumps hoE)
        doHoB hoB
    doHoB hoB = do
        putStrLn $ "hoDataTable:" <+> tshow (size $  hoDataTable hoB)
        putStrLn $ "hoEs:" <+> tshow (size $  hoEs hoB)
        putStrLn $ "hoRules:" <+> tshow (size $  hoRules hoB)
        let rules = hoRules hoB
        wdump FD.Rules $ putStrLn "  ---- user rules ---- " >> printRules RuleUser rules
        wdump FD.Rules $ putStrLn "  ---- user catalysts ---- " >> printRules RuleCatalyst rules
        wdump FD.RulesSpec $ putStrLn "  ---- specializations ---- " >> printRules RuleSpecialization rules
        wdump FD.Datatable $ do
             putStrLn "  ---- data table ---- "
             putDocM putStr (showDataTable (hoDataTable hoB))
             putChar '\n'
        wdump FD.Core $ do
            putStrLn " ---- lambdacube  ---- "
            mapM_ (\ (v,lc) -> putChar '\n' >> printCheckName'' (hoDataTable hoB) v lc) (hoEs hoB)
    printCheckName'' :: DataTable -> TVr -> E -> IO ()
    printCheckName'' _dataTable tvr e = do
        when (dump FD.EInfo || verbose2) $ putStrLn (show $ tvrInfo tvr)
        putStrLn (render $ hang 4 (pprint tvr <+> text "::" <+> pprint (tvrType tvr)))
        putStrLn (render $ hang 4 (pprint tvr <+> equals <+> pprint e))
