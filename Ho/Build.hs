module Ho.Build (
    module Ho.Type,
    dumpHoFile,
    findModule,
    hoToProgram,
    buildLibrary
    ) where


import Codec.Compression.GZip
import Control.Monad.Identity
import Data.Binary
import Data.Monoid
import Data.IORef
import Data.Tree
import Data.List hiding(union)
import Maybe
import Monad
import Prelude hiding(print,putStrLn)
import System.IO hiding(print,putStrLn)
import System.Posix.Files
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.PrettyPrint.HughesPJ as PPrint

import StringTable.Atom
import PackedString(packString)
import CharIO
import DataConstructors
import Directory
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.E
import E.Program
import E.Rules
import E.Show
import E.Traverse(emapE)
import E.TypeCheck()
import FrontEnd.Class
import FrontEnd.HsParser
import FrontEnd.Infix
import FrontEnd.ParseMonad
import FrontEnd.Syn.Options
import FrontEnd.Unlit
import FrontEnd.Warning
import FrontEnd.SrcLoc
import Ho.Binary()
import Ho.Library
import Ho.Type
import HsSyn
import Options
import Support.CFF
import Util.FilterInput
import Util.Gen hiding(putErrLn,putErr,putErrDie)
import Util.SetLike
import Version.Version(versionString)
import qualified Data.ByteString as BS
import qualified FlagDump as FD
import qualified FlagOpts as FO
import qualified Util.Graph as G
import qualified Support.MD5 as MD5


--
-- Ho File Format
--
-- ho files are standard CFF format files (PNG-like) as described in the Support.CFF modules.
--
-- the CFF magic for the files is the string "JHC"
--
-- JHDR - header info, contains a list of modules contained and dependencies that need to be checked to read the file
-- LIBR - only present if this is a library, contains library metainfo
-- TCIN - type checking information
-- CORE - compiled core and associated data
-- GRIN - compiled grin code
--
--

cff_magic = chunkType "JHC"
cff_jhdr  = chunkType "JHDR"
cff_core  = chunkType "CORE"


shortenPath :: String -> IO String
shortenPath x@('/':_) = do
    cd <- getCurrentDirectory
    pwd <- lookupEnv "PWD"
    h <- lookupEnv "HOME"
    --print (x,cd,h)
    let f d = d >>= \d -> getPrefix d x >>= \ ('/':rest) -> return rest
    return $ fromJust $ f (return cd) `mplus` f pwd `mplus` liftM ("~/" ++) (f h) `mplus` return x
shortenPath x = return x



instance DocLike d => PPrint d MD5.Hash where
    pprintPrec _ h = tshow h


type FileName = String

findFirstFile :: String -> [(String,a)] -> IO (Handle,MD5.Hash,FileName,a)
findFirstFile err [] = FrontEnd.Warning.err "missing-dep" ("Module not found: " ++ err) >> return (undefined,MD5.emptyHash,undefined,undefined)
findFirstFile err ((x,a):xs) = flip catch (\e ->   findFirstFile err xs) $ do
    fh <- openBinaryFile x ReadMode
    hash <- MD5.md5Handle fh
    return (fh,hash,x,a)


type DoneMap = IORef (Map.Map Module ModuleDone)

data ModuleDone =
    -- final classifications
    ModuleNotThere
    | ModuleParsed {
        modParsed :: HsModule,
        modName :: String,
        modHoName :: String,
        modHash :: MD5.Hash
        }
    | ModuleHo HoHeader Ho
    -- temporary classifications
    | ModuleNoHo


type MRet = [Module]

fetchModule :: DoneMap -> Either Module String -> IO MRet
fetchModule r_dm (Right n) = lookupModule r_dm True (Right n)
fetchModule r_dm (Left m) = ans where
    ans = do
        dm <- readIORef r_dm
        case Map.lookup m dm of
            Nothing -> lookupModule r_dm True (Left m)
            Just ModuleNoHo -> lookupModule r_dm False (Left m)
            -- final results
            Just ModuleNotThere -> return []
            Just ModuleParsed { modParsed = hs, modHash =  s1h } -> return $ [hsModuleName hs]
            Just (ModuleHo hoh _) -> return $ fsts (hohDepends hoh)


fillInHohHash :: HoHeader -> HoHeader
fillInHohHash hoh = hoh { hohHash = h } where
    h = MD5.md5Bytes $ concatMap (MD5.hashToBytes . snd) $ sort (hohDepends hoh)

checkHoFile :: DoneMap -> Module -> IO (HoHeader,Ho)
checkHoFile r_dm m = do
    dm <- readIORef r_dm
    case mlookup m dm of
        Just (ModuleHo hoh ho) -> return (hoh,ho)
        Just _ -> fail "checkHoFile"
        Nothing -> do
            (_,_,_,ho_name) <- moduleFind r_dm (Left m)
            loadHoFile r_dm ho_name

-- perhaps load a single ho file
loadHoFile :: DoneMap -> FileName -> IO (HoHeader,Ho)
loadHoFile r_dm ho_name = ans where
    ans = do
        ho_name' <- shortenPath ho_name
        Just (hoh,ho) <- checkForHoFile ho_name
        let cd (m,h) | h /= MD5.emptyHash = do
                (_,h',fn,_) <- moduleFind r_dm (Left m)
                unless (h == h') $ do
                    fn <- shortenPath fn
                    putVerboseLn $ ho_name' <+> "is out of date due to changed file:" <+> fn
                    True <- return False
                    return ()
            cd _ = return ()
            cd' (m,h) = do
                (h',_) <- checkHoFile r_dm m
                unless (h == hohHash h') $ do
                    putVerboseLn $ ho_name' <+> "is out of date due to modified ho file:" <+> (show m)
                    True <- return False
                    return ()
        flip catch (\e -> poison r_dm (map fst $ hohDepends hoh) >> ioError e) $ do
        mapM_ cd (hohDepends hoh)
        mapM_ cd' (hohModDepends hoh)
        putVerboseLn $ "Found ho file:   " <+> ho_name'
        forM_ (fsts $ hohDepends hoh) $ \m -> do
            modifyIORef r_dm (Map.insert m (ModuleHo hoh ho))
        return (hoh,ho)

poison :: DoneMap -> [Module] -> IO ()
poison r_dm xs = mapM_ f xs where
    f m = do
        dm <- readIORef r_dm
        case m `mlookup` dm of
            Nothing -> modifyIORef r_dm (minsert m ModuleNoHo)
            _ -> return ()

moduleFind :: DoneMap -> Either Module String -> IO (Handle,MD5.Hash,FileName,FileName)
moduleFind r_dm (Right n) = findFirstFile n [(n,reverse $ 'o':'h':dropWhile (/= '.') (reverse n))]
moduleFind r_dm (Left m) = do
    dm <- readIORef r_dm
    let (name,spath) = (fromModule m, searchPaths (fromModule m))
    case m `mlookup` dm of
        Nothing -> findFirstFile name spath
        Just _ -> do fail "moduleFind found"


checkTheHoFile r_dm ho_name = do
    --putVerboseLn $ "checkTheHoFile:" <+> ho_name
    flip catch (\e -> return Nothing) $ Just `fmap` loadHoFile r_dm ho_name


lookupModule :: DoneMap -> Bool -> Either Module String -> IO MRet
lookupModule r_dm useHo ms = do
        dm <- readIORef r_dm
        let (name,spath) = case ms of
                Left m -> (fromModule m, searchPaths (fromModule m))
                Right n -> (n,[(n,reverse $ 'o':'h':dropWhile (/= '.') (reverse n))])
            nogood = case ms of
                Left m -> modifyIORef r_dm (Map.insert m ModuleNotThere) >> return []
                Right n -> return []
        (fh,hash,fname,ho_name) <- findFirstFile name spath
        if hash == MD5.emptyHash then nogood else do
        mho <- if useHo && not (optIgnoreHo options) then checkTheHoFile r_dm ho_name else return Nothing
        case mho of
            Just (hoh,_) -> return $ fsts (hohDepends hoh)
            Nothing -> do
                hs <- parseHsSource fname fh
                wdump FD.Progress $ do
                    sp <- shortenPath fname
                    putErrLn $ "Found dependency:" <+> name <+> "at" <+> sp
                --if hsModuleName hs `mmember` dm then do putStrLn $ "Found a module name we already have: " ++ show (hsModuleName hs); nogood else do
                modifyIORef r_dm (Map.insert (hsModuleName hs) ModuleParsed { modParsed = hs, modHoName = ho_name, modName = fname, modHash = hash })
                mapM_ (fetchModule r_dm) $  map Left (hsModuleRequires hs)
                return $ [hsModuleName hs]





findModule :: [Either Module String]                             -- ^ Either a module or filename to find
              -> (CollectedHo -> Ho -> IO CollectedHo)              -- ^ Process initial ho loaded from file
              -> (CollectedHo -> [HsModule] -> IO (CollectedHo,Ho)) -- ^ Process set of mutually recursive modules to produce final Ho
              -> IO (CollectedHo,[(Module,MD5.Hash)],Ho)                                -- ^ (Final accumulated ho,just the ho read to satisfy this command)
findModule need ifunc func  = do
    r_dm <- newIORef Map.empty

    putVerboseLn $ "Loading libraries:" <+> show (optHls options)
    forM_ (optHls options) $ \l -> do
        (n',fn) <- findLibrary l
        putVerboseLn $ "Found" <+> show (l,n') <+> "at" <+> fn
        checkTheHoFile r_dm fn
    ms <- mapM (fetchModule r_dm) need
    processIOErrors
    let f (m:xs) ds | m `member` ds = f xs ds
        f (m:xs) ds = do
            Just mp <- Map.lookup m `fmap` readIORef r_dm
            case mp of
                ModuleParsed { modParsed = hs, modHash = fd, modHoName = s } -> do
                    (mp,readHo,libHo,rs) <- f (hsModuleRequires hs ++ xs) (Set.insert m ds)
                    return (mp `mappend` mempty { choFiles = Map.singleton m fd },readHo,libHo,((hs,(m,fd),s):rs))
                ModuleHo hoh ho -> do
                    let ss = Set.fromList $ fsts (hohDepends hoh)
                    (mp,readHo,(libDeps,libHo),rs) <- f (fsts (hohModDepends hoh) ++ xs) (Set.union ss ds)
                    let mp' = mp `mappend` mempty { choModules = mprovides hoh, choFiles = Map.fromList $ hohDepends hoh }
                        ho' = ho `mappend` readHo
                    case hohMetaInfo hoh of
                        [] -> return (mp', ho',(libDeps,libHo `mappend` ho),rs)
                        _  -> return (mp', ho',((m,hohHash hoh):libDeps,libHo),rs)
                ModuleNotThere -> fail $ "Module not found:" <+> show m
                ModuleNoHo -> fail $ "Module noho:" <+> show m
        f [] _ = return (mempty,mempty,mempty,mempty)
        mprovides hoh = Map.fromList [ (x,hohHash hoh) | (x,_) <- hohDepends hoh]
    (cho,readHo,(libDeps,libHo), ms) <- f (concat ms) Set.empty
    writeIORef r_dm (error "r_dm")  -- to encourage garbage collection.
    let mgraph =  (G.newGraph ms (fromModule . hsModuleName . fst3) (hsModuleRequires' . fst3) )
        scc = G.sccGroups mgraph
        mgraph' =  (G.newGraph scc (fromModule . hsModuleName . fst3 . head) (concatMap ff . concatMap (hsModuleRequires' . fst3)) )
        ff n = [ fromModule . hsModuleName $ ms | gs@((ms,_,_):_) <- scc, n `elem` map (fromModule . hsModuleName . fst3) gs]
    when (dump FD.SccModules) $ do
        CharIO.putErrLn $ "scc modules:\n" ++ unlines ( map  (\xs -> show [ hsModuleName x | (x,y,z) <- xs ]) scc)
        putErrLn $ drawForest (map (fmap (show . map (hsModuleName . fst3))) (G.dff mgraph'))

    let f ho libHo [] = processIOErrors >> return (ho,libDeps,libHo)
        f ho libHo (sc:scs) = do
            (cho',newHo) <- func ho [ hs | (hs,_,_) <- sc ]
            let mods = [ hsModuleName hs | (hs,_,_) <- sc ]
                mods' = snub [ m  | (hs,_,_) <- sc, m <- hsModuleRequires hs, m `notElem` mods]
                mdeps = [ (m,dep) | m <- mods', dep <- Map.lookup m (choModules cho')]
            let hoh = fillInHohHash HoHeader {
                                 hohDepends    = [ x | (_,x,_) <- sc],
                                 hohModDepends = mdeps,
                                 hohHash = undefined,
                                 hohMetaInfo   = []
                               }
            recordHoFile newHo [ x | (_,_,x) <- sc ] hoh
            f (cho' `mappend` mempty { choFiles = Map.fromList $ hohDepends hoh, choModules = mprovides hoh }) (libHo `mappend` newHo)  scs

    cho <- ifunc cho (mempty { hoDataTable = dataTablePrims } `mappend` readHo)
    f cho libHo scc




-- Read in a Ho file.

checkForHoFile :: String -> IO (Maybe (HoHeader,Ho))
checkForHoFile fn = flip catch (\e -> return Nothing) $ Just `fmap` readHoFile fn

readHoFile :: String -> IO (HoHeader,Ho)
readHoFile fn = do
    bs <- BS.readFile fn
    (ct,mp) <- bsCFF bs
    True <- return $ ct == cff_magic
    Just rhh <- return $ lookup cff_jhdr mp
    Just rho <- return $ lookup cff_core mp
    let hh = decode (decompress $ L.fromChunks [rhh])
    let ho = decode (decompress $ L.fromChunks [rho])
    return (hh,ho)


recordHoFile ::
    Ho               -- ^ File to record
    -> [FileName]    -- ^ files to write to
    -> HoHeader      -- ^ file header
    -> IO ()
recordHoFile ho fs header = do
    if optNoWriteHo options then do
        wdump FD.Progress $ do
            fs' <- mapM shortenPath fs
            putErrLn $ "Skipping Writing Ho Files: " ++ show fs'
      else do
    let removeLink' fn = catch  (removeLink fn)  (\_ -> return ())
    let g (fn:fs) = do
            f fn
            mapM_ (l fn) fs
            return ()
        g [] = error "Ho.g: shouldn't happen"
        l fn fn' = do
            wdump FD.Progress $ do
                fn_ <- shortenPath fn
                fn_' <- shortenPath fn'
                when (optNoWriteHo options) $ putErr "Skipping "
                putErrLn $ "Linking haskell object file:" <+> fn_' <+> "to" <+> fn_
            if optNoWriteHo options then return () else do
            let tfn = fn' ++ ".tmp"
            removeLink' tfn
            createLink fn tfn
            rename tfn fn'
        f fn = do
            wdump FD.Progress $ do
                when (optNoWriteHo options) $ putErr "Skipping "
                fn' <- shortenPath fn
                putErrLn $ "Writing haskell object file:" <+> fn'
            if optNoWriteHo options then return () else do
            let tfn = fn ++ ".tmp"
            fh <- openBinaryFile tfn WriteMode
            let theho =  mapHoBodies eraseE ho
            lazyWriteCFF fh cff_magic [(cff_jhdr, compress $ encode header),(cff_core, compress $ encode theho)]
            hFlush fh
            hClose fh
            rename tfn fn
    g fs


hsModuleRequires' = map fromModule . hsModuleRequires

hsModuleRequires x = Module "Jhc.Prim":ans where
    noPrelude =   or $ not (optPrelude options):[ opt == c | opt <- hsModuleOptions x, c <- ["-N","--noprelude"]]
    ans = snub $ (if noPrelude then id else  (Module "Prelude":)) [  hsImportDeclModule y | y <- hsModuleImports x]

searchPaths :: String -> [(String,String)]
searchPaths m = ans where
    f m | (xs,'.':ys) <- span (/= '.') m = let n = (xs ++ "/" ++ ys) in m:f n
        | otherwise = [m]
    ans = [ (root ++ suf,root ++ ".ho") | i <- optIncdirs options, n <- f m, suf <- [".hs",".lhs"], let root = i ++ "/" ++ n]


parseHsSource :: String -> Handle -> IO HsModule
parseHsSource fn fh = do
    pos <- hGetPosn fh
    ls <- replicateM 15 (ioM $ hGetLine fh)
    let f s = opt where
            Just opt = fileOptions opts `mplus` Just options where
            s' = if "shl." `isPrefixOf` reverse fn  then unlit fn s else s
            opts = concat [ words as | (x,as) <- parseOptions s', x `elem` ["OPTIONS","JHC_OPTIONS","OPTIONS_JHC"]]
    let fopts s = s `member` optFOptsSet opt where opt = f (concatMap concat ls)
    hSetPosn pos
    s <- case () of
        _ | fopts FO.Cpp -> hClose fh >> readSystem "cpp" ["-D__JHC__","-CC","-traditional", "--", fn]
          | fopts FO.M4 ->  hClose fh >> readSystem "m4" ["-D__JHC__", "-s", fn]
          | otherwise -> CharIO.hGetContents fh
    let s' = if "shl." `isPrefixOf` reverse fn  then unlit fn s'' else s''
        s'' = case s of
            '#':' ':_   -> '\n':s                --  line pragma
            '#':'l':'i':'n':'e':' ':_  -> '\n':s --  line pragma
            '#':'!':_ -> dropWhile (/= '\n') s   --  hashbang
            _ -> s
    case runParserWithMode (parseModeOptions $ f s) { parseFilename = fn } parse  s'  of
                      ParseOk ws e -> processErrors ws >> return e
                      ParseFailed sl err -> putErrDie $ show sl ++ ": " ++ err


mapHoBodies  :: (E -> E) -> Ho -> Ho
mapHoBodies sm ho = ho { hoEs = map f (hoEs ho) , hoRules =  runIdentity (E.Rules.mapBodies (return . sm) (hoRules ho)) } where
    f (t,e) = (t,sm e)


eraseE :: E -> E
eraseE e = runIdentity $ f e where
    f (EVar tv) = return $ EVar  tvr { tvrIdent = tvrIdent tv }
    f e = emapE f e




hoToProgram :: Ho -> Program
hoToProgram ho = programSetDs (hoEs ho) program {
    progClassHierarchy = hoClassHierarchy ho,
    progDataTable = hoDataTable ho
    }

---------------------------------
-- library specific routines
---------------------------------

buildLibrary :: (CollectedHo -> Ho -> IO CollectedHo)
             -> (CollectedHo -> [HsModule] -> IO (CollectedHo,Ho))
             -> FilePath
             -> IO ()
buildLibrary ifunc func = ans where
    ans fp = do
        (desc,name,hmods,emods) <- parse fp
        let allmods  = sort (emods ++ hmods)
        (cho,libDeps,ho) <- findModule (map Left (emods ++ hmods)) ifunc func
        let unknownMods = [ m | m <- mkeys (hoExports ho), m `notElem` allmods  ]
        mapM_ ((putStrLn . ("*** Module included in library that is not in export list: " ++)) . show) unknownMods
        let outName = case optOutName options of
                "hs.out" -> name ++ ".hl"
                fn -> fn
        let pdesc = [(toAtom n, packString v) | (n,v) <- ("jhc-hl-filename",outName):("jhc-description-file",fp):("jhc-compiled-by",versionString):desc, n /= "exposed-modules" ]
        let lhash = MD5.md5String (show $ choFiles cho)
        let hoh =  HoHeader {
                hohHash = lhash,
                hohDepends = [ (m,MD5.emptyHash) | m <- mkeys (hoExports ho)],
                hohModDepends = libDeps,
                hohMetaInfo = pdesc
                }
        recordHoFile ho [outName] hoh

    -- parse library description file
    parse fp = do
        putVerboseLn $ "Creating library from description file: " ++ show fp
        desc <- readDescFile fp
        when verbose2 $ mapM_ print desc
        let field x = lookup x desc
            jfield x = maybe (fail $ "createLibrary: description lacks required field " ++ show x) return $ field x
            mfield x = maybe [] (words . map (\c -> if c == ',' then ' ' else c)) $ field x
        name <- jfield "name"
        vers <- jfield "version"
        let hmods = map Module $ snub $ mfield "hidden-modules"
            emods = map Module $ snub $ mfield "exposed-modules"
        return (desc,name ++ "-" ++ vers,hmods,emods)


------------------------------------
-- dumping contents of a ho file
------------------------------------


instance DocLike d => PPrint d SrcLoc where
    pprint sl = tshow sl

{-# NOINLINE dumpHoFile #-}
dumpHoFile :: String -> IO ()
dumpHoFile fn = do
    (hoh,ho) <- readHoFile fn
    putStrLn fn
    when (not $ Prelude.null (hohDepends hoh)) $ putStrLn $ "Dependencies:\n" <>  vcat (map pprint $ sortUnder fst $ hohDepends hoh)
    when (not $ Prelude.null (hohModDepends hoh)) $ putStrLn $ "ModDependencies:\n" <>  vcat (map pprint $ sortUnder fst $ hohModDepends hoh)
    putStrLn $ "HoHash:" <+> pprint (hohHash hoh)
    putStrLn $ "MetaInfo:\n" <> vcat (sort [text (' ':' ':fromAtom k) <> char ':' <+> show v | (k,v) <- hohMetaInfo hoh])
    putStrLn $ "Modules contained:" <+> tshow (mkeys $ hoExports ho)
    putStrLn $ "number of definitions:" <+> tshow (size $ hoDefs ho)
    putStrLn $ "hoAssumps:" <+> tshow (size $ hoAssumps ho)
    putStrLn $ "hoFixities:" <+> tshow (size $  hoFixities ho)
    putStrLn $ "hoKinds:" <+> tshow (size $  hoKinds ho)
    putStrLn $ "hoClassHierarchy:" <+> tshow (size $  hoClassHierarchy ho)
    putStrLn $ "hoTypeSynonyms:" <+> tshow (size $  hoTypeSynonyms ho)
    putStrLn $ "hoDataTable:" <+> tshow (size $  hoDataTable ho)
    putStrLn $ "hoEs:" <+> tshow (size $  hoEs ho)
    putStrLn $ "hoRules:" <+> tshow (size $  hoRules ho)
    wdump FD.Exports $ do
        putStrLn "---- exports information ----";
        CharIO.putStrLn $  (pprint $ hoExports ho :: String)
    wdump FD.Defs $ do
        putStrLn "---- defs information ----";
        CharIO.putStrLn $  (pprint $ hoDefs ho :: String)
    when (dump FD.Kind) $ do
        putStrLn "---- kind information ----";
        CharIO.putStrLn $  (pprint $ hoKinds ho :: String)
    when (dump FD.ClassSummary) $ do
        putStrLn "---- class summary ---- "
        printClassSummary (hoClassHierarchy ho)
    when (dump FD.Class) $
         do {putStrLn "---- class hierarchy ---- ";
             printClassHierarchy (hoClassHierarchy ho)}
    let rules = hoRules ho
    wdump FD.Rules $ putStrLn "  ---- user rules ---- " >> printRules RuleUser rules
    wdump FD.Rules $ putStrLn "  ---- user catalysts ---- " >> printRules RuleCatalyst rules
    wdump FD.RulesSpec $ putStrLn "  ---- specializations ---- " >> printRules RuleSpecialization rules
    wdump FD.Datatable $ do
         putStrLn "  ---- data table ---- "
         putDocM CharIO.putStr (showDataTable (hoDataTable ho))
         putChar '\n'
    wdump FD.Types $ do
        putStrLn " ---- the types of identifiers ---- "
        putStrLn $ PPrint.render $ pprint (hoAssumps ho)
    wdump FD.Core $ do
        putStrLn " ---- lambdacube  ---- "
        mapM_ (\ (v,lc) -> putChar '\n' >> printCheckName'' (hoDataTable ho) v lc) (hoEs ho)
    where
    printCheckName'' :: DataTable -> TVr -> E -> IO ()
    printCheckName'' _dataTable tvr e = do
        when (dump FD.EInfo || verbose2) $ putStrLn (show $ tvrInfo tvr)
        putStrLn (render $ hang 4 (pprint tvr <+> text "::" <+> pprint (tvrType tvr)))
        putStrLn (render $ hang 4 (pprint tvr <+> equals <+> pprint e))

