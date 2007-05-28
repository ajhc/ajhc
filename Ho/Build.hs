module Ho.Build (
    module Ho.Type,
    dumpHoFile,
    findModule,
    hoToProgram,
    initialHo,
    recordHoFile,
    checkForHoFile
    ) where


import Codec.Compression.GZip
import Control.Monad.Identity
import Data.Binary
import Data.Monoid
import Data.Tree
import IO(bracket)
import List
import Maybe
import Monad
import Prelude hiding(print,putStrLn)
import System.IO hiding(print,putStrLn)
import System.Posix.Files
import System.Posix.IO
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import qualified Text.PrettyPrint.HughesPJ as PPrint

import Atom
import CharIO
import FrontEnd.Class
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
import FrontEnd.HsParser
import FrontEnd.Infix
import FrontEnd.ParseMonad
import FrontEnd.Unlit
import FrontEnd.Syn.Options
import GenUtil hiding(putErrLn,putErr,putErrDie)
import Ho.Type
import Ho.Binary
import Ho.LibraryMap
import HsSyn
import Options
import PackedString
import Util.FilterInput
import Util.SetLike
import FrontEnd.Warning
import qualified FlagDump as FD
import qualified FlagOpts as FO
import qualified Util.Graph as G

version :: Int
version = 7

magic = (packString "jhc Haskell Object File",version)
magic2 = packString "John's Haskell Compiler"


shortenPath :: String -> IO String
shortenPath x@('/':_) = do
    cd <- getCurrentDirectory
    pwd <- lookupEnv "PWD"
    h <- lookupEnv "HOME"
    --print (x,cd,h)
    let f d = d >>= \d -> getPrefix d x >>= \ ('/':rest) -> return rest
    return $ fromJust $ f (return cd) `mplus` f pwd `mplus` liftM ("~/" ++) (f h) `mplus` return x
shortenPath x = return x



emptyFileDep = FileDep mempty 0 mempty 0 0

instance Eq FileDep where
    a == b = map ($ a) fs == map ($ b) fs && fileDeviceID a == fileDeviceID b where
        fs = [fileModifyTime,fileFileID,fileFileSize]

instance DocLike d => PPrint d FileDep where
    pprint fd = tshow (fileName fd) <> char ':' <+> tshow (fileModifyTime fd)

toFileDep fn fs = FileDep {
    fileName = toAtom fn
    ,fileModifyTime = fromEnum (modificationTime fs)
    ,fileDeviceID = toAtom $ show (deviceID fs)
    ,fileFileID = fromIntegral (fileID fs)
    ,fileFileSize = fromIntegral (fileSize fs)
    }

findFirstFile :: String -> [(String,a)] -> IO (Handle,FileDep,a)
findFirstFile err [] = FrontEnd.Warning.err "missing-dep" ("Module not found: " ++ err) >> return (undefined,emptyFileDep,undefined)
findFirstFile err ((x,a):xs) = flip catch (\e ->   findFirstFile err xs) $ do
    (fh,fd) <- openGetFileDep x
    return (fh,fd,a)



findModule :: CollectedHo                                           -- ^ Accumulated Ho
              -> [Either Module String]                             -- ^ Either a module or filename to find
              -> (CollectedHo -> Ho -> IO CollectedHo)              -- ^ Process initial ho loaded from file
              -> (CollectedHo -> [HsModule] -> IO (CollectedHo,Ho)) -- ^ Process set of mutually recursive modules to produce final Ho
              -> IO (CollectedHo,Ho)                                -- ^ (Final accumulated ho,just the ho read to satisfy this command)
findModule cho need ifunc func  = do
    let f (Left (Module m)) = Left (Module m)
        f (Right n) = Right (n,[(n,reverse $ 'o':'h':dropWhile (/= '.') (reverse n))])
    (readHo,ms) <- nextModule (fmap Just . hoModules . choHo $ cho) [] mempty (map f (snub need))
    processIOErrors
    let mgraph =  (G.newGraph ms (fromModule . hsModuleName . fst3) (hsModuleRequires . fst3) )
        scc = G.sccGroups mgraph
        mgraph' =  (G.newGraph scc (fromModule . hsModuleName . fst3 . head) (concatMap ff . concatMap (hsModuleRequires . fst3)) )
        ff n = [ fromModule . hsModuleName $ ms | gs@((ms,_,_):_) <- scc, n `elem` map (fromModule . hsModuleName . fst3) gs]
    when (dump FD.SccModules) $ do
        CharIO.putErrLn $ "scc modules:\n" ++ unlines ( map  (\xs -> show [ hsModuleName x | (x,y,z) <- xs ]) scc)
        putErrLn $ drawForest (map (fmap (show . map (hsModuleName . fst3))) (G.dff mgraph'))
    let f ho readHo [] = return (ho,readHo)
        f ho readHo (sc:scs) = do
            (cho',newHo) <- func ho [ hs | (hs,_,_) <- sc ]
            let mods = [ hsModuleName hs | (hs,_,_) <- sc ]
                mods' = snub [ Module m  | (hs,_,_) <- sc, m <- hsModuleRequires hs, Module m `notElem` mods]
                mdeps = [ (m,dep) | m <- mods', Left dep <- Map.lookup m (hoModules . choHo $ cho')]
                ldeps = Map.fromList [ x | m <- mods', Right x <- Map.lookup m (hoModules . choHo $ cho)]
            let hoh = HoHeader { hohDepends    = [ x | (_,x,_) <- sc],
                                 hohModDepends = mdeps,
                                 hohMetaInfo   = []
                               }
            newHo <- return (newHo `mappend` mempty { hoLibraries = ldeps })
            newHo <- recordHoFile newHo [ x | (_,_,x) <- sc ] hoh
            f (cho' `mappend` collectedHo { choHo = mempty { hoModules = hoModules newHo }}) (readHo `mappend` newHo)  scs
    ho <- ifunc cho readHo
    f ho readHo scc


checkForHoFile :: String            -- ^ file name to check for
    -> IO (Maybe (HoHeader,Ho))
checkForHoFile fn = flip catch (\e -> return Nothing) $ do
    bracket (openGetFileDep fn) (\_ -> return ()) $ \ (fh,dep) -> do
    lbs <- L.hGetContents fh
    let (x,hh,ho,m2) = decode (decompress lbs)
    if x /= magic then (putErrLn $ "Bad ho file:" <+> fn)  >> return Nothing else do
    xs <- mapM checkDep (hohDepends hh)
    if not (and xs) then  return Nothing else do
        if m2 /= magic2 then (putErrLn $ "Bad ho file:" <+> fn)  >>  return Nothing else do
        wdump FD.Progress $ do
            fn' <- shortenPath fn
            putErrLn $ "Found object file:" <+> fn'
        if (all (`elem` loadedLibraries) (Map.keys $ hoLibraries ho)) then do
            return $ Just (hh,ho { hoModules = fmap (const (Left dep)) (hoExports ho) })
         else do
            putErrLn $ "No library dep for ho file:" <+> fn
            return Nothing


checkDep fd = do
    fs <- getFileStatus (fromAtom $ fileName fd)
    return (fd == toFileDep (fileName fd) fs)


-- | This reads in an entire ho file for diagnostic purposes.
readHoFile :: String -> IO (HoHeader,Ho)
readHoFile fn = do
    fh <- openBinaryFile fn ReadMode
    c <- L.hGetContents fh
    let (m1,hh,ho,m2) = decode (decompress c)
    when (m1 /= magic) (putErrDie $ "Bad ho file magic1:" <+> fn)
    when (m2 /= magic2) (putErrDie $ "Bad ho file magic2:" <+> fn)
    return (hh,ho)

{-# NOINLINE dumpHoFile #-}
dumpHoFile :: String -> IO ()
dumpHoFile fn = do
    (hoh,ho) <- readHoFile fn
    putStrLn fn
    when (not $ Prelude.null (hohDepends hoh)) $ putStrLn $ "Dependencies:" <+>  pprint (sortUnder (show . fileName) $ hohDepends hoh)
    when (not $ Prelude.null (hohDepends hoh)) $ putStrLn $ "ModDependencies:" <+>  pprint (sortUnder fst $ hohModDepends hoh)
    putStrLn $ "MetaInfo:\n" <> vcat (sort [text (' ':' ':unpackPS k) <> char ':' <+> show v | (k,v) <- hohMetaInfo hoh])
    putStrLn $ "Libraries depended on:" <+> pprint (sort $ Map.toList $ hoLibraries ho)
    putStrLn $ "Modules contained:" <+> tshow (mkeys $ hoExports ho)
    putStrLn $ "number of definitions:" <+> tshow (size $ hoDefs ho)
    putStrLn $ "hoAssumps:" <+> tshow (size $ hoAssumps ho)
    putStrLn $ "hoFixities:" <+> tshow (size $  hoFixities ho)
    putStrLn $ "hoKinds:" <+> tshow (size $  hoKinds ho)
    putStrLn $ "hoClassHierarchy:" <+> tshow (size $  hoClassHierarchy ho)
    putStrLn $ "hoTypeSynonyms:" <+> tshow (size $  hoTypeSynonyms ho)
    putStrLn $ "hoDataTable:" <+> tshow (size $  hoDataTable ho)
    putStrLn $ "hoEs:" <+> tshow (size $  hoEs ho)
    putStrLn $ "hoProps:" <+> tshow (size $  hoProps ho)
    putStrLn $ "hoRules:" <+> tshow (size $  hoRules ho)
    when (dump FD.Kind) $ do
        putStrLn " \n ---- kind information ---- \n";
        CharIO.putStrLn $  (pprint $ hoKinds ho :: String) -- pprintEnvMap kindInfo}
    when (dump FD.ClassSummary) $ do
        putStrLn "  ---- class summary ---- "
        printClassSummary (hoClassHierarchy ho)
    when (dump FD.Class) $
         do {putStrLn "  ---- class hierarchy ---- ";
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
        mapM_ (\ (v,lc) -> putChar '\n' >> printCheckName'' (hoDataTable ho) v lc) (melems $ hoEs ho)
    where
    printCheckName'' :: DataTable -> TVr -> E -> IO ()
    printCheckName'' _dataTable tvr e = do
        when (dump FD.EInfo || verbose2) $ putStrLn (show $ tvrInfo tvr)
        putStrLn (render $ hang 4 (pprint tvr <+> text "::" <+> pprint (tvrType tvr)))
        putStrLn (render $ hang 4 (pprint tvr <+> equals <+> pprint e))

--recordHoFile :: Ho -> [(HsModule,FileDep,String,[FileDep])] -> [FileDep] -> IO [FileDep]

recordHoFile ::
    Ho               -- ^ File to record
    -> [String]      -- ^ files to write to
    -> HoHeader      -- ^ file header
    -> IO Ho         -- ^ Ho updated with this recordfile dependencies
recordHoFile ho fs header = do
    if optNoWriteHo options then do
        wdump FD.Progress $ do
            fs' <- mapM shortenPath fs
            putErrLn $ "Skipping Writing Ho Files: " ++ show fs'
        return (ho { hoModules = fmap (const $ Left emptyFileDep) (hoExports ho) })
      else do
    let removeLink' fn = catch  (removeLink fn)  (\_ -> return ())
    let g (fn:fs) = do
            fd <- f fn
            mapM_ (l fn) fs
            return fd
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
            if optNoWriteHo options then return emptyFileDep else do
            let tfn = fn ++ ".tmp"
            fh <- openBinaryFile tfn WriteMode
            let theho =  mapHoBodies eraseE ho { hoUsedIds = mempty, hoModules = mempty }
            L.hPut fh (compress $ encode (magic,header,theho,magic2))
            hFlush fh
            (fh,fd) <- hGetFileDep fn fh
            hClose fh
            rename tfn fn
            return fd
    dep <- g fs
    return (ho { hoModules = fmap (const $ Left dep) (hoExports ho) })
    --return [ hsdep | (hs,hsdep,honm,ds) <- sc]

-- | Check that ho library dependencies are right
--hoLibraryDeps newHo oldHo = hoLibraries newHo `Map.isSubmapOf` hoLibraries oldHo

-- | Find a module, returning just the read Ho file and the parsed
-- contents of files that still need to be processed, This chases dependencies so
-- you could end up getting parsed source for several files back.
-- We only look for ho files where there is a cooresponding haskell source file.

nextModule ::
    Map.Map Module (Maybe (Either FileDep (LibraryName,CheckSum))) -- ^ modules we got, and don't need to worry about
    -> [(HsModule,FileDep,String)]  -- ^ todo list
    -> Ho -- ^ what we have read so far
    -> [Either Module (String,[(String,String)])] -- ^ either a module name or some files to search
    -> IO (Ho,[(HsModule,FileDep,String)]) -- ^ (everything read,what still needs to be done)

nextModule ms tl ho [] = return (ho,tl)
nextModule ms tl ho (Left m:rest) | m `mmember` ms = nextModule ms tl ho rest
nextModule ms tl ho (Left m:rest) = nextModule ms tl ho (Right (fromModule m,searchPaths (fromModule m)):rest)
nextModule ms tl ho (Right (name,files):rest) = result where
    result = do
        res@(_,fd,ho_name) <- findFirstFile name files
        when (fd == emptyFileDep) $ processIOErrors >> fail "Could not find file"
        if optIgnoreHo options then addNeed [] res else do
        mho <- checkForHoFile ho_name
        cho [] res mempty mho
    cho drest res zho mho = case mho of
            Nothing -> addNeed [] res
            Just (hoh,ho) -> cdeps (ho `mappend` zho:: Ho) res (drest ++ hohModDepends hoh)
    cdeps nho res ((m,fd):drest) = case mlookup m ms of
        Nothing | Just (Left fd') <- mlookup m (hoModules nho), fd' == fd -> cdeps nho res drest
        Nothing -> do
            r <- checkDep fd
            case r of
                True -> checkForHoFile (fromAtom $ fileName fd) >>= cho drest res nho
                False -> addNeed [] res
        Just Nothing -> cdeps nho res drest
        Just (Just (Left fd')) | fd == fd' -> cdeps nho res drest
        Just (Just (Left fd')) | fd /= emptyFileDep -> do
            wdump FD.Progress $ do
                putErrLn $ "Found newer dependency:" <+> fromModule m <+> "at" <+> pprint (fd,fd')
            addNeed [] res
        Just (Just _) -> addNeed [] res
    cdeps nho (fh,_,_) [] = hClose fh >> nextModule (ms `mappend` fmap Just (hoModules nho)) tl (nho `mappend` ho) rest
    addNeed additional (fh,fd,ho_name) = do
        cs <- if fopts FO.Cpp then filterInput "cpp" ["-D__JHC__","-traditional","-P"] fh
                              else CharIO.hGetContents fh
        hs <- parseHsSource (fromAtom $ fileName fd) cs
        case (hsModuleName hs `mmember` ms) of
            True -> do putStrLn $ "Found module name that we alread gots: " ++ show (hsModuleName hs); nextModule ms tl ho (map Left additional ++ rest)
            False -> do
                wdump FD.Progress $ do
                    sp <- shortenPath $ fromAtom (fileName fd)
                    putErrLn $ "Found dependency:" <+> name <+> "at" <+> sp -- fromAtom (fileName fd) --  <+> show (hsModuleRequires hs)
                nextModule (minsert (hsModuleName hs) Nothing ms) ((hs,fd,ho_name):tl) ho (rest ++ [ Left (Module m) | m <- hsModuleRequires hs] ++ map Left additional)






hsModuleRequires x = "Jhc.Prim":ans where
    noPrelude =   or $ not (optPrelude options):[ opt == c | opt <- hsModuleOptions x, c <- ["-N","--noprelude"]]
    ans = snub $ (if noPrelude then id else  ("Prelude":)) [ fromModule $ hsImportDeclModule y | y <- hsModuleImports x]

searchPaths :: String -> [(String,String)]
searchPaths m = ans where
    f m | (xs,'.':ys) <- span (/= '.') m = let n = (xs ++ "/" ++ ys) in m:f n
        | otherwise = [m]
    ans = [ (root ++ suf,root ++ ".ho") | i <- optIncdirs options, n <- f m, suf <- [".hs",".lhs"], let root = i ++ "/" ++ n]


parseHsSource :: String -> String -> IO HsModule
parseHsSource fn s = do
    let opts = concat [ words as | (x,as) <- parseOptions s', x `elem` ["OPTIONS","JHC_OPTIONS","OPTIONS_JHC"]]
        s' = if "shl." `isPrefixOf` reverse fn  then unlit fn s else s
    opt <- case fileOptions opts of
        Just o -> return o
        Nothing -> return options
    case runParserWithMode (parseModeOptions opt) { parseFilename = fn } parse  s'  of
                      ParseOk ws e -> processErrors ws >> return e
                      ParseFailed sl err -> putErrDie $ show sl ++ ": " ++ err


mapHoBodies  :: (E -> E) -> Ho -> Ho
mapHoBodies sm ho = ho { hoEs = fmap f (hoEs ho) , hoRules =  runIdentity (E.Rules.mapBodies (return . sm) (hoRules ho)) } where
    f (t,e) = (t,sm e)


eraseE :: E -> E
eraseE e = runIdentity $ f e where
    f (EVar tv) = return $ EVar  tvr { tvrIdent = tvrIdent tv }
    f e = emapE f e



hGetFileDep fn fh = do
    fd <- handleToFd fh
    fs <- getFdStatus fd
    fh <- fdToHandle fd
    return (fh,toFileDep fn fs)

openGetFileDep fn = do
    (fh,fs) <- openGetStatus fn
    return (fh,toFileDep fn fs)

openGetStatus fn = do
    fh <- openBinaryFile fn ReadMode
    fd <- handleToFd fh
    fs <- getFdStatus fd
    fh <- fdToHandle fd
    return (fh,fs)

hoToProgram :: Ho -> Program
hoToProgram ho = programSetDs (melems $ hoEs ho) program {
    progClassHierarchy = hoClassHierarchy ho,
    progDataTable = hoDataTable ho
    }

initialHo = mempty { hoDataTable = dataTablePrims  }

