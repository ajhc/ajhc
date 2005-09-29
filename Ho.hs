module Ho(Ho(..),HoHeader(..),FileDep(..),findModule,showHoCounts,initialHo,dumpHoFile,loadLibraries,recordHoFile) where


import Data.Graph(stronglyConnComp,SCC(..))
import Data.IORef
import Data.Monoid
import IO(bracket)
import List
import Prelude hiding(print,putStrLn)
import qualified Data.Map as Map
import qualified Text.PrettyPrint.HughesPJ as PPrint
import System.IO hiding(print,putStrLn)
import System.Posix.Files
import System.Posix.IO

import Atom
import Binary
import CanType
import CharIO
import Class
import Control.Monad.Identity
import DataConstructors
import Directory
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.E
import E.Inline(emapE)
import E.Rules
import E.Subst(substMap'')
import E.TypeCheck()
import FilterInput
import FrontEnd.HsParser
import FrontEnd.Infix
import FrontEnd.ParseMonad
import FrontEnd.Unlit
import GenUtil hiding(putErrLn,putErr,putErrDie)
import HsSyn
import KindInfer
import MapBinaryInstance()
import Maybe
import Monad
import Name
import Options
import PackedString
import PrimitiveOperators
import qualified FlagDump as FD
import qualified FlagOpts as FO
import Representation
import TypeSynonyms
import Warning


version :: Int
version = 5

magic = (packString "jhc Haskell Object File",version)
magic2 = packString "John's Haskell Compiler"


shortenPath :: String -> IO String
shortenPath x@('/':_) = do
    cd <- getCurrentDirectory
    pwd <- lookupEnv "PWD"
    h <- lookupEnv "HOME"
    --print (x,cd,h)
    let f d = d >>= \d -> getPrefix d x >>= \ ('/':rest) -> return rest
    return $ fromJust $ getPrefix cd x `mplus` f pwd `mplus` liftM ("~/" ++) (f h) `mplus` return x
shortenPath x = return x


data HoHeader = HoHeader {
    hohGeneration :: Int,
    hohDepends :: [FileDep],            -- ^ Haskell Source files depended on
    hohModDepends :: [(Module,FileDep)] -- ^ Other objects depended on
    }
    {-! derive: GhcBinary !-}

data Ho = Ho {
    -- filled in by front end
    hoModules :: Map.Map Module FileDep,     -- ^ Map of module to ho file, This never actually ends up in the binary file on disk, but is filled in when the file is read.
    hoExports :: Map.Map Module [Name],
    hoDefs :: Map.Map Name (SrcLoc,[Name]),
    hoAssumps :: Map.Map Name Scheme,        -- used for typechecking
    hoFixities :: FixityMap,
    hoKinds :: KindEnv,                      -- used for typechecking
    hoClassHierarchy :: ClassHierarchy,
    hoTypeSynonyms :: TypeSynonyms,
    hoProps :: Map.Map Name [Atom],
    -- Filled in by E generation
    hoDataTable :: DataTable,
    hoEs :: Map.Map Name (TVr,E),
    hoRules :: Rules
    }
    {-! derive: GhcBinary, Monoid !-}


-- | Contains hopefully enough meta-info to uniquely identify a file
-- independent of its name.

data FileDep = FileDep {
    fileName :: Atom,
    fileModifyTime :: Int,
    fileDeviceID :: Atom,
    fileFileID :: Int,
    fileFileSize :: Int
    } deriving(Show)
    {-! derive: GhcBinary !-}

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
findFirstFile err [] = Warning.err "missing-dep" ("Module not found: " ++ err) >> return (undefined,emptyFileDep,undefined)
findFirstFile err ((x,a):xs) = flip catch (\e ->   findFirstFile err xs) $ do
    (fh,fd) <- openGetFileDep x
    return (fh,fd,a)



findModule :: Ho                                 -- ^ Accumulated Ho
              -> (Either Module String)          -- ^ Either a module or filename to find
              -> (Ho -> IO Ho)                   -- ^ Process initial ho loaded from files and library
              -> (Ho -> [HsModule] -> IO Ho)     -- ^ Process set of mutually recursive modules to produce final Ho
              -> IO Ho                           -- ^ Final accumulated ho
findModule have (Left m) ifunc _ | m `Map.member` (hoExports have) = ifunc have
findModule have need ifunc func  = do
    let f (Left (Module m)) = (m,searchPaths m)
        f (Right n) = (n,[(n,reverse $ 'o':'h':dropWhile (/= '.') (reverse n))])
        (name,files) = f need
    (ho,ms) <- Ho.getModule have name files
    processIOErrors
    let scc = map f $  stronglyConnComp [ (x,fromModule $ hsModuleName hs,hsModuleRequires hs) | x@(hs,fd,honm) <- ms ]
        f (AcyclicSCC x) = [x]
        f (CyclicSCC xs) = xs
    when (dump FD.SccModules) $ CharIO.putErrLn $ "scc modules:\n" ++ unlines ( map  (\xs -> show [ hsModuleName x | (x,y,z) <- xs ]) scc)
    let f ho [] = return ho
        f ho (sc:scs) = do
            ho' <- func ho [ hs | (hs,_,_) <- sc ]
            let mods = [ hsModuleName hs | (hs,_,_) <- sc ]
                mods' = [ Module m  | (hs,_,_) <- sc, m <- hsModuleRequires hs, Module m `notElem` mods]
                mdeps = [ (m,runIdentity $ Map.lookup m (hoModules ho)) | m <- mods']
            ho' <- recordHoFile ho' [ x | (_,_,x) <- sc ] HoHeader { hohGeneration = 0, hohDepends = [ x | (_,x,_) <- sc], hohModDepends = mdeps }
            f (ho `mappend` ho') scs
    ho <- ifunc (ho `mappend` have)
    f ho scc

checkForHoFile :: String            -- ^ file name to check for
    -> IO (Maybe (HoHeader,Ho))
checkForHoFile fn = flip catch (\e -> putErrLn (show e) >> return Nothing) $ do
    bracket (openGetFileDep fn) (hClose . fst) $ \ (fh,dep) -> do
    -- (fh,dep) <- openGetFileDep fn
--    if optIgnoreHo options then do
--        wdump FD.Progress $ do
--            fn' <- shortenPath fn
--            putErrLn $ "Skipping haskell object file:" <+> fn'
--        return Nothing
--     else do
    --wdump FD.Progress $ do
    --    putErrLn $ "Found haskell object file:" <+> fn
    bh <- openBinIO fh
    x <- get bh
    if x /= magic then (putErrLn $ "Bad ho file:" <+> fn)  >> return Nothing else do
    hh <- get bh
    xs <- mapM checkDep (hohDepends hh)
    if not (and xs) then  return Nothing else do
        ho <- get bh
        x <- get bh
        if x /= magic2 then (putErrLn $ "Bad ho file:" <+> fn)  >>  return Nothing else do
        wdump FD.Progress $ do
            fn' <- shortenPath fn
            putErrLn $ "Found object file:" <+> fn'
        return $ Just (hh,ho { hoModules = fmap (const dep) (hoExports ho) })

checkDep fd = do
    fs <- getFileStatus (fromAtom $ fileName fd)
    return (fd == toFileDep (fileName fd) fs)


-- | This reads in an entire ho file for diagnostic purposes.
readHoFile :: String -> IO (HoHeader,Ho)
readHoFile fn = do
    fh <- openBinaryFile fn ReadMode
    bh <- openBinIO fh
    x <- get bh
    when (x /= magic) (putErrDie $ "Bad ho file magic1:" <+> fn)
    hh <- get bh
    ho <- get bh
    x <- get bh
    when (x /= magic2) (putErrDie $ "Bad ho file magic2:" <+> fn)
    return (hh,ho)


{-# NOINLINE dumpHoFile #-}
dumpHoFile :: String -> IO ()
dumpHoFile fn = do
    (hoh,ho) <- readHoFile fn
    putStrLn fn
    putStrLn $ "Generation:" <+> tshow (hohGeneration hoh)
    putStrLn $ "Dependencies:" <+>  pprint (sortUnder (show . fileName) $ hohDepends hoh)
    putStrLn $ "ModDependencies:" <+>  pprint (sortUnder fst $ hohModDepends hoh)
    putStrLn $ "hoMods:" <+> tshow (map fromModule $ Map.keys $  hoExports ho)
    putStrLn $ "hoExports:" <+> tshow (size $ hoExports ho)
    putStrLn $ "hoDefs:" <+> tshow (size $ hoDefs ho)
    putStrLn $ "hoAssumps:" <+> tshow (Map.size $ hoAssumps ho)
    --putErrLn $ "hoAssumps:" <+> vcat (map show $ Map.keys $ hoAssumps ho)
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
    when (dump FD.Rules) $ do
        putStrLn "  ---- rules ---- "
        printRules (hoRules ho)
    wdump FD.Datatable $ do
         putStrLn "  ---- data table ---- "
         putDocM CharIO.putStr (showDataTable (hoDataTable ho))
         putChar '\n'
    wdump FD.Types $ do
        putStrLn " ---- the types of identifiers ---- "
        putStrLn $ PPrint.render $ pprint (hoAssumps ho)



instance (PPrint d a, PPrint d b) => PPrint d (Map.Map a b) where
    pprint m = vcat [ pprint x <+> text "=>" <+> pprint y | (x,y) <- Map.toList m]

--recordHoFile :: Ho -> [(HsModule,FileDep,String,[FileDep])] -> [FileDep] -> IO [FileDep]

recordHoFile ::
    Ho               -- ^ File to record
    -> [String]      -- ^ files to write to
    -> HoHeader      -- ^ file header
    -> IO Ho         -- ^ Ho updated with this recordfiel dependencies
recordHoFile ho fs header = do
    if optNoWriteHo options then do
        wdump FD.Progress $ do
            fs' <- mapM shortenPath fs
            putErrLn $ "Skipping Writing Ho Files: " ++ show fs'
        return (ho { hoModules = fmap (const emptyFileDep) (hoExports ho) })
      else do
    --let header = HoHeader { hohGeneration = 0, hohDepends = snub (fd ++ concat [ hsdep:ds | (hs,hsdep,honm,ds) <- sc] )}
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
            bh <- openBinIO fh
            put bh magic
            put bh header
            put bh (mapHoBodies eraseE ho { hoModules = mempty })
            put bh magic2
            (fh,fd) <- hGetFileDep fn fh
            hClose fh
            rename tfn fn
            return fd
    dep <- g fs
    return (ho { hoModules = fmap (const dep) (hoExports ho) })
    --return [ hsdep | (hs,hsdep,honm,ds) <- sc]



-- | Find a module, returning the combined up to date Ho files and the parsed
-- contents of files that still need to be processed, This chases dependencies so
-- you could end up getting parsed source for several files back.
-- We only look for ho files where there is a cooresponding haskell source file.

getModule ::
    Ho          -- ^ Current set of modules, we assume anything in here is prefered to what is found on disk.
    -> String   -- ^ Module name for printing error messages
    -> [(String,String)]  -- ^ files to search, and the cooresponding ho file
    -> IO (Ho,[(HsModule,FileDep,String)])
getModule ho name files  = do
    ho_ref <- newIORef ho
    fixup_ref <- newIORef (getFixups ho)
    need_ref <- newIORef []
    let loop name files  = do
            --wdump FD.Progress $ do
            --    putErrLn $ "Looking for :" <+> name <+> "at" <+> show files
            -- First find the haskell source file.
            (fh,fd,ho_name) <- findFirstFile name files
            --if fd == emptyFileDep then return mempty else do
            when (fd == emptyFileDep) $ processIOErrors >> fail "Couldn't find file" -- then return mempty else do
            mho <- if optIgnoreHo options then do
                    wdump FD.Progress $ do
                        putErrLn $ "Skipping haskell object file:" <+> ho_name
                    return Nothing
                   else checkForHoFile ho_name
            case mho of
                Just (hh,ho') -> do
                    let f (a:as) = do
                            r <- checkHoDep a
                            if r then f as else return False
                        f [] = return True
                    r <- f (hohModDepends hh)
                    case r of
                        True -> do
                            fixups <- readIORef fixup_ref
                            let nfixups = getFixups ho' `mappend` fixups
                            writeIORef fixup_ref nfixups
                            modifyIORef ho_ref (applyFixups nfixups ho' `mappend`) >> hClose fh
                        False -> addNeed name fd fh ho_name
                Nothing -> addNeed name fd fh ho_name
        checkHoDep :: (Module,FileDep) -> IO Bool
        checkHoDep (m,fd) = do
            --wdump FD.Progress $ do
            --    putErrLn $ "checking dependency:" <+> show m <+> "at" <+> fromAtom (fileName fd)
            ho <- readIORef ho_ref
            case Map.lookup m (hoModules ho) of
                Just fd' | fd == fd' -> return True
                Just fd' | fd /= emptyFileDep -> do
                    wdump FD.Progress $ do
                        putErrLn $ "Found newer dependency:" <+> fromModule m <+> "at" <+> pprint (fd,fd')
                    return False
                Just _ -> return False
                Nothing -> do
                    xs <- readIORef need_ref
                    case lookup m xs of
                        Just _ -> return False
                        Nothing -> loop (fromModule m) (searchPaths (fromModule m)) >> checkHoDep (m,fd)
        addNeed :: String -> FileDep -> Handle -> String ->  IO ()
        addNeed name fd fh ho_name = do
            cs <- if fopts FO.Cpp then filterInput "cpp" ["-D__JHC__","-traditional","-P"] fh
                                  else CharIO.hGetContents fh
            hs <- parseHsSource (fromAtom $ fileName fd) cs
            wdump FD.Progress $ do
                sp <- shortenPath $ fromAtom (fileName fd)
                putErrLn $ "Found dependency:" <+> name <+> "at" <+> sp -- fromAtom (fileName fd) --  <+> show (hsModuleRequires hs)
            modifyIORef need_ref $ ((hsModuleName hs,(hs,fd,ho_name)):)
            mapM_ (checkHoDep . (flip (,) emptyFileDep) . Module) $ hsModuleRequires hs
    loop name files
    ho   <- readIORef ho_ref
    need <- readIORef need_ref
    return (ho,snds need)

hsModuleRequires x = ans where
    noPrelude =   or $ not (optPrelude options):[ opt == c | opt <- hsModuleOptions x, c <- ["-N","--noprelude"]]
    ans = snub $ (if noPrelude then id else  ("Prelude":)) [ fromModule $ hsImportDeclModule y | y <- hsModuleImports x]

searchPaths :: String -> [(String,String)]
searchPaths m = ans where
    f m | (xs,'.':ys) <- span (/= '.') m = let n = (xs ++ "/" ++ ys) in m:f n
        | otherwise = [m]
    ans = [ (root ++ suf,root ++ ".ho") | i <- optIncdirs options, n <- f m, suf <- [".hs",".lhs"], let root = i ++ "/" ++ n]


parseHsSource :: String -> String -> IO HsModule
parseHsSource fn s = case runParserWithMode ParseMode { parseFilename = fn } parse  s'  of
                      ParseOk e -> return e
                      ParseFailed sl err -> putErrDie $ show sl ++ ": " ++ err
    where
    s' = if "shl." `isPrefixOf` reverse fn  then unlit fn s else s
                      -- warnF fn "parse-error" err >> return emptyHsModule


mapHoBodies  :: (E -> E) -> Ho -> Ho
mapHoBodies sm ho = ho { hoEs = Map.map f (hoEs ho) , hoRules =  runIdentity (E.Rules.mapBodies (return . sm) (hoRules ho)) } where
    f (t,e) = (t,sm e)



eraseE :: E -> E
eraseE e = runIdentity $ f e where
    f (EVar tv) = return $ EVar  tvr { tvrIdent = tvrIdent tv }
    f e = emapE f e

getFixups :: Ho -> Map.Map Int E
getFixups ho = Map.fromList [ (tvrIdent x,EVar x) | (x,_) <- Map.elems (hoEs ho)]

applyFixups :: Map.Map Int E -> Ho -> Ho
applyFixups mie ho = ho { hoEs = Map.map f (hoEs ho) , hoRules =  runIdentity (E.Rules.mapBodies (return . sm) (hoRules ho)) } where
    f (t,e) = (t,sm e)
    sm = substMap'' mie




{-
emptyHsModule = HsModule {
    hsModuleName = Module "@invalid",
    hsModuleImports = [],
    hsModuleExports = Nothing,
    hsModuleDecls = [],
    hsModuleOptions = []
    }
-}

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


showHoCounts ho = do
    putErrLn $ "hoMods:" <+> tshow (map fromModule $ Map.keys $  hoExports ho)
    putErrLn $ "hoExports:" <+> tshow (size $ hoExports ho)
    putErrLn $ "hoDefs:" <+> tshow (size $ hoDefs ho)
    putErrLn $ "hoAssumps:" <+> vcat (map show $ Map.keys $ hoAssumps ho)
    putErrLn $ "hoFixities:" <+> tshow (size $  hoFixities ho)
    putErrLn $ "hoKinds:" <+> tshow (size $  hoKinds ho)
    putErrLn $ "hoClassHierarchy:" <+> tshow (size $  hoClassHierarchy ho)
    putErrLn $ "hoTypeSynonyms:" <+> tshow (size $  hoTypeSynonyms ho)
    putErrLn $ "hoDataTable:" <+> tshow (size $  hoDataTable ho)
    putErrLn $ "hoEs:" <+> tshow (size $  hoEs ho)
    putErrLn $ "hoProps:" <+> tshow (size $  hoProps ho)
    putErrLn $ "hoRules:" <+> tshow (size $  hoRules ho)


loadLibraries :: IO Ho
loadLibraries = f initialHo (optHls options)  where
    f ho [] = return ho
    f ho (fn:rs) = checkForHoFile fn >>= \x -> case x of
        Nothing -> putErrDie $ "Library not found or invalid: " ++ show fn
        Just (_,ho') -> f (ho' `mappend` ho) rs



initialHo = mempty { hoEs = es , hoClassHierarchy = ch  }  where
    ch = foldl addOneInstanceToHierarchy mempty (map ((,) False) primitiveInsts)
    es = Map.fromList [  (n,(tVr (atomIndex $ toAtom n) (getType v),v)) |  (n,v) <- constantMethods ] `mappend` es'
    --es' = Map.fromList [ (n,(tVr (atomIndex $ toAtom n) (getType v),v)) | (n,t,p,d) <- theMethods, let v = f n t p d  ]
    es' = Map.fromList [ (n,(tVr (atomIndex $ toAtom n) (error "f no longer relevant"),v)) | (n,t,p,d) <- theMethods, let v = f n t p d  ]
    f _ _ _ _ = error "f no longer relevant"





    {-
    f n t p d = ans where
        (r':as') = reverse t
        r = tt r'
        as = map tt (reverse as')
        tt 'a' = ELit (LitCons (parseName TypeConstructor d) [] eStar)
        tt 'B' = tBool
        tt 'I' = tInt
        tvs = [  (TVr i a) | a <- as | i <- [ 2,4 ..]]
        ans = foldr ELam (EPrim (primPrim p) (map EVar tvs) r) tvs
      -}




    {-

    -- Collect all complete 'ho' files and parsed code we need to compile
--getModule :: Set.Set String -> String -> [(String,String)] -> IO (Ho,[],[FileDep])
getModule ws name files = do
    --wdump FD.Progress $ do
    --    putErrLn $ "getModule:" <+> tshow ws <+> tshow name <+> tshow files
    (c,fd,honm) <- findFirstFile name files
    if fd == emptyFileDep then return (mempty,mempty,mempty) else do
    ho <- checkForHoFile honm
    case ho of
        Just (ds,ho) -> mapM_ (\x -> modifyIORef ws (Set.insert x)) (Map.keys $ hoExports ho) >> return (ho,mempty,ds)
        Nothing -> do
            hs <- parseHsSource (fromAtom $ fileName fd) c
            wdump FD.Progress $ do
                putErrLn $ "Found dependency:" <+> name <+> "at" <+> fromAtom (fileName fd)  <+> show (hsModuleRequires hs)
            --print hs
            modifyIORef ws $ Set.insert ( hsModuleName hs)
            ws' <- readIORef ws
            --mapM_ (modifyIORef ws) [Set.insert (Module x) | x <- hsModuleRequires hs]
            let f x = do
                ws' <- readIORef ws
                case Module x `Set.member` ws' of
                    True ->  return (mempty,mempty,mempty)
                    False -> Ho.getModule ws x (searchPaths x)
                --f x | Module x `Set.member` ws' = return (mempty,mempty,mempty)
                --    | otherwise = Ho.getModule ws x (searchPaths x)
            xs <- mapM f (hsModuleRequires hs)
            let x@(_,_,ds) = mconcat xs
            return $ mconcat [(mempty,[(hs,fd,honm,ds)],mempty),x]

            -}
