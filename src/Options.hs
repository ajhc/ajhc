module Options(
    processOptions,
    Opt(..),
    options,
    Mode(..),
    StopCondition(..),
    putProgress,
    putProgressLn,
    putVerbose,
    putVerboseLn,
    getArguments,
    findHoCache,
    verbose,
    verbose2,
    progress,
    preprocess,
    dump,
    wdump,
    fopts,
    wdump',
    fopts',
    flint,
    LibDesc(..),
    readYamlOpts,
    fileOptions,
    withOptions,
    withOptionsT,
    getArgString,
    outputName,
    OptM(),
    OptT(),
    OptionMonad(..),
    flagOpt
    ) where

import Control.Monad.Error()    -- IO MonadPlus instance
import Control.Monad.Reader
import Data.Char
import Data.Yaml.Syck
import System
import System.Console.GetOpt
import System.Directory
import System.IO.Unsafe
import Util.Std
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as M
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Data.Set as Set
import qualified System.FilePath as FP

import Options.Map
import Options.Type
import RawFiles(prelude_m4)
import RawFiles(targets_ini)
import Support.IniParse
import Support.TempDir
import Util.ExitCodes
import Util.FilterInput
import Util.Gen
import Util.YAML
import Version.Config
import Version.Version(versionString,versionContext)
import qualified FlagDump as FD
import qualified FlagOpts as FO
import qualified Version.Config as VC

{-@CrossCompilation

# Basics

Unlike many other compilers, jhc is a native cross compiler. What this means is
that every compile of jhc is able to create code for all possible target
systems. This leads to many simplifications when it comes to cross compiling
with jhc. Basically in order to cross compile, you need only pass the flag
'--cross' to jhc, and pass an appropriate '-m' option to tell jhc what machine
you are targetting. An example would be

    ; jhc --cross -mwin32 test/HelloWorld.hs

The targets list is extensible at run-time via the targets.ini file explained
below.

# targets.ini

This file determines what targets are available. The format consists of entries as follows.

    [targetname]
    key1=value
    key2=value
    key3+=value
    merge=targetname2

merge is a special key meaning to merge the contents of another target into the
current one. The configuration file is read in order, and the final value set
for a given key is the one that is used.

An example describing how to cross compile for windows is as follows:

    [win32]
    cc=i386-mingw32-gcc
    cflags+=-mwindows -mno-cygwin
    executable_extension=.exe
    merge=i686

This sets the compiler to use as well as a few other options then jumps to the
generic i686 routine. The special target [default] is always read before all
other targets. If '--cross' is specified on the command line then this is the
only implicitly included configuration, otherwise jhc will assume you are
compiling for the current architecture and choose an appropriate target to
include in addition to default.

jhc will attempt to read several targets.ini files in order. they are

$PREFIX/etc/jhc-\$VERSION/targets.ini
:   this is the targets.ini that is included with jhc and contains the default options.

`$PREFIX/etc/jhc-\$VERSION/targets-local.ini`
:   jhc will read this if it exists, it is used to specify custom system wide configuration options, such as the name of local compilers.

$HOME/.jhc/targets.ini
:   this is where a users local configuration information goes.

$HOME/etc/jhc/targets.ini
:   this is simply for people that prefer to not use hidden directories for configuration

The last value specified for an option is the one used, so a users local
configuration overrides the system local version which overrides the built in
options.

## targets.ini options available

Option                    Meaning
------                    ---------------------------------------------------------------------------
_cc_                      what c compiler to use. generally this will be gcc for local builds and something like $ARCH-$HOST-gcc for cross compiles
_byteorder_               one of *le* or *be* for little or big endian
_gc_                      what garbage collector to use. It should be one of *static* or *boehm*.
_cflags_                  options to pass to the c compiler
_cflags\_debug_           options to pass to the c compiler only when debugging is enabled
_cflags\_nodebug_         options to pass to the c compiler only when debugging is disabled
_profile_                 whether to include profiling code in the generated executable
_autoload_                what haskell libraries to autoload, seperated by commas.
_executable\_extension_   specifies an extension that should be appended to executable files, (i.e. .EXE on windows)
_merge_                   a special option that merges the contents of another configuration target into the currrent one.
_bits_                    the number of bits a pointer contains on this architecture
_bits\_max_               the number of bits in the largest integral type. should be the number of bits in the 'intmax_t' C type.
_arch_                    what to pass to gcc as the architecture

# Unusual Cross compilation

When you need to do more interesting things than just specify a different
compiler, such as modifying or replacing parts of the rts, or importing the code
into another build environment such as android, you can have jhc create a
standalone directory of C code that can then be made with make. specify the
`-tdir dir` option along with `-C` to create the project in the given directory.

## Internal RTS macros

Define                             Meaning
------                             ---------------------------------------------------------------------------
\_JHC\_ARM\_STAY\_IN\_THUMB\_MODE  set bit0 to any function pointers, for Cortex-M*. ([more detail](http://communities.mentor.com/community/cs/archives/arm-gnu/msg01904.html))
\_JHC\_JGC\_NAIVEGC                run gc when have no more blocks.
\_JHC\_JGC\_STACKGROW              number of stack entry growed when run short of it.
\_JHC\_JGC\_FIXED\_MEGABLOCK       use a single megablock without allocation megablock.
\_JHC\_JGC\_BLOCK\_SHIFT           bit shift to specify block size. Use it internally like this: (1 << (_JHC_JGC_BLOCK_SHIFT)).
\_JHC\_JGC\_MEGABLOCK\_SHIFT       bit shift to specify megablock size. Use it internally like this: (1 << (_JHC_JGC_MEGABLOCK_SHIFT)).

-}

getArguments = do
    x <- lookupEnv "JHC_OPTS"
    let eas = maybe [] words x
    as <- System.getArgs
    return (eas ++ as)

pfill ::
    Int            -- ^ maximum width
    -> (a -> Int)  -- ^ find width of any element
    -> [a]         -- ^ input elements
    -> [[a]]       -- ^ output element
pfill maxn length xs = f maxn xs [] [] where
    f n (x:xs) ws ls | lx < n = f (n - lx) xs (x:ws) ls where
        lx = length x
    f _ (x:xs) [] ls = f (maxn - length x) xs [x] ls
    f _ (x:xs) ws ls = f (maxn - length x) xs [x] (ws:ls)
    f _ [] [] ls = reverse (map reverse ls)
    f _ [] ws ls = reverse (map reverse (ws:ls))

helpUsage = usageInfo header theoptions ++ trailer where
    header = "Usage: jhc [OPTION...] Main.hs"
    trailer = "\n" ++ mkoptlist "-d" FD.helpFlags ++ "\n" ++ mkoptlist "-f" FO.helpFlags
    mkoptlist d os = "valid " ++ d ++ " arguments: 'help' for more info\n    " ++ intercalate "\n    " (map (intercalate ", ") $ pfill 100 ((2 +) . length) os) ++ "\n"

{-# NOINLINE processOptions #-}
-- | Parse commandline options.
processOptions :: IO Opt
processOptions = do
    -- initial argument processing
    argv <- getArguments
    let (o,ns,rc) = getOpt Permute theoptions argv
    optColumns <- getColumns
    optIncdirs <- initialIncludes
    optHlPath <- initialLibIncludes
    o <- return (foldl (flip ($)) emptyOpt { optColumns, optIncdirs, optHlPath } o)
    when (rc /= []) $ putErrLn (concat rc ++ helpUsage) >> exitWith exitCodeUsage
    let (nset,errs) = processLanguageFlags (reverse $ optExtensions o) (optFOptsSet o)
    o <- return $ o { optFOptsSet = nset }
    unless (null errs) $ do
        putErrLn $ "Unrecognized -X flags: " ++  concatMap (' ':) errs

    case optStop o of
        StopError s -> putErrLn "bad option passed to --stop should be one of parse, deps, typecheck, or c" >> exitWith exitCodeUsage
        _ -> return ()
    let readYaml o fp = do
            when (dopts' o FD.Progress) $ putStrLn $ "Reading " ++ fp
            snd <$> readYamlOpts o fp
    o <- foldM readYaml o (optWith o)
    case optMode o of
        ShowHelp  | optVerbose o > 0  -> putStrLn prettyOptions >> exitSuccess
        ShowHelp    -> doShowHelp
        Version   | optVerbose o > 0  -> putStrLn (versionString ++ BS.toString versionContext) >> exitSuccess
        Version     -> putStrLn versionString >> exitSuccess
        PrintHscOptions -> do
            putStrLn $ "-I" ++ VC.datadir ++ "/" ++ VC.package ++ "-" ++ VC.shortVersion ++ "/include"
            exitSuccess
        _ -> return ()
    -- read targets.ini file
    Just home <- fmap (`mplus` Just "/") $ lookupEnv "HOME"
    inis <- parseIniFiles (optVerbose o > 0) (BS.toString targets_ini) [confDir ++ "/targets.ini", confDir ++ "/targets-local.ini", home ++ "/etc/jhc/targets.ini", home ++ "/.jhc/targets.ini"] (optArch o)
    -- process dump flags
    o <- either putErrDie return $ postProcessFD o
    when (FD.Ini `S.member` optDumpSet o) $ flip mapM_ (M.toList inis) $ \(a,b) -> putStrLn (a ++ "=" ++ b)
    -- set flags based on ini options
    let o1 = case M.lookup "gc" inis of
            Just "jgc" -> optFOptsSet_u (S.insert FO.Jgc) o
            Just "boehm" -> optFOptsSet_u (S.insert FO.Boehm) o
            _ -> o
    o2 <- either putErrDie return $ postProcessFO o1

    -- add autoloads based on ini options
    let autoloads = maybe [] (tokens (',' ==)) (M.lookup "autoload" inis)
        Opt { .. } = o2
        fopt = o2 {
            optIncdirs = nub optIncdirs,
            optHlPath = nub optHlPath,
            optIncs = nub optIncs,
            optArgs = ns,
            optInis = inis,
            optAutoLoads = autoloads }
    case optMode of
        ShowConfig  -> doShowConfig fopt
        _ -> return fopt

doShowHelp = do
    putStrLn helpUsage
    exitSuccess

findHoCache :: IO (Maybe FilePath)
findHoCache = do
    cd <- lookupEnv "JHC_CACHE"
    case optHoCache options `mplus` cd of
        Just "-" -> do return Nothing
        Just s -> do return (Just s)
        Nothing | isNothing (optHoDir options) -> do
            Just home <- fmap (`mplus` Just "/") $ lookupEnv "HOME"
            let cd = home ++ "/.jhc/cache"
            createDirectoryIfMissing True cd
            return (Just cd)
        _  -> return Nothing

doShowConfig Opt { .. } = do
    ls <- initialLibIncludes
    let (==>) :: ToNode b => String -> b -> (String,Node)
        a ==> b = (a,toNode b)
    putStrLn $ showYAML $ toNode [
        "name" ==> package,
        "version" ==> version,
        "libdir" ==> libdir,
        "datadir" ==> datadir,
        "libraryInstall" ==> libraryInstall,
        "host" ==> host,
        "jhclibpath" ==> optHlPath,
        "hs-source-dirs" ==> optIncdirs,
        "include-dirs" ==> optIncs,
        "build-depends" ==> optHls,
        "flags-option"  ==> optFOptsSet,
        "flags-dump"    ==> optDumpSet
        ]
    exitSuccess

instance ToNode FO.Flag where
    toNode = toNode . show
instance ToNode FD.Flag where
    toNode = toNode . show

{-# NOINLINE options #-}
-- | The global options currently used.
options :: Opt
options = unsafePerformIO processOptions

-- | Put a string to stderr when running verbose.
putVerbose :: String -> IO ()
putVerbose s = when (optVerbose options > 0) $ putErr s

-- | Put a line to stderr when running verbose.
putVerboseLn :: String -> IO ()
putVerboseLn s = putVerbose (s ++ "\n")

putProgress :: String -> IO ()
putProgress s = when progress $ putErr s

-- | Put a line to stderr when running verbose.
putProgressLn :: String -> IO ()
putProgressLn s = putProgress (s ++ "\n")

-- | Is verbose > 0?
progress :: Bool
progress = dump FD.Progress

-- | Is verbose > 0?
verbose :: Bool
verbose = optVerbose options > 0
-- | Is verbose > 1?
verbose2 :: Bool
verbose2 = optVerbose options > 1

-- | Test whether a dump flag is set.
dump :: FD.Flag -> Bool
dump s = s `S.member` optDumpSet options
-- | Test whether an option flag is set.
fopts :: FO.Flag -> Bool
fopts s = s `S.member` optFOptsSet options
-- | Do the action when the suplied dump flag is set.
wdump :: (Monad m) => FD.Flag -> m () -> m ()
wdump f = when (dump f)

-- | Test whether an option flag is set.
fopts' :: Opt -> FO.Flag -> Bool
fopts' opt s = s `S.member` optFOptsSet opt
-- | Test whether an option flag is set.
dopts' :: Opt -> FD.Flag -> Bool
dopts' opt s = s `S.member` optDumpSet opt
-- | Do the action when the suplied dump flag is set.
wdump' :: (Monad m) => Opt -> FD.Flag -> m () -> m ()
wdump' opt f = when $ f `S.member` optDumpSet opt

-- | Is the \"lint\" option flag set?
flint :: Bool
flint = FO.Lint `S.member` optFOptsSet options

class Monad m => OptionMonad m where
    getOptions :: m Opt
    getOptions = return options

instance OptionMonad Identity
instance OptionMonad IO

newtype OptT m a = OptT (ReaderT Opt m a)
    deriving(MonadIO,Monad,Functor,MonadTrans)

type OptM = OptT Identity

instance Monad m => OptionMonad (OptT m) where
    getOptions = OptT ask

withOptions :: Opt -> OptM a -> a
withOptions opt (OptT x) = runIdentity (runReaderT x opt)

withOptionsT :: Opt -> OptT m a -> m a
withOptionsT opt (OptT x) = runReaderT x opt

outputName = fromMaybe "hs.out" (optOutName options)

flagOpt :: OptionMonad m => FO.Flag -> m Bool
flagOpt flag = do
    opt <- getOptions
    return (flag `S.member` optFOptsSet opt)

getArgString = do
    name <- System.getProgName
    args <- getArguments
    return (simpleQuote (name:args),head $ lines versionString)

--prettyOptions :: Opt -> String
--prettyOptions _ = ""

-- | Width of terminal.
getColumns :: IO Int
getColumns = do
    mcol <- lookupEnv "COLUMNS"
    return $ fromMaybe 80 (mcol >>= readM)

-- | Include directories taken from JHCPATH enviroment variable.
initialIncludes :: IO [String]
initialIncludes = do
    p <- lookupEnv "JHC_PATH"
    let x = fromMaybe "" p
    return (".":(tokens (== ':') x))

-- | Include directories taken from JHCLIBPATH enviroment variable.
initialLibIncludes :: IO [String]
initialLibIncludes = do
    ps <- lookupEnv "JHC_LIBRARY_PATH"
    h <- lookupEnv "HOME"
    let paths = h ++ ["/usr/local","/usr"]
        bases = ["/lib","/share"]
        vers = ["/jhc-" ++ shortVersion, "/jhc"]
    let lpath = nub $ maybe [] (tokens (':' ==))  ps ++ [ p ++ b ++ v | p <- paths, v <- vers, b <- bases ]
               ++ [d ++ v | d <- [libdir,datadir], v <- vers] ++ [libraryInstall]
    return $ if "." `elem` lpath then lpath else ".":lpath

data LibDesc = LibDesc (Map.Map String [String]) (Map.Map String String)

readDescFile :: Opt -> FilePath -> IO LibDesc
readDescFile origOpt fp = do
--    wdump FD.Progress $ putErrLn $ "Reading: " ++ show fp
    let doYaml opt = do
            lbs <- LBS.readFile fp
            dt <- preprocess opt fp lbs
            desc <- iocatch (parseYamlBytes $ BS.concat (LBS.toChunks dt))
                (\e -> putErrDie $ "Error parsing desc file '" ++ fp ++ "'\n" ++ show e)
            when (optVerbose origOpt > 1) $ do
                yaml <- emitYaml desc
                putStrLn yaml
            return $ procYaml desc
    case FP.splitExtension fp of
        (_,".yaml") -> doYaml origOpt
        (FP.takeExtension -> ".yaml",".m4") -> doYaml origOpt { optFOptsSet = FO.M4 `Set.insert` optFOptsSet origOpt }
        _ -> putErrDie $ "Do not recoginize description file type: " ++ fp

readYamlOpts :: Opt -> FilePath -> IO (LibDesc,Opt)
readYamlOpts origOpt@Opt { .. } fp = do
    ld@(LibDesc dlist dsing) <- readDescFile origOpt fp

    let mfield x = maybe [] id $ Map.lookup x dlist
        dd "." = FP.takeDirectory fp
        dd ('.':'/':x) = dd x
        dd x = FP.takeDirectory fp FP.</> x
    (optFOptsSet,errs) <- return $ processLanguageFlags (mfield "extensions") optFOptsSet
    unless (null errs) $ do
        putErrLn $ "---"
        putErrLn $ fp ++ ":Unrecognized extensions" ++  show errs
    optIncdirs <- return $ map dd (mfield "hs-source-dirs") ++ optIncdirs
    optIncs <- return $ map dd (mfield "include-dirs") ++ optIncs
    optHls <- return $ mfield "build-depends" ++ optHls
    let nopt = Opt { .. }
        fileOpts = fileOptions nopt (mfield "options")
    bopt <- case fileOpts of
        Left errs -> do
            putErrLn $ "---"
            putErrLn $ fp ++ ":Unrecognized options" ++  show (mfield "options")
            putErrLn $ errs
            return nopt
        Right o -> return o
    return (ld,bopt)

preprocess :: Opt -> FilePath -> LBS.ByteString -> IO LBS.ByteString
preprocess opt fn lbs = do
    let fopts s = s `Set.member` optFOptsSet opt
        incFlags = [ "-I" ++ d | d <- optIncdirs opt ++ optIncs opt]
        defFlags = ("-D__JHC__=" ++ revision):("-D__JHC_VERSION__=" ++ version):[ "-D" ++ d | d <- optDefs opt]
    case () of
        _ | fopts FO.Cpp -> readSystem "cpphs" $ incFlags ++ defFlags ++ [fn]
          | fopts FO.M4  -> do
                m4p <- m4Prelude
                readSystem "m4" $ ["-s", "-P"] ++ incFlags ++ defFlags ++ [m4p,fn]
          | otherwise -> return lbs

m4Prelude :: IO FilePath
m4Prelude = fileInTempDir "prelude.m4" $ \fp -> do putStrLn $ "Writing stuff:" ++ fp ; BS.writeFile fp prelude_m4 ; return ()

procYaml :: YamlNode -> LibDesc
procYaml MkNode { n_elem = EMap ms } = f ms mempty mempty where
    f [] dlm dsm = LibDesc (combineAliases dlm) dsm
    f ((n_elem -> EStr (map toLower . unpackBuf -> x),y):rs) dlm dsm = if x `Set.member` list_fields then dlist y else dsing y where
        dlist (n_elem -> EStr y)  = f rs (Map.insert x [unpackBuf y] dlm) dsm
        dlist (n_elem -> ESeq ss) = f rs (Map.insert x [ unpackBuf y | (n_elem -> EStr y) <- ss ] dlm) dsm
        dlist _ = f rs dlm dsm
        dsing (n_elem -> EStr y) = f rs dlm (Map.insert x (unpackBuf y) dsm)
        dsing _ = f rs dlm dsm
    f (_:xs) dlm dsm = f xs dlm dsm
procYaml _ = LibDesc mempty mempty

list_fields = Set.fromList $ [
    "exposed-modules",
    "include-dirs",
    "extensions",
    "options",
    "c-sources",
    "include-sources",
    "build-depends"
    ] ++ map fst alias_fields
      ++ map snd alias_fields

alias_fields = [
 --  ("other-modules","hidden-modules"),
   ("exported-modules","exposed-modules"),
   ("hs-source-dir","hs-source-dirs")
   ]

combineAliases mp = f alias_fields mp where
    f [] mp = mp
    f ((x,y):rs) mp = case Map.lookup x mp of
        Nothing -> f rs mp
        Just ys -> f rs $ Map.delete x $ Map.insertWith (++) y ys mp
