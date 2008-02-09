{-# OPTIONS -w -funbox-strict-fields #-}
module Options(
    processOptions,
    Opt(..),
    options,
    Mode(..),
    putVerbose,
    putVerboseLn,
    getArguments,
    verbose,
    verbose2,
    dump,
    wdump,
    fopts,
    flint,
    fileOptions,
    withOptions,
    withOptionsT,
    getArgString,
    OptM(),
    OptT(),
    OptionMonad(..),
    flagOpt
    ) where

import Control.Monad.Error()    -- IO MonadPlus instance
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Set as S
import Data.List(nub)
import System
import System.Console.GetOpt
import System.IO.Unsafe

import Util.Gen
import qualified FlagDump
import qualified FlagOpts
import Version.Config
import Version.Version(versionString)

data Mode = BuildHl String -- ^ Build the specified hl-file given a description file.
          | Interactive    -- ^ Run interactively.
          | Version        -- ^ Print version and die.
          | VersionCtx     -- ^ Print version context and die.
          | ShowHelp       -- ^ Show help message and die.
          | ShowConfig     -- ^ Show configuration info.
          | Interpret      -- ^ Interpret.
          | CompileHo      -- ^ Compile ho
          | CompileHoGrin  -- ^ Compile ho and grin
          | CompileExe     -- ^ Compile executable
          | ShowHo String  -- ^ Show ho-file.
          | ListLibraries  -- ^ List libraries
            deriving(Eq)


data Opt = Opt {
    optMode        :: Mode,       -- ^ Mode of interaction
    optColumns     :: !Int,       -- ^ Width of terminal.
    optDebug       :: !Bool,      -- ^ Debugging.
    optDump        ::  [String],  -- ^ Dump options (raw).
    optStmts       ::  [String],  -- ^ statements to execute
    optFOpts       ::  [String],  -- ^ Flag options (raw).
    optIncdirs     ::  [String],  -- ^ Include directories.
    optProgArgs    ::  [String],  -- ^ Arguments to pass to the interpreted program.
    optCCargs      ::  [String],  -- ^ Optional arguments to the C compiler.
    optHls         ::  [String],  -- ^ Load the specified hl-files (haskell libraries).
    optHlPath      ::  [String],  -- ^ Path to look for libraries.
    optCC          ::  String,    -- ^ C compiler.
    optArgs        ::  [String],
    optKeepGoing   :: !Bool,      -- ^ Keep going when encountering errors.
    optMainFunc    ::  Maybe (Bool,String),    -- ^ Entry point name for the main function.
    optArch        ::  Maybe String,           -- ^ target architecture
    optOutName     ::  String,                 -- ^ Name of output file.
    optPrelude     :: !Bool,                   -- ^ No implicit Prelude.
    optIgnoreHo    :: !Bool,                   -- ^ Ignore ho-files.
    optNoWriteHo   :: !Bool,                   -- ^ Don't write ho-files.
    optNoAuto      :: !Bool,                   -- ^ Don't autoload packages
    optFollowDeps  :: !Bool,                   -- ^ Don't follow dependencies, all deps must be loaded from packages or specified on the command line.
    optVerbose     :: !Int,                    -- ^ Verbosity
    optStatLevel   :: !Int,                    -- ^ Level to print statistics
    optDumpSet     ::  S.Set FlagDump.Flag,    -- ^ Dump flags.
    optFOptsSet    ::  S.Set FlagOpts.Flag     -- ^ Flag options (-f\<opt\>).
  } {-!derive: update !-}


opt = Opt {
    optMode        = CompileExe,
    optColumns     = getColumns,
    optDebug       = False,
    optIncdirs     = initialIncludes,
    optHls         = [],
    optHlPath      = initialLibIncludes,
    optProgArgs    = [],
    optDump        = [],
    optStmts       = [],
    optFOpts       = ["default"],
    optCCargs      = [],
    optCC          = "gcc",
    optArgs        = [],
    optIgnoreHo    = False,
    optNoWriteHo   = False,
    optKeepGoing   = False,
    optMainFunc    = Nothing,
    optArch        = Nothing,
    optOutName     = "hs.out",
    optPrelude     = True,
    optFollowDeps  = True,
    optVerbose     = 0,
    optStatLevel   = 1,
    optNoAuto      = False,
    optDumpSet     = S.empty,
    optFOptsSet    = S.empty
}

idu "-" _ = []
idu d ds = ds ++ [d]

theoptions :: [OptDescr (Opt -> Opt)]
theoptions =
    [ Option ['V'] ["version"]   (NoArg  (optMode_s Version))          "print version info and exit"
    , Option []    ["version-context"] (NoArg  (optMode_s VersionCtx)) "print version context info and exit"
    , Option []    ["help"]      (NoArg  (optMode_s ShowHelp))         "print help information and exit"
    , Option []    ["config"]    (NoArg  (optMode_s ShowConfig))       "show a variety of config info"
    , Option ['v'] ["verbose"]   (NoArg  (optVerbose_u (+1)))          "chatty output on stderr"
    , Option ['z'] []            (NoArg  (optStatLevel_u (+1)))        "Increase verbosity of statistics"
    , Option ['d'] []            (ReqArg (\d -> optDump_u (d:)) "[no-]flag")  "dump specified data during compilation"
    , Option ['f'] []            (ReqArg (\d -> optFOpts_u (d:)) "[no-]flag") "set or clear compilation options"
    , Option ['o'] ["output"]    (ReqArg (optOutName_s) "FILE")        "output to FILE"
    , Option ['i'] ["include"]   (ReqArg (optIncdirs_u . idu) "DIR")   "library directory"
    , Option []    ["optc"]      (ReqArg (optCCargs_u . idu) "option") "extra options to pass to c compiler"
    , Option []    ["progc"]     (ReqArg (\d -> optCC_s d) "gcc")      "c compiler to use"
    , Option []    ["arg"]       (ReqArg (\d -> optProgArgs_u (++ [d])) "arg") "arguments to pass interpreted program"
    , Option ['N'] ["noprelude"] (NoArg  (optPrelude_s False))         "no implicit prelude"
    , Option ['C'] []            (NoArg  (optMode_s CompileHoGrin))    "Typecheck, compile ho and grin."
    , Option ['c'] []            (NoArg  (optMode_s CompileHo))        "Typecheck and compile ho."
    , Option ['I'] ["interpret"] (NoArg  (optMode_s Interpret))        "interpret."
    , Option ['k'] ["keepgoing"] (NoArg  (optKeepGoing_s True))        "keep going on errors."
    , Option []    ["width"]     (ReqArg (optColumns_s . read) "COLUMNS") "width of screen for debugging output."
    , Option []    ["main"]      (ReqArg (optMainFunc_s . Just . (,) False) "Main.main")  "main entry point."
    , Option ['m'] ["arch"]      (ReqArg (optArch_s . Just ) "arch")            "target architecture."
    , Option []    ["entry"]     (ReqArg (optMainFunc_s . Just . (,) True)  "<expr>")  "main entry point, showable expression."
    , Option ['e'] []            (ReqArg (\d -> optStmts_u (d:)) "<statement>")  "run given statement as if on jhci prompt"
    , Option []    ["debug"]     (NoArg  (optDebug_s True))            "debugging"
    , Option []    ["show-ho"]   (ReqArg  (optMode_s . ShowHo) "file.ho") "Show ho file"
    , Option []    ["noauto"]    (NoArg  (optNoAuto_s True))           "Don't automatically load base and haskell98 packages"
    , Option ['p'] []            (ReqArg (\d -> optHls_u (++ [d])) "file.hl") "Load given haskell library .hl file"
    , Option ['L'] []            (ReqArg (optHlPath_u . idu) "path")   "Look for haskell libraries in the given directory."
    , Option []    ["build-hl"]  (ReqArg (optMode_s . BuildHl) "file.cabal") "Build hakell library from given library description file"
    , Option []    ["interactive"] (NoArg  (optMode_s Interactive))    "run interactivly"
    , Option []    ["ignore-ho"]   (NoArg  (optIgnoreHo_s True))       "Ignore existing haskell object files"
    , Option []    ["nowrite-ho"]  (NoArg  (optNoWriteHo_s True))      "Do not write new haskell object files"
    , Option []    ["no-ho"]       (NoArg  (optNoWriteHo_s True . optIgnoreHo_s True)) "same as --ignore-ho and --nowrite-ho"
    , Option []    ["no-follow-deps"] (NoArg  (optFollowDeps_s False)) "Don't follow depencies not listed on command line."
    , Option []    ["list-libraries"] (NoArg  (optMode_s ListLibraries)) "List of installed libraries."
    ]

-- | Width of terminal.
getColumns :: Int
getColumns = read $ unsafePerformIO (getEnv "COLUMNS" `mplus` return "80")


postProcessFD :: Monad m => Opt -> m Opt
postProcessFD o = case FlagDump.process (optDumpSet o) (optDump o ++ vv) of
        (s,[]) -> return $ o { optDumpSet = s }
        (_,xs) -> fail ("Unrecognized dump flag passed to '-d': "
                        ++ unwords xs ++ "\nValid dump flags:\n\n" ++ FlagDump.helpMsg)
    where
    vv | optVerbose o >= 2 = ["veryverbose"]
       | optVerbose o >= 1 = ["verbose"]
       | otherwise = []

postProcessFO :: Monad m => Opt -> m Opt
postProcessFO o = case FlagOpts.process (optFOptsSet o) (optFOpts o) of
        (s,[]) -> return $ o { optFOptsSet = s }
        (_,xs) -> fail ("Unrecognized flag passed to '-f': "
                        ++ unwords xs ++ "\nValid flags:\n\n" ++ FlagOpts.helpMsg)

getArguments = do
    x <- lookupEnv "JHCOPTS"
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

{-# NOINLINE processOptions #-}
-- | Parse commandline options.
processOptions :: IO Opt
processOptions = do
    argv <- getArguments
    let header = "Usage: jhc [OPTION...] Main.hs"
    let mkoptlist d os = "valid " ++ d ++ " arguments: 'help' for more info\n    " ++ intercalate "\n    " (map (intercalate ", ") $ pfill 100 ((2 +) . length) os) ++ "\n"
    let trailer = "\n" ++ mkoptlist "-d" FlagDump.helpFlags ++ "\n" ++ mkoptlist "-f" FlagOpts.helpFlags
    let (o,ns,rc) = getOpt Permute theoptions argv
    when (rc /= []) $ putErrDie (concat rc ++ usageInfo header theoptions ++ trailer)
    o1 <- either putErrDie return $ postProcessFD (foldl (flip ($)) opt o)
    o2 <- either putErrDie return $ postProcessFO o1
    when (optMode o2 == ShowHelp) $ do
        putStrLn (usageInfo header theoptions ++ trailer)
        exitSuccess
    when (optMode o2 == ShowConfig) $ do
        mapM_ (\ (x,y) -> putStrLn (x ++ ": " ++ y))  configs
        exitSuccess
    case optNoAuto o2 of
      True -> return (o2 { optArgs = ns })
      False-> return (o2 { optArgs = ns, optHls  = ("base":"haskell98":optHls o2) })

a ==> b = (a,show b)

configs = [
    "jhclibpath" ==> initialLibIncludes,
    "version" ==> version,
    "package" ==> package,
    "libdir" ==> libdir,
    "datadir" ==> datadir,
    "libraryInstall" ==> libraryInstall,
    "host" ==> host
    ]


{-# NOINLINE fileOptions #-}
fileOptions :: Monad m => [String] -> m Opt
fileOptions xs = case getOpt Permute theoptions xs of
    (os,[],[]) -> postProcessFD (foldl (flip ($)) options os) >>= postProcessFO
    (_,_,errs) -> fail (concat errs)

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

-- | Is verbose > 0?
verbose :: Bool
verbose = optVerbose options > 0
-- | Is verbose > 1?
verbose2 :: Bool
verbose2 = optVerbose options > 1

-- | Test whether a dump flag is set.
dump :: FlagDump.Flag -> Bool
dump s = s `S.member` optDumpSet options
-- | Test whether an option flag is set.
fopts :: FlagOpts.Flag -> Bool
fopts s = s `S.member` optFOptsSet options
-- | Do the action when the suplied dump flag is set.
wdump :: (Monad m) => FlagDump.Flag -> m () -> m ()
wdump f = when (dump f)

-- | Is the \"lint\" option flag set?
flint :: Bool
flint = FlagOpts.Lint `S.member` optFOptsSet options

-- | Include directories taken from JHCPATH enviroment variable.
initialIncludes :: [String]
initialIncludes = unsafePerformIO $ do
    p <- lookupEnv "JHCPATH"
    let x = maybe "" id p
    return (".":(tokens (== ':') x))

-- | Include directories taken from JHCLIBPATH enviroment variable.
initialLibIncludes :: [String]
initialLibIncludes = unsafePerformIO $ do
    ps <- lookupEnv "JHCLIBPATH"
    h <- lookupEnv "HOME"
    let paths = h ++ ["/usr/local","/usr"]
        bases = ["/lib","/share"]
        vers = ["/jhc-" ++ shortVersion, "/jhc"]
    return $ nub $ maybe [] (tokens (':' ==))  ps ++ [ p ++ b ++ v | b <- bases, p <- paths, v <- vers ] ++ [libraryInstall]


class Monad m => OptionMonad m where
    getOptions :: m Opt
    getOptions = return options

instance OptionMonad Identity

newtype OptT m a = OptT (ReaderT Opt m a)
    deriving(MonadIO,Monad,Functor,MonadTrans)

type OptM = OptT Identity

instance Monad m => OptionMonad (OptT m) where
    getOptions = OptT ask



withOptions :: Opt -> OptM a -> a
withOptions opt (OptT x) = runIdentity (runReaderT x opt)

withOptionsT :: Opt -> OptT m a -> m a
withOptionsT opt (OptT x) = runReaderT x opt


flagOpt :: OptionMonad m => FlagOpts.Flag -> m Bool
flagOpt flag = do
    opt <- getOptions
    return (flag `S.member` optFOptsSet opt)

getArgString = do
    name <- System.getProgName
    args <- getArguments
    return (simpleQuote (name:args),head $ lines versionString)
