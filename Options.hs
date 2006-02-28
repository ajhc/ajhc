{-# OPTIONS -w -funbox-strict-fields #-}
module Options(
    processOptions,
    Opt(..),
    options,
    Mode(..),
    putVerbose,
    putVerboseLn,
    verbose,
    verbose2,
    dump,
    wdump,
    fopts,
    flint,
    fileOptions,
    withOptions,
    withOptionsT,
    OptM(),
    OptT(),
    OptionMonad(..),
    flagOpt
    ) where

import Control.Monad.Error()    -- IO MonadPlus instance
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Set as S
import System
import System.Console.GetOpt
import System.IO.Unsafe

import GenUtil
import qualified FlagDump
import qualified FlagOpts
import Version.Raw(basePackages, libraryPath)

data Mode = BuildHl String -- ^ Load the specified hl-files (haskell libraries).
          | Interactive    -- ^ Run interactively.
          | SelfTest       -- ^ Perform self-test
          | Version        -- ^ Print version and die.
          | VersionCtx     -- ^ Print version context and die.
          | Interpret      -- ^ Interpret.
          | CompileHo      -- ^ Compile ho
          | CompileHoGrin  -- ^ Compile ho and grin
          | CompileExe     -- ^ Compile executable
          | ShowHo String  -- ^ Show ho-file.
          | ListLibraries  -- ^ List libraries
            deriving(Eq,Show)

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
    optOutName     ::  String,                 -- ^ Name of output file.
    optPrelude     :: !Bool,                   -- ^ No implicit Prelude.
    optIgnoreHo    :: !Bool,                   -- ^ Ignore ho-files.
    optNoWriteHo   :: !Bool,                   -- ^ Don't write ho-files.
    optNoAuto      :: !Bool,                   -- ^ Don't autoload packages
    optVerbose     :: !Int,                    -- ^ Verbosity
    optDumpSet     ::  S.Set FlagDump.Flag,    -- ^ Dump flags.
    optFOptsSet    ::  S.Set FlagOpts.Flag     -- ^ Flag options (-f\<opt\>).
  } deriving(Show) {-!derive: update !-}


opt = Opt {
    optMode        = CompileExe,
    optColumns     = getColumns,
    optDebug       = False,
    optIncdirs     = initialIncludes,
    optHls         = [],
    optHlPath      = initialIncludes ++ libraryPath,
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
    optOutName     = "hs.out",
    optPrelude     = True,
    optVerbose     = 0,
    optNoAuto      = False,
    optDumpSet     = S.empty,
    optFOptsSet    = S.empty
}

idu "-" _ = []
idu d ds = ds ++ [d]

theoptions :: [OptDescr (Opt -> Opt)]
theoptions =
    [ Option ['V'] ["version"]   (NoArg  (optMode_s Version))    "print version info and exit"
    , Option []    ["version-context"] (NoArg  (optMode_s VersionCtx)) "print version context (darcs changes) info and exit"
    , Option ['v'] ["verbose"]   (NoArg  (optVerbose_u (+1)))    "chatty output on stderr"
    , Option ['d'] []            (ReqArg (\d -> optDump_u (d:)) "dump-flag")  "dump specified data to stdout"
    , Option ['f'] []            (ReqArg (\d -> optFOpts_u (d:)) "flag")  "set compilation options"
    , Option ['o'] ["output"]    (ReqArg (optOutName_s) "FILE")  "output to FILE"
    , Option ['i'] ["include"]   (ReqArg (optIncdirs_u . idu) "DIR") "library directory"
    , Option []    ["optc"]      (ReqArg (optCCargs_u . idu) "option") "extra options to pass to c compiler"
    , Option []    ["progc"]     (ReqArg (\d -> optCC_s d) "CC") "c compiler to use"
    , Option []    ["arg"]       (ReqArg (\d -> optProgArgs_u (++ [d])) "arg") "arguments to pass interpreted program"
    , Option ['N'] ["noprelude"] (NoArg  (optPrelude_s False))   "no implicit prelude"
    , Option ['C'] []            (NoArg  (optMode_s CompileHoGrin))   "Typecheck, compile ho and grin."
    , Option ['c'] []            (NoArg  (optMode_s CompileHo))   "Typecheck and compile ho."
    , Option ['I'] ["interpret"] (NoArg  (optMode_s Interpret)) "interpret."
    , Option ['k'] ["keepgoing"] (NoArg  (optKeepGoing_s True))  "keep going on errors."
    , Option []    ["width"]     (ReqArg (optColumns_s . read) "COLUMNS") "width of screen for debugging output."
    , Option ['m'] ["main"]      (ReqArg (optMainFunc_s . Just . (,) False) "Main.main")  "main entry point."
    , Option []    ["entry"]     (ReqArg (optMainFunc_s . Just . (,) True)  "<expr>")  "main entry point, showable expression."
    , Option ['e'] []            (ReqArg (\d -> optStmts_u (d:)) "<statement>")  "run given statement as if on jhci prompt"
    , Option []    ["debug"]     (NoArg  (optDebug_s True)) "debugging"
    , Option []    ["show-ho"]   (ReqArg  (optMode_s . ShowHo) "file.ho") "Show ho file"
    , Option []    ["noauto"]    (NoArg  (optNoAuto_s True)) "Don't automatically load base and haskell98 packages"
    , Option ['p'] []            (ReqArg (\d -> optHls_u (++ [d])) "file.hl") "Load given haskell library .hl file"
    , Option ['L'] []            (ReqArg (optHlPath_u . idu) "path") "Look for haskell libraries in the given directory."
    , Option []    ["build-hl"]  (ReqArg (optMode_s . BuildHl) "file.hl") "Build hakell library from given list of modules"
    , Option []    ["interactive"] (NoArg  (optMode_s Interactive)) "run interactivly"
    , Option []    ["ignore-ho"]  (NoArg  (optIgnoreHo_s True)) "Ignore existing haskell object files"
    , Option []    ["nowrite-ho"] (NoArg  (optNoWriteHo_s True)) "Do not write new haskell object files"
    , Option []    ["no-ho"]      (NoArg  (optNoWriteHo_s True . optIgnoreHo_s True)) "same as --ignore-ho and --nowrite-ho"
    , Option []    ["selftest"]   (NoArg  (optMode_s SelfTest)) "Perform internal integrity testing"
    , Option []    ["list-libraries"]   (NoArg  (optMode_s ListLibraries)) "List of installed libraries."
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

{-# NOINLINE processOptions #-}
-- | Parse commandline options.
processOptions :: IO Opt
processOptions = System.getArgs >>= (\argv -> either putErrDie return $ do
    let header = "Usage: jhc [OPTION...] Main.hs"
    let (o,ns,rc) = getOpt Permute theoptions argv
    when (rc /= []) $ fail (concat rc ++ usageInfo header theoptions)
    o1 <- postProcessFD (foldl (flip ($)) opt o)
    o2 <- postProcessFO o1
    case optNoAuto o2 of
      True -> return (o2 { optArgs = ns })
      False-> return (o2 { optArgs = ns, optHls  = basePackages ++ optHls o2 }))

{-# NOINLINE fileOptions #-}
fileOptions :: Monad m => [String] -> m Opt
fileOptions xs = case getOpt Permute theoptions xs of
    (os,[],[]) -> postProcessFD (foldl (flip ($)) options os)
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

