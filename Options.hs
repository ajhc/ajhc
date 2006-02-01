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

basePackages = ["base-0.1", "haskell98-0.1"]

data Mode = BuildHl String -- ^ Load the specified hl-files (haskell libraries).
          | Interactive    -- ^ Run interactively.
          | SelfTest       -- ^ Perform self-test
          | Version        -- ^ Print version and die.
          | VersionCtx     -- ^ Print version context and die.
          | Interpret      -- ^ Interpret.
          | CompileHo      -- ^ Compile ho
          | CompileExe     -- ^ Compile executable
          | ShowHo String  -- ^ Show ho-file.
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
    optNoAuto      = True,
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
    , Option ['i'] ["include"]   (ReqArg (\d -> optIncdirs_u (idu d)) "DIR") "library directory"
    , Option []    ["optc"]      (ReqArg (\d -> optCCargs_u (idu d)) "option") "extra options to pass to c compiler"
    , Option []    ["progc"]     (ReqArg (\d -> optCC_s d) "CC") "c compiler to use"
    , Option []    ["arg"]       (ReqArg (\d -> optProgArgs_u (++ [d])) "arg") "arguments to pass interpreted program"
    , Option ['N'] ["noprelude"] (NoArg  (optPrelude_s False))   "no implicit prelude"
    , Option ['C'] ["justcheck"] (NoArg  (optMode_s CompileHo))   "Typecheck and compile ho."
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
    , Option []    ["build-hl"]  (ReqArg (optMode_s . BuildHl) "file.hl") "Build hakell library from given list of modules"
    , Option []    ["interactive"] (NoArg  (optMode_s Interactive)) "run interactivly"
    , Option []    ["ignore-ho"]  (NoArg  (optIgnoreHo_s True)) "Ignore existing haskell object files"
    , Option []    ["nowrite-ho"] (NoArg  (optNoWriteHo_s True)) "Do not write new haskell object files"
    , Option []    ["selftest"]   (NoArg  (optMode_s SelfTest)) "Perform internal integrity testing"
    ]

-- | Width of terminal.
getColumns :: Int
getColumns = read $ unsafePerformIO (getEnv "COLUMNS" `mplus` return "80")


postProcess :: Opt -> (Opt, String)
postProcess o = case FlagDump.process (optDumpSet o) (optDump o ++ vv) of
        (s,errs) -> (o { optDumpSet = s }, f errs) where
                f [] = ""
                f xs = "Unrecognized dump flag passed to '-d': " ++ unwords xs ++ "\nValid dump flags:\n\n" ++ FlagDump.helpMsg
    where
    vv | optVerbose o >= 2 = ["veryverbose"]
       | optVerbose o >= 1 = ["verbose"]
       | otherwise = []

postProcess' o = case FlagOpts.process (optFOptsSet o) (optFOpts o) of
        (s,errs) -> (o { optFOptsSet = s }, f errs) where
                f [] = ""
                f xs = "Unrecognized flag passed to '-f': " ++ unwords xs ++ "\nValid flags:\n\n" ++ FlagOpts.helpMsg

{-# NOINLINE processOptions #-}
-- | Parse commandline options.
processOptions :: IO Opt
processOptions = do
    argv <- System.getArgs
    let header = "Usage: jhc [OPTION...] Main.hs"
    case (getOpt Permute theoptions argv) of
	  (o,ns,[]) -> case postProcess (foldl (flip ($)) opt o) of
                (o,"") -> case postProcess' o of
                    (o,"") -> case optNoAuto o of
                               True -> return (o { optArgs = ns })
                               False-> return (o { optArgs = ns, optHls  = basePackages ++ optHls o })
                    (_,err) -> putErrDie err
                (_,err) -> putErrDie err
	  --(_,_,[]) -> putErrDie (usageInfo header options)
	  (_,_,errs) -> putErrDie (concat errs ++ usageInfo header theoptions)

{-# NOINLINE fileOptions #-}
fileOptions :: Monad m => [String] -> m Opt
fileOptions xs = case getOpt Permute theoptions xs of
    (os,[],[]) -> case postProcess (foldl (flip ($)) options os) of
            (o,"") -> return o
            (_,err) -> fail err
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

