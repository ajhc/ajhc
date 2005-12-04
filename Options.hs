{-# OPTIONS -w -funbox-strict-fields #-}
module Options(processOptions, Opt(..), options, putVerbose, putVerboseLn, verbose, verbose2, dump, wdump, fopts, flint, fileOptions, versionString) where

import Data.Version
import Monad
import qualified Data.Set as S
import System
import System.Console.GetOpt
import System.Info
import System.IO.Unsafe

import GenUtil
import qualified FlagDump
import qualified FlagOpts
import {-# SOURCE #-} SelfTest(selfTest)
import Version
import VersionCtx

data Opt = Opt {
    optColumns     :: !Int,       -- ^ Width of terminal.
    optCompile     :: !Bool,      -- ^ Compile.
    optDebug       :: !Bool,      -- ^ Debugging.
    optSelfTest    :: !Bool,      -- ^ Perform self-test
    optDump        ::  [String],  -- ^ Dump options (raw).
    optStmts       ::  [String],  -- ^ statements to execute
    optFOpts       ::  [String],  -- ^ Flag options (raw).
    optIncdirs     ::  [String],  -- ^ Include directories.
    optProgArgs    ::  [String],  -- ^ Arguments to pass to the interpreted program.
    optShowHo      ::  [String],  -- ^ Show ho-file.
    optCCargs      ::  [String],  -- ^ Optional arguments to the C compiler.
    optHls         ::  [String],  -- ^ Load the specified hl-files (haskell libraries).
    optBuildHl     ::  String,    -- ^ Build a hl (haskell library) from the set of modules given.
    optCC          ::  String,    -- ^ C compiler.
    optArgs        ::  [String],
    optInteractive :: !Bool,      -- ^ Run interactively.
    optVersion     :: !Bool,      -- ^ Print version and die.
    optVersionCtx  :: !Bool,      -- ^ Print version context and die.
    optInterpret   :: !Bool,      -- ^ Interpret.
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
    optColumns     = getColumns,
    optCompile     = True,
    optDebug       = False,
    optSelfTest    = False,
    optIncdirs     = initialIncludes,
    optHls         = [],
    optBuildHl     = "",
    optProgArgs    = [],
    optDump        = [],
    optStmts       = [],
    optFOpts       = ["default"],
    optShowHo      = [],
    optCCargs      = [],
    optCC          = "gcc",
    optArgs        = [],
    optInteractive = False,
    optIgnoreHo    = False,
    optNoWriteHo   = False,
    optInterpret   = False,
    optKeepGoing   = False,
    optMainFunc    = Nothing,
    optOutName     = "hs.out",
    optPrelude     = True,
    optVerbose     = 0,
    optVersion     = False,
    optVersionCtx  = False,
    optNoAuto      = True,
    optDumpSet     = S.empty,
    optFOptsSet    = S.empty
}

idu "-" _ = []
idu d ds = ds ++ [d]

theoptions :: [OptDescr (Opt -> Opt)]
theoptions =
    [ Option ['V'] ["version"]   (NoArg  (optVersion_s True))    "print version info and exit"
    , Option []    ["version-context"] (NoArg  (optVersionCtx_s True)) "print version context (darcs changes) info and exit"
    , Option ['v'] ["verbose"]   (NoArg  (optVerbose_u (+1)))    "chatty output on stderr"
    , Option ['d'] []            (ReqArg (\d -> optDump_u (d:)) "dump-flag")  "dump specified data to stdout"
    , Option ['f'] []            (ReqArg (\d -> optFOpts_u (d:)) "flag")  "set compilation options"
    , Option ['o'] ["output"]    (ReqArg (optOutName_s) "FILE")  "output to FILE"
    , Option ['i'] ["include"]   (ReqArg (\d -> optIncdirs_u (idu d)) "DIR") "library directory"
    , Option []    ["optc"]      (ReqArg (\d -> optCCargs_u (idu d)) "option") "extra options to pass to c compiler"
    , Option []    ["progc"]     (ReqArg (\d -> optCC_s d) "CC") "c compiler to use"
    , Option []    ["arg"]       (ReqArg (\d -> optProgArgs_u (++ [d])) "arg") "arguments to pass interpreted program"
    , Option ['N'] ["noprelude"] (NoArg  (optPrelude_s False))   "no implicit prelude"
    , Option ['C'] ["justcheck"] (NoArg  (optCompile_s False))   "don't compile. just typecheck."
    , Option ['I'] ["interpret"] (NoArg  (optInterpret_s True . optCompile_s False)) "interpret."
    , Option ['k'] ["keepgoing"] (NoArg  (optKeepGoing_s True))  "keep going on errors."
    , Option []    ["width"]     (ReqArg (optColumns_s . read) "COLUMNS") "width of screen for debugging output."
    , Option ['m'] ["main"]      (ReqArg (optMainFunc_s . Just . (,) False) "Main.main")  "main entry point."
    , Option []    ["entry"]     (ReqArg (optMainFunc_s . Just . (,) True)  "<expr>")  "main entry point, showable expression."
    , Option ['e'] []            (ReqArg (\d -> optStmts_u (d:)) "<statement>")  "run given statement as if on jhci prompt"
    , Option []    ["debug"]     (NoArg  (optDebug_s True)) "debugging"
    , Option []    ["show-ho"]   (ReqArg  (\d -> optShowHo_u (++ [d])) "file.ho") "Show ho file"
    , Option []    ["noauto"]    (NoArg  (optNoAuto_s True)) "Don't automatically load base and haskell98 packages"
    , Option ['p'] []            (ReqArg (\d -> optHls_u (++ [d])) "file.hl") "Load given haskell library .hl file"
    , Option []    ["build-hl"]  (ReqArg (\d -> optBuildHl_s d) "file.hl") "Build hakell library from given list of modules"
    , Option []    ["interactive"] (NoArg  (optInteractive_s True)) "run interactivly"
    , Option []    ["ignore-ho"]  (NoArg  (optIgnoreHo_s True)) "Ignore existing haskell object files"
    , Option []    ["nowrite-ho"] (NoArg  (optNoWriteHo_s True)) "Do not write new haskell object files"
    , Option []    ["selftest"]   (NoArg  (optSelfTest_s True)) "Perform internal integrity testing"
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

versionString = concat ["jhc ", jhcVersion, " ", compileDate, " (", darcsTag, "+",darcsPatches, ")\n"]
                 ++ "compiled by " ++ compilerName ++ "-" ++ showVersion compilerVersion ++ " on a " ++ arch ++ " running " ++ os

{-# NOINLINE processOptions #-}
-- | Parse commandline options.
processOptions :: IO Opt
processOptions = do
    argv <- System.getArgs
    let header = "Usage: jhc [OPTION...] Main.hs"
    case (getOpt Permute theoptions argv) of
	  (o,ns,[]) -> case postProcess (foldl (flip ($)) opt o) of
                (o,"") -> case postProcess' o of
                    (Opt { optVersion = True },_) -> do
                        putStrLn versionString
                        exitSuccess
                    (Opt { optVersionCtx = True },_) -> do
                        putStrLn changes_txt
                        exitSuccess
                    (Opt { optSelfTest = True},_) -> do
                        putStrLn "Starting self testing..."
                        SelfTest.selfTest ns
                        exitSuccess
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
