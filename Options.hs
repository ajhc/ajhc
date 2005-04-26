{-# OPTIONS -w -funbox-strict-fields #-}
module Options(processOptions, Opt(..), options, putVerbose, putVerboseLn, verbose, verbose2, dump, wdump, fopts, flint, fileOptions) where

import Control.Monad.Error
import Data.Version
import GenUtil
import Monad
import qualified Data.Set as S
import qualified FlagDump
import qualified FlagOpts
import System
import System.Console.GetOpt
import System.Info
import System.IO.Unsafe
import Version

data Opt = Opt {
    optColumns     :: !Int,
    optCompile     :: !Bool,
    optDebug       :: !Bool,
    optDump        ::  [String],
    optFOpts       ::  [String],
    optIncdirs     ::  [String],
    optProgArgs    ::  [String],
    optShowHo      ::  [String],
    optCCargs      ::  [String],
    optHls         ::  [String],
    optBuildHl     ::  String,
    optCC          ::  String,
    optArgs        ::  [String],
    optInteractive :: !Bool,
    optVersion     :: !Bool,
    optInterpret   :: !Bool,
    optKeepGoing   :: !Bool,
    optMainFunc    ::  Maybe (Bool,String),
    optOutName     ::  String,
    optPrelude     :: !Bool,
    optIgnoreHo    :: !Bool,
    optNoWriteHo   :: !Bool,
    optVerbose     :: !Int,
    optDumpSet     ::  S.Set FlagDump.Flag,
    optFOptsSet    ::  S.Set FlagOpts.Flag
  } deriving(Show) {-!derive: update !-}


opt = Opt {
    optColumns     = getColumns,
    optCompile     = True,
    optDebug       = False,
    optIncdirs     = initialIncludes,
    optHls         = [],
    optBuildHl     = "",
    optProgArgs    = [],
    optDump        = [],
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
    optDumpSet     = S.empty,
    optFOptsSet    = S.empty
}

idu "-" _ = []
idu d ds = ds ++ [d]

theoptions :: [OptDescr (Opt -> Opt)]
theoptions =
    [ Option ['V'] ["version"]   (NoArg  (optVersion_s True))    "print version info and exit"
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
    , Option ['e'] []            (ReqArg (optMainFunc_s . Just . (,) True)  "<expr>")  "main entry point, showable expression."
    , Option []    ["debug"]     (NoArg  (optDebug_s True)) "debugging"
    , Option []    ["show-ho"]   (ReqArg  (\d -> optShowHo_u (++ [d])) "file.ho") "Show ho file"
    , Option ['p'] []            (ReqArg (\d -> optHls_u (++ [d])) "file.hl") "Load given haskell library .hl file"
    , Option []    ["build-hl"]  (ReqArg (\d -> optBuildHl_s d) "file.hl") "Build hakell library from given list of modules"
    , Option []    ["interactive"] (NoArg  (optInteractive_s True)) "run interactivly"
    , Option []    ["ignore-ho"] (NoArg  (optIgnoreHo_s True)) "Ignore existing haskell object files"
    , Option []    ["nowrite-ho"] (NoArg  (optNoWriteHo_s True)) "Do not write new haskell object files"
    ]

getColumns :: Int
getColumns = read $ unsafePerformIO (getEnv "COLUMNS" `mplus` return "80")


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
processOptions = do
    argv <- System.getArgs
    let header = "Usage: jhc [OPTION...] Main.hs"
    case (getOpt Permute theoptions argv) of
	  (o,ns,[]) -> case postProcess (foldl (flip ($)) opt o) of
                (o,"") -> case postProcess' o of
                    (Opt { optVersion = True },_) -> do
                        putStr $ "jhc compiled by " ++ compilerName ++ "-" ++ showVersion compilerVersion
                        putStrLn $ " on a " ++ arch ++ " running " ++ os
                        putStrLn changes_txt
                        exitSuccess
                    (o,"") -> return (o { optArgs = ns })
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
options :: Opt
options = unsafePerformIO processOptions

putVerbose s = when (optVerbose options > 0) $ putErr s
putVerboseLn s = putVerbose (s ++ "\n")

verbose = optVerbose options > 0
verbose2 = optVerbose options > 1

--dump s = s `S.member` S.fromList (optDump options)

dump s = s `S.member` optDumpSet options
fopts s = s `S.member` optFOptsSet options
wdump f = when (dump f)

flint = FlagOpts.Lint `S.member` optFOptsSet options

initialIncludes = unsafePerformIO $ do
    p <- lookupEnv "JHCPATH"
    Just x <- return $  p `mplus` Just ""
    return (".":(tokens (== ':') x))

