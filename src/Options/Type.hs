{-# OPTIONS -w -funbox-strict-fields #-}
module Options.Type(
    StopCondition(..),
    theoptions,Opt(..),
    Mode(..),
    optFOptsSet_u,
    prettyOptions,
    emptyOpt,
    postProcessFD
    ,postProcessFO
    ,fileOptions) where

import Data.List(intercalate)
import System.Console.GetOpt

import Options.Map
import Util.DocLike
import qualified Data.Map as M
import qualified Data.Set as S
import qualified FlagDump as FD
import qualified FlagOpts as FO

{-# NOINLINE theoptions #-}
theoptions :: [OptDescr (Opt -> Opt)]
theoptions =
    [Option ['V'] ["version"]         (NoArg  (optMode_s Version))          "print version info and exit"
--    ,Option []    ["version-context"] (NoArg  (optMode_s VersionCtx))       "print version context info and exit"
    ,Option ['h'] ["help"]            (NoArg  (optMode_s ShowHelp))         "print help information and exit"
    ,Option []    ["info"]            (NoArg  (optMode_s ShowConfig))       "show compiler configuration information and exit"
    ,Option []    ["purge-cache"]     (NoArg  (optMode_s PurgeCache))       "clean out jhc compilation cache"
    ,Option ['v'] ["verbose"]         (NoArg  (optVerbose_u (+1)))          "chatty output on stderr"
    ,Option ['z'] []                  (NoArg  (optStatLevel_u (+1)))        "Increase verbosity of statistics"
    ,Option ['d'] []                  (ReqArg (optDump_u . (:))  "[no-]flag") "dump specified data during compilation"
    ,Option ['f'] []                  (ReqArg (optFOpts_u . (:)) "[no-]flag") "set or clear compilation options"
    ,Option ['X'] []                  (ReqArg (optExtensions_u . (:))  "Extension") "enable the given language extension"
    ,Option ['o'] ["output"]          (ReqArg (optOutName_s . Just) "FILE") "output to FILE"
    ,Option ['i'] ["include"]         (ReqArg (optIncdirs_u . idu) "DIR")   "where to look for source files"
    ,Option ['I'] []                  (ReqArg (optIncs_u . idu) "DIR")       "add to preprocessor include path"
    ,Option []    ["with"]            (ReqArg (optWith_u . idu) "foo.yaml") "include values from yaml file in configuration"
    ,Option ['D'] []                  (ReqArg (optDefs_u . (:)) "NAME=VALUE") "add new definitions to set in preprocessor"
    ,Option []    ["optc"]            (ReqArg (optCCargs_u . idu) "option") "extra options to pass to c compiler"
    ,Option ['c'] []                  (NoArg  (optStop_s CompileHo))        "just compile the modules, caching the results."
    ,Option ['C'] []                  (NoArg  (optStop_s StopC))            "compile to C code"
    ,Option ['E'] []                  (NoArg  (optMode_s Preprocess))       "preprocess the input and print result to stdout"
    ,Option ['k'] ["keepgoing"]       (NoArg  (optKeepGoing_s True))        "keep going on errors"
    ,Option []    ["cross"]           (NoArg  (optCross_s True))            "enable cross-compilation, choose target with the -m flag"
    ,Option []    ["stop"]            (ReqArg (optStop_s . stop) "parse/typecheck/c") "stop after the given pass, parse/typecheck/c"
    ,Option []    ["width"]           (ReqArg (optColumns_s . read) "COLUMNS") "width of screen for debugging output"
    ,Option []    ["main"]            (ReqArg (optMainFunc_s . Just . (,) False) "Main.main")  "main entry point"
    ,Option ['m'] ["arch"]            (ReqArg (optArch_u . idu ) "arch")      "target architecture options"
    ,Option []    ["entry"]           (ReqArg (optMainFunc_s . Just . (,) True)  "<expr>")  "main entry point, showable expression"
--  ,Option ['e'] []                  (ReqArg (\d -> optStmts_u ( d:)) "<statement>")  "run given statement as if on jhci prompt"
    ,Option []    ["show-ho"]         (ReqArg (optMode_s . ShowHo) "file.ho") "Show contents of ho or hl file"
    ,Option []    ["noauto"]          (NoArg  (optNoAuto_s True))           "Don't automatically load base and haskell98 packages"
    ,Option ['p'] []                  (ReqArg (optHls_u . idu) "package")   "Load given haskell library package"
    ,Option ['L'] []                  (ReqArg (optHlPath_u . idu) "path")   "Look for haskell libraries in the given directory"
    ,Option []    ["build-hl"]        (ReqArg (optMode_s . BuildHl) "desc.yaml") "Build hakell library from given library description file"
    ,Option []    ["annotate-source"] (ReqArg (optAnnotate_s . Just) "<dir>") "Write preprocessed and annotated source code to the directory specified"
    ,Option []    ["deps"]            (ReqArg (optDeps_s . Just) "<file.yaml>") "Write dependency information to file specified"
    ,Option []    ["interactive"]     (NoArg  (optMode_s Interactive))      "run interactivly ( for debugging only)"
    ,Option []    ["ignore-cache"]    (NoArg  (optIgnoreHo_s True))         "Ignore existing compilation cache entries."
    ,Option []    ["readonly-cache"]  (NoArg  (optNoWriteHo_s True))        "Do not write new information to the compilation cache."
    ,Option []    ["no-cache"]        (NoArg  (optNoWriteHo_s True . optIgnoreHo_s True)) "Do not use or update the cache."
    ,Option []    ["cache-dir"]       (ReqArg (optHoCache_s . Just ) "JHC_CACHE")  "Use a global cache located in the directory passed as an argument."
    ,Option []    ["stale"]           (ReqArg (optStale_u . idu) "Module")  "Treat these modules as stale, even if they exist in the cache"
    ,Option []    ["list-libraries"]  (NoArg  (optMode_s ListLibraries))    "List of installed libraries"
    ,Option []    ["tdir"]            (ReqArg (optWorkDir_s . Just) "dir/") "specify the directory where all intermediate files/dumps will be placed."
--   , Option []    ["print-hsc-options"] (NoArg (optMode_s PrintHscOptions)) "print options to pass to hsc2hs"
    ]

data Mode
    = BuildHl FilePath         -- ^ Build the specified hl-file given a description file.
    | Interactive              -- ^ Run interactively.
    | Version                  -- ^ Print version and die.
--    | VersionCtx               -- ^ Print version context and die.
    | ShowHelp                 -- ^ Show help message and die.
    | ShowConfig               -- ^ Show configuration info.
    | CompileExe               -- ^ Compile executable
    | ShowHo String            -- ^ Show ho-file.
    | ListLibraries            -- ^ List libraries
    | PrintHscOptions          -- ^ Print options for hsc2hs
    | PurgeCache               -- ^ Purge the cache
    | Preprocess               -- ^ Filter through preprocessor
    deriving(Eq)

data StopCondition
    = StopError String         -- ^ error
    | StopParse                -- ^ Just parse and rename modules then exit
    | StopTypeCheck            -- ^ Stop after type checking
    | StopC                    -- ^ Stop after producing C code.
    | CompileHo                -- ^ Compile ho
    | StopNot                  -- ^ Don't stop believing.
    deriving(Eq)

data Opt = Opt {
    optMode        :: !Mode,      -- ^ Mode of interaction
    optColumns     :: !Int,       -- ^ Width of terminal.
    optDump        ::  [String],  -- ^ Dump options (raw).
    optStmts       ::  [String],  -- ^ statements to execute
    optFOpts       ::  [String],  -- ^ Flag options (raw).
    optIncdirs     ::  [String],  -- ^ Include directories.
    optCCargs      ::  [String],  -- ^ Optional arguments to the C compiler.
    optHls         ::  [String],  -- ^ Load the specified hl-files (haskell libraries).
    optAutoLoads   ::  [String],  -- ^ AutoLoaded haskell libraries.
    optHlPath      ::  [String],  -- ^ Path to look for libraries.
    optIncs        ::  [String],
    optWith        ::  [String],
    optDefs        ::  [String],
    optExtensions  ::  [String],
    optStop        :: !StopCondition,
    optWorkDir     ::  Maybe FilePath,
    optAnnotate    ::  Maybe FilePath,
    optDeps        ::  Maybe FilePath,
    optHoDir       ::  Maybe FilePath,
    optHoCache     ::  Maybe FilePath,
    optArgs        ::  [String],
    optStale       ::  [String],  -- ^ treat these modules as stale
    optKeepGoing   :: !Bool,      -- ^ Keep going when encountering errors.
    optMainFunc    ::  Maybe (Bool,String),    -- ^ Entry point name for the main function.
    optArch        ::  [String],           -- ^ target architecture
    optCross       :: !Bool,
    optOutName     ::  Maybe String,           -- ^ Name of output file.
    optIgnoreHo    :: !Bool,                   -- ^ Ignore ho-files.
    optNoWriteHo   :: !Bool,                   -- ^ Don't write ho-files.
    optNoAuto      :: !Bool,                   -- ^ Don't autoload packages
    optVerbose     :: !Int,                    -- ^ Verbosity
    optStatLevel   :: !Int,                    -- ^ Level to print statistics
    optInis        ::  M.Map String String,    -- ^ options read from ini files
    optDumpSet     ::  S.Set FD.Flag,    -- ^ Dump flags.
    optFOptsSet    ::  S.Set FO.Flag     -- ^ Flag options (-f\<opt\>).
  } {-!derive: update !-}

emptyOpt = Opt {
    optMode        = CompileExe,
    optColumns     = 80,
    optCross       = False,
    optIncdirs     = [],
    optAnnotate    = Nothing,
    optDeps        = Nothing,
    optHls         = [],
    optAutoLoads   = [],
    optHlPath      = [],
    optIncs        = [],
    optWith        = [],
    optDefs        = [],
    optExtensions  = [],
    optStop        = StopNot,
    optDump        = [],
    optStale       = [],
    optStmts       = [],
    optFOpts       = ["default"],
    optCCargs      = [],
    optWorkDir     = Nothing,
    optHoDir       = Nothing,
    optHoCache     = Nothing,
    optArgs        = [],
    optIgnoreHo    = False,
    optNoWriteHo   = False,
    optKeepGoing   = False,
    optMainFunc    = Nothing,
    optArch        = ["default"],
    optOutName     = Nothing,
    optInis        = M.empty,
    optVerbose     = 0,
    optStatLevel   = 1,
    optNoAuto      = False,
    optDumpSet     = S.singleton FD.Progress,
    optFOptsSet    = languageDefault
}

idu "-" _ = []
idu d ds = ds ++ [d]

stop "parse" = StopParse
stop "deps" = StopParse
stop "typecheck" = StopTypeCheck
stop "c" = StopC
stop s = StopError s

{-# NOINLINE fileOptions #-}
fileOptions :: Monad m => Opt -> [String] -> m Opt
fileOptions options xs = case getOpt Permute theoptions xs of
    (os,[],[]) -> postProcessFD (foldl (flip ($)) options os) >>= postProcessFO
    (_,_,errs) -> fail (concat errs)

postProcessFD :: Monad m => Opt -> m Opt
postProcessFD o = case FD.process (optDumpSet o) (optDump o ++ vv) of
        (s,[]) -> return $ o { optDumpSet = s, optDump = [] }
        (_,xs) -> fail ("Unrecognized dump flag passed to '-d': "
                        ++ unwords xs ++ "\nValid dump flags:\n\n" ++ FD.helpMsg)
    where
    vv | optVerbose o >= 2 = ["veryverbose"]
       | optVerbose o >= 1 = ["verbose"]
       | otherwise = []

postProcessFO :: Monad m => Opt -> m Opt
postProcessFO o = case FO.process (optFOptsSet o) (optFOpts o) of
        (s,[]) -> return $ o { optFOptsSet = s, optFOpts = [] }
        (_,xs) -> fail ("Unrecognized flag passed to '-f': "
                        ++ unwords xs ++ "\nValid flags:\n\n" ++ FO.helpMsg)

{-# NOINLINE prettyOptions #-}
prettyOptions :: String
prettyOptions =  showSD $ vcat ([h1 "Usage Examples"] ++ usages
        ++ [h1 "Option Flags"] ++ flags) where
    h1 t = text "# " <> text t $$ text ""
    flags = dashes $+$ header $+$ hdash $+$ vsep (map f theoptions) $+$ dashes
    cmd =  text "    ./jhc [OPTION]"
    dashes = text $ replicate 40 '-'
    usages =
        [cmd <+> text "File.hs   # compile given module"
        ,cmd <+> text "--build-hl libdef.yaml   # compile library described by file"]
    f (Option cs vs arg msg) = pads [unwords sflg,unwords mflg,msg] where
        sflg = map (\c -> ['-',c,' '] ++ rg) cs
        mflg =  map (\s -> "--" ++ s ++ " " ++ rg) vs
        rg = rarg arg
    rarg NoArg {} = ""
    rarg (ReqArg _ n) = n
    vsep xs = vcat $ intercalate (text "") xs
    hdash = pads ["----","----","--------"]
    header = pads ["flag","","description"]
    pads (x:y:xs) = hsep $ pad 18 x:pad 25 y:[ pad 20 x | x <- xs ]
    pad n t = text $ t ++ replicate (n - length t) ' '
