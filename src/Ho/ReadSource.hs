-- routines dealing with reading and preprocessing source code files.
module Ho.ReadSource(
   preprocess,
   preprocessHs,
   fetchCompilerFlags,
   parseHsSource
) where

import Util.Std
import System.FilePath as FP
import System.Process
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBSU
import qualified Data.Map as Map

import FrontEnd.HsSyn
import FrontEnd.Syn.Options
import FrontEnd.Unlit
import FrontEnd.Warning
import Options
import Options.Map
import PackedString
import Support.TempDir
import Util.Gen
import qualified FlagDump as FD
import qualified FlagOpts as FO
import qualified FrontEnd.Lex.Parse as FLP

preprocessHs :: Opt -> FilePath -> LBS.ByteString -> IO LBS.ByteString
preprocessHs options fn lbs = preprocess (fst $ collectFileOpts options fn (LBSU.toString $ LBS.take 2048 lbs)) fn lbs

collectFileOpts options fn s = (lproc opt,isJust fopts)  where
    copts os = [ as | (x,as) <- popts, x `elem` os]
    Just opt = fopts `mplus` Just options
    fopts = fileOptions options opts
    popts = parseOptions $ if FP.takeExtension fn == ".lhs" then unlit fn s else s
    opts = concatMap words (copts ["OPTIONS","JHC_OPTIONS","OPTIONS_JHC"])
    lflags = concatMap (words . (map (\c -> if c == ',' then ' ' else c)))
        (copts ["LANGUAGE","JHC_LANGUAGE"] ++ [ o | '-':'X':o <- opts])
    lproc opt@Opt { optFOptsSet } = opt { optFOptsSet = fst $ processLanguageFlags lflags optFOptsSet }

parseHsSource :: Opt -> FilePath -> LBS.ByteString -> IO (HsModule,LBS.ByteString)
parseHsSource options fp@(FP.splitExtension -> (base,".hsc")) _ = do
    let out = FP.takeFileName base ++ ".hs"
    tdir <- getTempDir
    (cc,cflags) <- fetchCompilerFlags
    let incFlags = [ "-I" ++ d | d <- optIncdirs options ++ optIncs options]
        defFlags = [ "-D" ++ d | d <- optDefs options ]
    let hscargs =   [fp, "-o", tdir </> out] ++ defFlags ++ incFlags ++ concatMap (\x -> ["-C",x]) cflags ++ ["-c", cc]
    when verbose $
        print ("hsc2hs",hscargs)
    rawSystem "hsc2hs" hscargs
    print tdir
    print out
    lbs <- LBS.readFile $ tdir </> out
    parseHsSource options out lbs
parseHsSource options fn lbs = do
    lbs' <- preprocessHs options fn lbs
    let s = LBSU.toString lbs'
    let s' = if FP.takeExtension fn == ".lhs" then unlit fn s'' else s''
        s'' = case s of
            '#':' ':_   -> '\n':s                --  line pragma
            '#':'l':'i':'n':'e':' ':_  -> '\n':s --  line pragma
            '#':'!':_ -> dropWhile (/= '\n') s   --  hashbang
            _ -> s
    wdump FD.Preprocessed $ do
        putStrLn s'
    fn <- shortenPath fn
    let (fileOpts',ogood) = collectFileOpts options fn s
    unless ogood $
        warn (bogusASrcLoc { srcLocFileName = packString fn })
            UnknownOption "Invalid options in OPTIONS pragma"
    p <- FLP.parse fileOpts' fn s'
    return (p,LBSU.fromString s')

fetchCompilerFlags :: IO (FilePath,     -- ^ file path to compiler
                          [String])     -- ^ compiler arguments
fetchCompilerFlags = return (cc,args) where
    lup k = maybe "" id $ Map.lookup k (optInis options)
    boehmOpts | fopts FO.Boehm = ["-D_JHC_GC=_JHC_GC_BOEHM", "-lgc"]
              | fopts FO.Jgc   = ["-D_JHC_GC=_JHC_GC_JGC"]
              | otherwise = []
    profileOpts | fopts FO.Profile || lup "profile" == "true" = ["-D_JHC_PROFILE=1"]
                | otherwise = []
    debug = if fopts FO.Debug then words (lup "cflags_debug") else words (lup "cflags_nodebug")
    cc = lup "cc"
    args = words (lup "cflags") ++ debug ++ optCCargs options  ++ boehmOpts ++ profileOpts
