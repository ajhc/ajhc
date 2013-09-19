-- routines dealing with reading and preprocessing source code files.
module Ho.ReadSource(
   preprocess,
   preprocessHs,
   languageFlags,
   fetchCompilerFlags,
   parseHsSource
) where

import Control.Monad
import Data.Char
import Data.Maybe
import System.FilePath as FP
import System.Process
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBSU
import qualified Data.Map as Map
import qualified Data.Set as Set

import FrontEnd.HsParser
import FrontEnd.HsSyn
import FrontEnd.ParseMonad
import FrontEnd.SrcLoc
import FrontEnd.Syn.Options
import FrontEnd.Unlit
import FrontEnd.Warning
import Options
import PackedString
import RawFiles(prelude_m4)
import Support.TempDir
import Util.FilterInput
import Util.Gen
import Version.Config(revision,version)
import qualified FlagDump as FD
import qualified FlagOpts as FO

preprocessHs :: Opt -> FilePath -> LBS.ByteString -> IO LBS.ByteString
preprocessHs options fn lbs = preprocess (fst $ collectFileOpts options fn (LBSU.toString $ LBS.take 2048 lbs)) fn lbs

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

collectFileOpts options fn s = (lproc opt,isJust fopts)  where
    copts os = [ as | (x,as) <- popts, x `elem` os]
    Just opt = fopts `mplus` Just options
    fopts = fileOptions options opts
    popts = parseOptions $ if FP.takeExtension fn == ".lhs" then unlit fn s else s
    opts = concatMap words (copts ["OPTIONS","JHC_OPTIONS","OPTIONS_JHC"])
    (pfs,nfs,_) = languageFlags $ concatMap (words . (map (\c -> if c == ',' then ' ' else c)))
        (copts ["LANGUAGE","JHC_LANGUAGE"] ++ optExtensions options ++ [ o | '-':'X':o <- opts])
    lproc opt = opt { optFOptsSet = Set.union pfs (optFOptsSet opt) Set.\\ nfs }

-- translates a list of language extensions as pased to a LANGUAGE pragma or
-- the -X option to the equivalent '-f' flags. The first return value are the
-- positive flags, the negative flags, and the third is the unrecognized extensions.
languageFlags :: [String] -> (Set.Set FO.Flag,Set.Set FO.Flag,[String])
languageFlags ls = f ls Set.empty Set.empty [] where
    f [] pfs nfs us = (pfs,nfs,us)
    f (l:ls) pfs nfs us | Just lo <- Map.lookup ll langmap =  f ls (Set.union lo pfs) nfs us
                        | 'n':'o':ll <- ll, Just lo <- Map.lookup ll langmap = f ls pfs (nfs `Set.union` lo) us
                        | otherwise = f ls pfs nfs (l:us)
        where ll = map toLower l

langmap = Map.fromList [
    "m4" ==> FO.M4,
    "cpp" ==> FO.Cpp,
    "foreignfunctioninterface" ==> FO.Ffi,
    "implicitprelude" ==> FO.Prelude,
    "unboxedtuples" ==> FO.UnboxedTuples,
    "unboxedvalues" ==> FO.UnboxedValues,
    "monomorphismrestriction" ==> FO.MonomorphismRestriction,
    "explicitforall" ==> FO.Forall,
    "existentialquantification" =+> [FO.Forall,FO.Exists],
    "scopedtypevariables" ==> FO.Forall,
    "rankntypes" ==> FO.Forall,
    "rank2types" ==> FO.Forall,
    "bangpatterns" ==> FO.BangPatterns,
    "polymorphiccomponents" ==> FO.Forall,
    "TypeFamilies" ==> FO.TypeFamilies,
    "magichash" ==> FO.UnboxedValues
    ] where x ==> y = (x,Set.singleton y)
            x =+> y = (x,Set.fromList y)

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
    case runParserWithMode (parseModeOptions fileOpts') { parseFilename = fn } parse  s'  of
                      (ws,ParseOk e) -> processErrors ws >> return (e { hsModuleOpt = fileOpts' },LBSU.fromString s')
                      (_,ParseFailed sl err) -> putErrDie $ show sl ++ ": " ++ err

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
