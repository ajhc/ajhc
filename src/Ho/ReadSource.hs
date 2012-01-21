-- routines dealing with reading and preprocessing source code files.
module Ho.ReadSource(
   preprocess,
   preprocessHs,
   parseHsSource
) where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Foreign.C
import System.Directory
import System.FilePath as FP
import System.Process
import System.Random (randomIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBSU
import qualified Data.Set as Set

import FrontEnd.HsParser
import FrontEnd.HsSyn
import FrontEnd.ParseMonad
import Support.TempDir
import FrontEnd.Syn.Options
import FrontEnd.Unlit
import FrontEnd.Warning
import Options
import RawFiles(prelude_m4)
import Util.FilterInput
import Util.Gen
import Version.Config(revision,version)
import qualified FlagDump as FD
import qualified FlagOpts as FO

preprocessHs :: FilePath -> LBS.ByteString -> IO LBS.ByteString
preprocessHs fn lbs = preprocess (collectFileOpts fn (LBSU.toString $ LBS.take 2048 lbs)) fn lbs

preprocess :: Opt -> FilePath -> LBS.ByteString -> IO LBS.ByteString
preprocess opt fn lbs = do
    let fopts s = s `Set.member` optFOptsSet opt
        incFlags = [ "-I" ++ d | d <- optIncdirs options ++ optIncs opt]
        defFlags = ("-D__JHC__=" ++ revision):("-D__JHC_VERSION__=" ++ version):[ "-D" ++ d | d <- optDefs opt]
    case () of
        _ | fopts FO.Cpp -> readSystem "cpp" $ ["-CC","-traditional"] ++ incFlags ++ defFlags ++ [fn]
          | fopts FO.M4 -> do
            m4p <- m4Prelude
            result <- readSystem "m4" $ ["-s", "-P"] ++ incFlags ++ defFlags ++ [m4p,fn]
            removeFile m4p >> return result
          | otherwise -> return lbs

m4Prelude :: IO FilePath
m4Prelude = do
    fp <- fileInTempDir "prelude.m4"
    BS.writeFile fp prelude_m4
    return fp

collectFileOpts fn s = opt where
    Just opt = fileOptions opts `mplus` Just options
    popts = parseOptions $ if FP.takeExtension fn == ".lhs" then unlit fn s else s
    opts' = concat [ words as | (x,as) <- popts, x `elem` ["OPTIONS","JHC_OPTIONS","OPTIONS_JHC"]]
    opts = opts' ++ [ "--noprelude" | ("NOPRELUDE",_) <- popts] ++ langs
    langs = catMaybes $ map (flip lookup langmap) $ concat [ words (map (\c -> if c == ',' then ' ' else toLower c) as) | ("LANGUAGE",as) <- popts ]

langmap = [
    "m4" ==> "m4",
    "cpp" ==> "cpp",
    "foreignfunctioninterface" ==> "ffi",
    "noimplicitprelude" ==> "--noprelude",
    "unboxedtuples" ==> "unboxed-tuples"
    ] where x ==> y = (x,if head y == '-' then y else "-f" ++ y)

parseHsSource :: FilePath -> LBS.ByteString -> IO (HsModule,LBS.ByteString)
parseHsSource fp@(FP.splitExtension -> (base,".hsc")) _ = do
    let out = FP.takeFileName base ++ ".hs"
    tdir <- getTempDir
    let incFlags = [ "-I" ++ d | d <- optIncdirs options ++ optIncs options]
    let hscargs =   [fp, "-o", tdir </> out] ++ incFlags
    when verbose $
        print ("hsc2hs",hscargs)
    rawSystem "hsc2hs" hscargs
    print tdir
    print out
    lbs <- LBS.readFile $ tdir </> out
    parseHsSource out lbs

parseHsSource fn lbs = do
    let fileOpts = collectFileOpts fn (LBSU.toString $ LBS.take 2048 lbs)
    lbs' <- preprocess fileOpts fn lbs
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
    case runParserWithMode (parseModeOptions $ collectFileOpts fn s) { parseFilename = fn } parse  s'  of
                      (ws,ParseOk e) -> processErrors ws >> return (e,LBSU.fromString s')
                      (_,ParseFailed sl err) -> putErrDie $ show sl ++ ": " ++ err
