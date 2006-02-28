module Ho.Library
    (loadLibraries, createLibrary, parseLibraryDescription
    ) where

import HsSyn
import Ho.Build
import Ho.LibraryMap
import Ho.Type
import GenUtil
import Options(options, optHls)
import PackedString
import Util.MD5(md5file)

import Control.Monad(when,foldM)
import Data.List(sort)
import qualified Data.Map as Map
import Data.Monoid
import System.IO


data Library = Library {
    libraryDesc :: [(PackedString,PackedString)],
    libraryHo   :: Ho,
    libraryFP   :: FilePath,
    libraryMD5  :: CheckSum
    }
type LMap = Map.Map LibraryName Library

-- Load a library in a recursive fashion

libraryDeps :: Library -> [(LibraryName, CheckSum)]
libraryDeps = Map.toList . hoLibraries . libraryHo

loadP :: Maybe CheckSum -> LMap -> LibraryName -> IO LMap
loadP mbcs got name = do
    case Map.lookup name got of
      Nothing -> do
        rfp <- libraryMapFind name
        pkg <- readLibraryFile rfp mbcs
        let got' = Map.insert name pkg got
        foldM (\gm (pn,cs) -> loadP (Just cs) gm pn) got' $ libraryDeps pkg
      Just pkg | mbcs == Nothing               -> return got
               | mbcs == Just (libraryMD5 pkg) -> return got
               | otherwise                     -> fail ("Checksum mismatch for library "++name)

-- load libraries



loadLibraries :: IO Ho
loadLibraries = do
    ps <- foldM (loadP Nothing) Map.empty (optHls options)
    return $ mconcat (initialHo : map libraryHo (Map.elems ps))

-- Write a library and mutilate it to fit the description

createLibrary :: FilePath
              -> [(String,String)]
              -> IO ()
createLibrary fp desc = do
  let field x = lookup x desc
  let jfield x = maybe (error "createLibrary: description lacks required field "++show x) id $ field x
  let mfield x = maybe [] (words . map (\c -> if c == ',' then ' ' else c)) $ field x
  let name  = jfield "name"
      hmods = mfield "hidden-modules"
      emods = mfield "exposed-modules"
  let noc _ hsm = fail ("createLibrary: won't compile anything, requested: "++show (map hsModuleName hsm))
  let allmods  = sort $ map Module (emods ++ hmods)
  let fun ho m = do mho <- checkForHoModule m
                    case mho of
                      Nothing      -> fail (show fp++" depends not done.")
                      Just (_,ho') -> return $ mappend ho ho'
  ho <- foldM fun mempty allmods
  let homods = sort $ Map.keys (hoExports ho)
  when (homods /= allmods) $
      putErrDie ("Final ho consists of wrong modules:\nexpected: \t"
                 ++show allmods++"\nencountered: \t"++show homods)
  let ho' = ho { hoExports = Map.difference (hoExports ho)
                             (Map.fromList [(Module x,()) | x <- hmods]) }
  let pdesc = [(packString n, packString v) | (n,v) <- desc ]
  writeLibraryFile fp $ Library pdesc ho "" 0


parseLibraryDescription :: String -> [(String,String)]
parseLibraryDescription = map g . f . e . lines
    where e = filter (any (not . space))
          f (x:(c:r):t) | space c = f ((x++" "++dropWhile space r):t)
          f (x:t)                 = x : f t
          f []                    = []
          g l = let (h,(_:t)) = break (':'==) l in (h,dropWhile space t)
          space c = c == ' ' || c == '\t'

-- IO with Libraries


readLibraryFile :: FilePath -> Maybe CheckSum -> IO Library
readLibraryFile fp mbcs = do
    pkgCS <- md5file fp
    when (maybe False (pkgCS /=) mbcs) $
        putErrDie ("Loading library "++show fp++" failed: Checksum does not match")
    mho <- checkForHoFile fp
    case mho of
      Nothing       -> putErrDie ("Loading library "++fp++" failed due to missing dependencies")
      Just (hoh,ho) -> return $
          Library { libraryDesc= hohMetaInfo hoh,
                    libraryFP  = fp,
                    libraryMD5 = pkgCS,
                    libraryHo  = ho
                  }

writeLibraryFile :: FilePath -> Library -> IO ()
writeLibraryFile fp pkg = recordHoFile (libraryHo pkg) [fp] hoh >> return ()
    where hoh = HoHeader 1 [] [] (libraryDesc pkg)
