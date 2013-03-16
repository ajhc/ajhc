{-# LANGUAGE ImpredicativeTypes #-}
module Ho.Binary(readHoFile,recordHoFile,readHlFile,recordHlFile) where

import Codec.Compression.Zlib
import Control.Monad
import Data.Binary
import System.Posix.Files
import Text.Printf
import Util.Gen
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Version

import FrontEnd.Rename(FieldMap(..))
import Ho.Type
import Name.Binary()
import Options
import Support.CFF
import Support.MapBinaryInstance

current_version :: Int
current_version = 11

readHFile :: FilePath -> IO (FilePath,HoHeader,forall a . Binary a => ChunkType -> a)
readHFile fn = do
    bs <- BS.readFile fn
    fn' <- shortenPath fn
    (ct,mp) <- bsCFF bs
    True <- return $ ct == cff_magic
    let fc ct = case lookup ct mp of
            Nothing -> error $ "No chunk '" ++ show ct ++ "' found in file " ++ fn
            Just x -> decode . decompress $ LBS.fromChunks [x]
    let hoh = fc cff_jhdr
    when (hohVersion hoh /= current_version) $ fail "invalid version in hofile"
    return (fn',hoh,fc)

readHoFile :: FilePath -> IO (HoHeader,HoIDeps,Ho)
readHoFile fn = do
    (_fn,hoh,fc) <- readHFile fn
    let Left modGroup = hohName hoh
    return (hoh,fc cff_idep,Ho { hoModuleGroup = modGroup, hoTcInfo = fc cff_defs, hoBuild = fc cff_core})

recordHoFile ::
    Ho               -- ^ File to record
    -> HoIDeps
    -> [FilePath]    -- ^ files to write to
    -> HoHeader      -- ^ file header
    -> IO ()
recordHoFile ho idep fs header = do
    if optNoWriteHo options then do
        when verbose $ do
            fs' <- mapM shortenPath fs
            putErrLn $ "Skipping Writing Ho Files: " ++ show fs'
      else do
    let removeLink' fn = iocatch  (removeLink fn)  (\_ -> return ())
    let g (fn:fs) = do
            f fn
            mapM_ (l fn) fs
            return ()
        g [] = error "Ho.g: shouldn't happen"
        l fn fn' = do
            when verbose $ do
                fn_ <- shortenPath fn
                fn_' <- shortenPath fn'
                when (optNoWriteHo options) $ putErr "Skipping "
                putErrLn $ printf "Linking haskell object file: %s to %s" fn_' fn_
            if optNoWriteHo options then return () else do
            let tfn = fn' ++ ".tmp"
            removeLink' tfn
            createLink fn tfn
            rename tfn fn'
        f fn = do
            when verbose $ do
                when (optNoWriteHo options) $ putErr "Skipping "
                fn' <- shortenPath fn
                putErrLn $ "Writing haskell object file: " ++ fn'
            if optNoWriteHo options then return () else do
            let tfn = fn ++ ".tmp"
                cfflbs = mkCFFfile cff_magic [
                    (cff_jhdr, compress $ encode header { hohVersion = current_version }),
                    (cff_idep, compress $ encode idep),
                    (cff_defs, compress $ encode $ hoTcInfo ho),
                    (cff_core, compress $ encode $ hoBuild ho)]
            LBS.writeFile tfn cfflbs
            rename tfn fn
    g fs

recordHlFile
    :: Library
    -> IO ()
recordHlFile l = do
    --let theho =  mapHoBodies eraseE ho
    let cfflbs = mkCFFfile cff_magic $ [
            (cff_jhdr, compress $ encode (libHoHeader l) { hohVersion = current_version }),
            (cff_libr, compress $ encode $ libHoLib l),
            (cff_ldef, compress $ encode $ libTcMap l),
            (cff_lcor, compress $ encode $ libBuildMap l),
            (cff_file, compress $ encode $ libExtraFiles l)]
    let tfp = libFileName l ++ ".tmp"
    LBS.writeFile tfp cfflbs
    rename tfp $ libFileName l

readHlFile :: FilePath -> IO Library
readHlFile fn = do
    (_fn',hoh,fc) <- readHFile fn
    return Library { libHoHeader = hoh, libHoLib =  fc cff_libr,
        libTcMap = fc cff_ldef, libBuildMap = fc cff_lcor,
        libFileName = fn, libExtraFiles = fc cff_file }

instance Binary ExtraFile where
    put (ExtraFile a b) = put (a,b)
    get = do
        (x,y) <- get
        return $ ExtraFile x y

instance Binary FieldMap where
    put (FieldMap ac ad) = do
	    putMap ac
	    putMap ad
    get = do
    ac <- getMap
    ad <- getMap
    return (FieldMap ac ad)

instance Data.Binary.Binary HoHeader where
    put (HoHeader aa ab ac ad ae) = do
	    Data.Binary.put aa
	    Data.Binary.put ab
	    Data.Binary.put ac
	    Data.Binary.put ad
	    Data.Binary.put ae
    get = do
    aa <- get
    ab <- get
    ac <- get
    ad <- get
    ae <- get
    return (HoHeader aa ab ac ad ae)

instance Data.Binary.Binary HoIDeps where
    put (HoIDeps aa ab ac ad) = do
	    Data.Binary.put aa
	    Data.Binary.put ab
	    Data.Binary.put ac
	    Data.Binary.put ad
    get = do
    aa <- get
    ab <- get
    ac <- get
    ad <- get
    return (HoIDeps aa ab ac ad)

instance Data.Binary.Binary HoLib where
    put (HoLib aa ab ac ad) = do
	    Data.Binary.put aa
	    Data.Binary.put ab
	    Data.Binary.put ac
	    Data.Binary.put ad
    get = do
    aa <- get
    ab <- get
    ac <- get
    ad <- get
    return (HoLib aa ab ac ad)

instance Binary Data.Version.Version where
    put (Data.Version.Version a b) = put a >> put b
    get = liftM2 Data.Version.Version get get

instance Data.Binary.Binary HoTcInfo where
    put (HoTcInfo aa ab ac ad ae af ag ah) = do
	    Data.Binary.put aa
	    putMap ab
	    putMap ac
	    Data.Binary.put ad
	    Data.Binary.put ae
	    Data.Binary.put af
	    Data.Binary.put ag
	    Data.Binary.put ah
    get = do
    aa <- get
    ab <- getMap
    ac <- getMap
    ad <- get
    ae <- get
    af <- get
    ag <- get
    ah <- get
    return (HoTcInfo aa ab ac ad ae af ag ah)

instance Data.Binary.Binary HoBuild where
    put (HoBuild aa ab ac) = do
	    Data.Binary.put aa
	    Data.Binary.put ab
	    Data.Binary.put ac
    get = do
    aa <- get
    ab <- get
    ac <- get
    return (HoBuild aa ab ac)
