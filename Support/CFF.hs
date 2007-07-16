
-- chunked file format.
-- A generalization of the PNG format for user defined file formats.

module Support.CFF(
    ChunkType(),
    FileOffset(),
    ChunkLength(),
    chunkType,
    isCritical,
    isPrivate,
    isSafeToCopy,
    readCFFHeader,
    readCFF,
    lazyReadCFF,
    lazyGetCFF,
    readChunk,
    writeCFF
    )where

import Control.Concurrent.MVar
import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.Word
import System
import System.IO
import System.IO.Unsafe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

newtype ChunkType = ChunkType Word32
    deriving(Eq,Ord)

type FileOffset = Word
type ChunkLength = Word

-- the file's magic number is as follows:
--
-- 0x89      - high bit set, to check for 8 bit transmission errors and to avoid being treated as a text file
-- 3 bytes   - identifies the particular file format. i.e. 'PNG' or 'JHC'
-- 0x0D 0x0A - DOS style line ending, to detect errors.
-- 0x1A      - EOF marker, to avoid corrupting the screen when typed under dos/windows
-- 0x0A      - unix EOL marker, to detect line conversion errors


instance Show ChunkType where
    showsPrec _ (ChunkType w) xs = b 3:b 2:b 1:b 0:xs where
        b n = chr $ fromIntegral ((w `shiftR` (8 * n)) .&. 0xFF)

instance Read ChunkType where
    readsPrec _ (b1:b2:b3:b4:xs) = [(chunkType [b1,b2,b3,b4],xs)]
    readsPrec _ _ = []

chunkType [b1,b2,b3,b4] = bytesToChunkType (fi b1) (fi b2) (fi b3) (fi b4) where
    fi = fromIntegral . ord
chunkType _ = error "chunkType: not a chunk."


-- critical if the first letter is capitalized
isCritical :: ChunkType -> Bool
isCritical (ChunkType w) =  w .&. 0x20000000 == 0

-- private if the second letter is capitalized
isPrivate :: ChunkType -> Bool
isPrivate  (ChunkType w) =  w .&. 0x00200000 == 0

-- chunk should be copied if unrecognized by an editor
isSafeToCopy :: ChunkType -> Bool
isSafeToCopy (ChunkType w) =  w .&. 0x00000020 == 0


readCFFHeader :: Handle -> IO ChunkType
readCFFHeader h = do
    let checkByte b = do
            z <- getByte h
            unless (z == b) $ fail "readCFFInfo: invalid chunked file"
    checkByte 0x89
    b1 <- getByte h
    b2 <- getByte h
    b3 <- getByte h
    checkByte 0x0d
    checkByte 0x0a
    checkByte 0x1a
    checkByte 0x0a
    return $ bytesToChunkType b1 b2 b3 (fromIntegral $ ord ' ')

writeCFFHeader :: Handle -> ChunkType -> IO ()
writeCFFHeader h (ChunkType ft) = do
    writeByte h 0x89
    let (b1,b2,b3,_) = word32ToBytes ft
    writeByte h b1
    writeByte h b2
    writeByte h b3
    writeByte h 0x0d
    writeByte h 0x0a
    writeByte h 0x1a
    writeByte h 0x0a


readCFFInfo :: Handle -> IO (ChunkType,[(ChunkType,FileOffset,ChunkLength)])
readCFFInfo h = do
    cffType <- readCFFHeader h
    let readChunk fo | fo `seq` True = do
            b <- hIsEOF h
            if b then return [] else do
            len <- readWord32 h
            ct <- readChunkType h
            hSeek h RelativeSeek (fromIntegral len)
            _csum  <- readWord32 h
            xs <- readChunk (fo + fromIntegral len + 12)
            return ((ct,fo + 8,fromIntegral len):xs)

    xs <- readChunk (8::FileOffset)
    return (cffType,xs)


readCFF :: Handle -> IO (ChunkType,[(ChunkType,BS.ByteString)])
readCFF h = do
    cffType <- readCFFHeader h
    let readChunk = do
            b <- hIsEOF h
            if b then return [] else do
            len <- readWord32 h
            ct <- readChunkType h
            bs <- BS.hGet h (fromIntegral len)
            _csum <- readWord32 h -- TODO verify checksum
            xs <- readChunk
            return ((ct,bs):xs)
    xs <- readChunk
    return (cffType,xs)

-- this verifies a cff is of a specific type, and reads a specific chunk only.
readChunk :: Handle -> ChunkType -> ChunkType -> IO BS.ByteString
readChunk h eft ect = do
    cffType <- readCFFHeader h
    when (cffType /= eft) $ fail "readChunk: CFF file of incorrect type"
    let readChunk = do
            b <- hIsEOF h
            if b then fail "readChunk: specified chunk was not found" else do
            len <- readWord32 h
            ct <- readChunkType h
            if ct == ect
            then do
                BS.hGet h (fromIntegral len)
            else do
                hSeek h RelativeSeek (fromIntegral len + 4)
                readChunk
    readChunk


writeCFF :: Handle -> (ChunkType,[(ChunkType,BS.ByteString)]) -> IO ()
writeCFF h (ft,xs) = do
    writeCFFHeader h ft
    let writeChunk (ChunkType ct,bs) = do
            writeWord32 h (fromIntegral $ BS.length bs)
            writeWord32 h ct
            BS.hPut h bs
            writeWord32 h 0 -- TODO proper checksum
    mapM_ writeChunk xs



lazyReadCFF :: Handle -> IO (ChunkType,Map.Map ChunkType [BS.ByteString])
lazyReadCFF h = do
    mv <- newMVar ()
    let getMap = do
            xs <- readChunk
            let xs' = sortBy (\ (x,y) (a,b) -> compare x a) xs
                xs'' = groupBy  (\ (x,y) (a,b) -> x == a) xs'
            return (Map.fromList xs'')
        readChunk = do
            b <- hIsEOF h
            if b then return [] else do
            len <- readWord32 h
            ct <- readChunkType h
            off <- hTell h
            bs <- unsafeInterleaveIO $ do
                takeMVar mv
                hSeek h AbsoluteSeek off
                BS.hGet h (fromIntegral len)
                putMVar mv ()
            hSeek h RelativeSeek 4
            xs <- readChunk
            return ((ct,bs):xs)

    cffType <- readCFFHeader h
    map <-  getMap
    return (cffType,map)


lazyGetCFF fn = do openBinaryFile fn ReadMode >>= lazyReadCFF


-------------------------------------------------
-- Various routines for reading and writing bytes
-------------------------------------------------

getByte :: Handle -> IO Word8
getByte h = liftM (fromIntegral . ord) (hGetChar h)

writeByte :: Handle -> Word8 -> IO ()
writeByte h b = hPutChar h (chr $ fromIntegral b)

bytesToChunkType b1 b2 b3 b4 = ChunkType $ bytesToWord32 b1 b2 b3 b4

word32ToBytes :: Word32 -> (Word8,Word8,Word8,Word8)
word32ToBytes w = (b 3,b 2,b 1,b 0) where
        b n = fromIntegral ((w `shiftR` (8 * n)) .&. 0xFF)

bytesToWord32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
bytesToWord32 b1 b2 b3 b4 = b 3 b1 .|. b 2 b2 .|. b 1 b3 .|. b 0 b4  where
    b n c = (fromIntegral c) `shiftL` (8 * n)

readChunkType :: Handle -> IO ChunkType
readChunkType h = do
    w <- readWord32 h
    return $ ChunkType w

readWord32 :: Handle -> IO Word32
readWord32 h = do
    b1 <- getByte h
    b2 <- getByte h
    b3 <- getByte h
    b4 <- getByte h
    let ChunkType ct = bytesToChunkType b1 b2 b3 b4
    return ct

writeWord32 :: Handle -> Word32 -> IO ()
writeWord32 h w = do
    let (b1,b2,b3,b4) = word32ToBytes w
    writeByte h b1
    writeByte h b2
    writeByte h b3
    writeByte h b4


--main = do
--    --xs <- getArgs
--    --mapM_ print ([ (x,isCritical x) | x <- map readChunk xs])
--    --mapM_ print (sort $ map readChunk xs)
--    xs <- getArgs
--    flip mapM xs $ \fn -> do
--        h <- openBinaryFile fn ReadMode
--        cf <- readCFF h
--        hClose h
--        print cf
--        nh <- openBinaryFile "out.cff" WriteMode
--        writeCFF nh cf
--        hClose nh
--    return ()
