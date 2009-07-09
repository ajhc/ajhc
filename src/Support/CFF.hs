{-# OPTIONS -funbox-strict-fields  -O2 #-}

-- chunked file format.
-- A generalization of the PNG format for user defined file formats.

module Support.CFF(
    ChunkType(),
    FileType(),
    FileOffset(),
    ChunkLength(),
    chunkType,
    isCritical,
    isPrivate,
    isSafeToCopy,
    readCFFHeader,
    readCFF,
    bsCFF,
    lbsCFF,
    mkCFFfile,
    readChunk,
    lazyWriteCFF,
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
import qualified Data.Map as Map


type FileOffset = Word
type ChunkLength = Word

-- the file's magic number is as follows:
--
-- 0x89      - high bit set, to check for 8 bit transmission errors and to avoid being treated as a text file
-- 3 bytes   - identifies the particular file format. i.e. 'PNG' or 'JHC'
-- 0x0D 0x0A - DOS style line ending, to detect errors.
-- 0x1A      - EOF marker, to avoid corrupting the screen when typed under dos/windows
-- 0x0A      - unix EOL marker, to detect line conversion errors

-----------------------------------
-- Routines dealing with ChunkTypes
-----------------------------------

type FileType = ChunkType
newtype ChunkType = ChunkType Word32
    deriving(Eq,Ord)

instance Show ChunkType where
    showsPrec _ (ChunkType w) xs = b 3:b 2:b 1:b 0:xs where
        b n = chr $ fromIntegral ((w `shiftR` (8 * n)) .&. 0xFF)

instance Read ChunkType where
    readsPrec _ (b1:b2:b3:b4:xs) = [(chunkType [b1,b2,b3,b4],xs)]
    readsPrec _ _ = []

chunkType :: String -> ChunkType
chunkType [b1,b2,b3,b4] = bytesToChunkType (fi b1) (fi b2) (fi b3) (fi b4) where
    fi = fromIntegral . ord
chunkType [b1,b2,b3] = chunkType [b1,b2,b3,' ']
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


lbsCFF :: Monad m => LBS.ByteString -> m (FileType,[(ChunkType,LBS.ByteString)])
lbsCFF bs = ans bs where
    ans bs' = do
        let checkByte n b = do
                unless ((bs `LBS.index` n) == b) $ fail "bsCFF: invalid chunked file"
            bs = LBS.take 8 bs'
        when (LBS.length bs < 8) $ fail "bsCFF: chunked file is too short"
        checkByte 0 0x89
        checkByte 4 0x0d
        let b1 = bs `LBS.index` 1
            b2 = bs `LBS.index` 2
            b3 = bs `LBS.index` 3
        checkByte 5 0x0a
        checkByte 6 0x1a
        checkByte 7 0x0a
        let header =  bytesToChunkType b1 b2 b3 (fromIntegral $ ord ' ')
        return (header,readRest (LBS.drop 8 bs))

    bsWord32 :: LBS.ByteString -> Word32
    bsWord32 bs = w where
        b1 = bs `LBS.index` 0
        b2 = bs `LBS.index` 1
        b3 = bs `LBS.index` 2
        b4 = bs `LBS.index` 3
        ChunkType w = bytesToChunkType b1 b2 b3 b4

    readRest bs = f bs where
        f bs | LBS.null bs = []
        f bs = (ct,bdata):f (LBS.drop 4 brest) where
            len = bsWord32 bs
            ct = ChunkType $ bsWord32 (LBS.drop 4 bs)
            (bdata,brest)  = LBS.splitAt (fromIntegral len) (LBS.drop 8 bs)


bsCFF :: Monad m => BS.ByteString -> m (FileType,[(ChunkType,BS.ByteString)])
bsCFF bs = ans bs where
    ans bs = do
        let checkByte n b = do
                unless ((bs `BS.index` n) == b) $ fail "bsCFF: invalid chunked file"
        when (BS.length bs < 8) $ fail "bsCFF: chunked file is too short"
        checkByte 0 0x89
        checkByte 4 0x0d
        let b1 = bs `BS.index` 1
            b2 = bs `BS.index` 2
            b3 = bs `BS.index` 3
        checkByte 5 0x0a
        checkByte 6 0x1a
        checkByte 7 0x0a
        let header =  bytesToChunkType b1 b2 b3 (fromIntegral $ ord ' ')
        return (header,readRest (BS.drop 8 bs))

    bsWord32 :: BS.ByteString -> Word32
    bsWord32 bs = w where
        b1 = bs `BS.index` 0
        b2 = bs `BS.index` 1
        b3 = bs `BS.index` 2
        b4 = bs `BS.index` 3
        ChunkType w = bytesToChunkType b1 b2 b3 b4

    readRest bs = f bs where
        f bs | BS.null bs = []
        f bs = (ct,bdata):f (BS.drop 4 brest) where
            len = bsWord32 bs
            ct = ChunkType $ bsWord32 (BS.drop 4 bs)
            (bdata,brest)  = BS.splitAt (fromIntegral len) (BS.drop 8 bs)

mkCFFHeader :: FileType -> BS.ByteString
mkCFFHeader (ChunkType ft) = BS.pack [0x89,b1,b2,b3,0x0d,0x0a,0x1a,0x0a] where
    (b1,b2,b3,_) = word32ToBytes ft

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

writeCFFHeader :: Handle -> FileType -> IO ()
writeCFFHeader h ft = BS.hPut h (mkCFFHeader ft)


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
            if ct == ect then do BS.hGet h (fromIntegral len) else do
                hSeek h RelativeSeek (fromIntegral len + 4)
                readChunk
    readChunk


mkCFFfile :: FileType -> [(ChunkType,LBS.ByteString)] -> LBS.ByteString
mkCFFfile ft cs = LBS.fromChunks [mkCFFHeader ft] `LBS.append` LBS.concat (concatMap f cs) where
    f (ChunkType ct,bs) = [hl,bs,zero]  where
        (b1,b2,b3,b4) = word32ToBytes ct
        (l1,l2,l3,l4) = word32ToBytes (fromIntegral $ LBS.length bs)
        hl = LBS.pack [l1,l2,l3,l4,b1,b2,b3,b4]
zero :: LBS.ByteString
zero = LBS.pack [0,0,0,0]

writeCFF :: Handle -> ChunkType -> [(ChunkType,BS.ByteString)] -> IO ()
writeCFF h ft xs = do
    writeCFFHeader h ft
    let writeChunk (ChunkType ct,bs) = do
            writeWord32 h (fromIntegral $ BS.length bs)
            writeWord32 h ct
            BS.hPut h bs
            writeWord32 h 0 -- TODO proper checksum
    mapM_ writeChunk xs

lazyWriteCFF :: Handle -> ChunkType -> [(ChunkType,LBS.ByteString)] -> IO ()
lazyWriteCFF h ft xs = do
    writeCFFHeader h ft
    let writeChunk (ChunkType ct,bs) = do
            writeWord32 h (fromIntegral $ LBS.length bs)
            writeWord32 h ct
            LBS.hPut h bs
            writeWord32 h 0 -- TODO proper checksum
    mapM_ writeChunk xs



-------------------------------------------------
-- Various routines for reading and writing bytes
-------------------------------------------------

getByte :: Handle -> IO Word8
getByte h = liftM (fromIntegral . ord) (hGetChar h)

writeByte :: Handle -> Word8 -> IO ()
writeByte h b = hPutChar h (chr $ fromIntegral b)

bytesToChunkType :: Word8 -> Word8 -> Word8 -> Word8 -> ChunkType
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

