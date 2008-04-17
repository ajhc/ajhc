{-# OPTIONS_JHC -fffi #-}
module System.IO(
    BufferMode(..),
    Handle,
    IOMode(..),
    SeekMode(..),
    hClose,
    hFileSize,
    hSeek,
    hTell,
    hFlush,
    hGetBuf,
    hGetPosn,
    hSetPosn,
    hGetContents,
    hIsOpen,
    hPrint,
    hPutBuf,
    hPutChar,
    hPutStr,
    hPutStrLn,
    openFile,
    withFile,
    fixIO,
    stdin,stdout,stderr,
    try

    ) where

import Jhc.IO
import Jhc.Handle
import Prelude.IOError
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Data.Char(ord)
import Data.Int


data BufferMode = NoBuffering | LineBuffering | BlockBuffering (Maybe Int)
    deriving(Eq, Ord, Read, Show)
data SeekMode = AbsoluteSeek | RelativeSeek | SeekFromEnd
    deriving(Eq,Ord,Bounded,Enum,Read,Show)

type HandlePosn = Integer



try            :: IO a -> IO (Either IOError a)
try f          =  catch (do r <- f
                            return (Right r))
                        (return . Left)


withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile fp iom action = do
    h <- openFile fp iom
    r <- action h
    hClose h
    return r



hFlush :: Handle -> IO ()
hFlush h = withHandle h c_fflush

isEOF :: IO Bool
isEOF = hIsEOF stdin

hIsEOF :: Handle -> IO Bool
hIsEOF h = withHandle h $ \ptr -> do
    r <- c_feof ptr
    return (r /= 0)

hPutChar h ch = withHandle h $ \ptr -> do
    c_fputwc (fromInt (ord ch)) ptr
    return ()

hPutStr     :: Handle -> String -> IO ()
hPutStr h s   = withHandle h $ \ptr -> do
    sequence_ [ c_fputwc (fromInt (ord ch)) ptr | ch <- s ]

hPutStrLn   :: Handle -> String -> IO ()
hPutStrLn h s = do
    hPutStr h s
    hPutChar h '\n'

hPrint      :: Show a => Handle -> a -> IO ()
hPrint h x    =  hPutStrLn h (show x)


hGetContents :: Handle -> IO String
hGetContents h = withHandle h $ \ptr -> do
    let getContents' = do
            ch <- c_fgetwc ptr
            case ch of
                -1 -> return []
                _ -> do
                    xs <- unsafeInterleaveIO getContents'
                    return (unsafeChr ch:xs)
    unsafeInterleaveIO getContents'

hTell :: Handle -> IO Integer
hTell h = withHandle h $ \ptr -> fmap fromIntegral (c_ftell ptr)

hSeek :: Handle -> SeekMode -> Integer -> IO ()
hSeek h v w = withHandle h $ \ptr -> do
    let sm x = case x of
            AbsoluteSeek -> c_SEEK_SET
            RelativeSeek -> c_SEEK_CUR
            SeekFromEnd  -> c_SEEK_END
    c_fseek ptr (fromIntegral w) (sm v)
    return ()

hGetPosn :: Handle -> IO HandlePosn
hGetPosn h = hTell h

hSetPosn :: Handle -> HandlePosn -> IO ()
hSetPosn h hp = hSeek h AbsoluteSeek hp

hPutBuf :: Handle -> Ptr a -> Int -> IO ()
hPutBuf h p c = do
    let count = fromIntegral c
    rc <- withHandle h $ fwrite p 1 count
    if rc /= count then fail "hPutBuf: short write" else return ()

hGetBuf :: Handle -> Ptr a -> Int -> IO Int
hGetBuf h p c = do
    let count = fromIntegral c
    rc <- withHandle h $ fread p 1 count
    return $ fromIntegral rc

hFileSize :: Handle -> IO Integer
hFileSize h = do
    cp <- hTell h
    hSeek h SeekFromEnd 0
    fl <- hTell h
    hSeek h AbsoluteSeek cp
    return fl

foreign import ccall "stdio.h fwrite_unlocked" fwrite  :: Ptr a -> CSize -> CSize -> Ptr Handle -> IO CSize
foreign import ccall "stdio.h fread_unlocked" fread :: Ptr a -> CSize -> CSize -> Ptr Handle -> IO CSize

foreign import primitive "I2I" cwintToChar :: CWint -> Char

foreign import ccall "stdio.h fflush" c_fflush :: Ptr Handle -> IO ()

foreign import ccall "wchar.h jhc_utf8_getc" c_fgetwc :: Ptr Handle -> IO Int
foreign import ccall "wchar.h jhc_utf8_putc" c_fputwc :: Int -> Ptr Handle -> IO Int

foreign import ccall "stdio.h feof" c_feof :: Ptr Handle -> IO CInt
foreign import ccall "stdio.h ftell" c_ftell :: Ptr Handle -> IO IntMax                  -- XXX
foreign import ccall "stdio.h fseek" c_fseek :: Ptr Handle -> IntMax -> CInt -> IO CInt  -- XXX

foreign import primitive "const.SEEK_SET" c_SEEK_SET :: CInt
foreign import primitive "const.SEEK_CUR" c_SEEK_CUR :: CInt
foreign import primitive "const.SEEK_END" c_SEEK_END :: CInt

