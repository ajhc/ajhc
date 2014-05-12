{-# OPTIONS_JHC -fffi #-}
module System.IO(
    module System.IO.Error,
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
    hGetChar,
    hGetLine,
    hIsOpen,
    hIsClosed,
    hPrint,
    hPutBuf,
    hPutChar,
    hPutStr,
    hPutStrLn,
    hIsEOF,
    isEOF,
    hWaitForInput,
    openFile,
    openBinaryFile,
    withFile,
    fixIO,
    HandlePosn,
    stdin,stdout,stderr,
    hIsReadable,
    hIsSeekable,
    hIsWritable,
    hLookAhead,
    hReady,
    hSetBuffering,
    hGetBuffering
    ) where

import Foreign.Ptr
import Jhc.Basics
import Jhc.Handle
import Jhc.IO
import Jhc.Num
import Jhc.Type.C
import System.C.Stdio

data BufferMode = NoBuffering | LineBuffering | BlockBuffering (Maybe Int)
    deriving(Eq, Ord, Read, Show)
data SeekMode = AbsoluteSeek | RelativeSeek | SeekFromEnd
    deriving(Eq,Ord,Bounded,Enum,Read,Show)

type HandlePosn = Integer

hIsReadable h = return $ handleIOMode h `elem` [ReadMode,ReadWriteMode]
hIsWritable h = return $ handleIOMode h `elem` [AppendMode,WriteMode,ReadWriteMode]

withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile fp iom action = do
    h <- openFile fp iom
    r <- action h
    hClose h
    return r

hIsClosed h = not `fmap` hIsOpen h

hFlush :: Handle -> IO ()
hFlush h = withHandle h c_fflush

isEOF :: IO Bool
isEOF = hIsEOF stdin

hIsEOF :: Handle -> IO Bool
hIsEOF h = withHandle h $ \ptr -> do
    r <- c_feof ptr
    return (r /= 0)

hWaitForInput :: Handle -> Int -> IO Bool
hWaitForInput h to = withHandle h $ \ptr -> c_wait_for_input ptr to

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
hPrint h x  =  hPutStrLn h (show x)

hGetLine    :: Handle -> IO String
hGetLine h  =  do c <- hGetChar h
                  if c == '\n' then return "" else
                    do s <- hGetLine h
                       return (c:s)

hGetChar :: Handle -> IO Char
hGetChar h = withHandle h $ \ptr -> do
    ch <- c_fgetwc ptr
    case ch of
        -1 -> fail "hGetChar: EOF"
        _  -> return (unsafeChr ch)

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
    rc <- withHandle h $ c_fwrite p 1 count
    if rc /= count then fail "hPutBuf: short write" else return ()

hGetBuf :: Handle -> Ptr a -> Int -> IO Int
hGetBuf h p c = do
    let count = fromIntegral c
    rc <- withHandle h $ c_fread p 1 count
    return $ fromIntegral rc

hIsSeekable :: Handle -> IO Bool
hIsSeekable _ = return True

hLookAhead :: Handle -> IO Char
hLookAhead = error "hLookAhead"

hReady :: Handle -> IO Bool
hReady _ = return True

hSetBuffering :: Handle -> BufferMode -> IO ()
hSetBuffering _ _ = return ()

hGetBuffering :: Handle -> IO BufferMode
hGetBuffering _ = error "hGetBuffering"

hFileSize :: Handle -> IO Integer
hFileSize h = do
    cp <- hTell h
    hSeek h SeekFromEnd 0
    fl <- hTell h
    hSeek h AbsoluteSeek cp
    return fl

foreign import primitive "I2I" cwintToChar :: CWint -> Char
foreign import ccall "jhc_wait_for_input" c_wait_for_input :: FILE -> Int -> IO Bool
