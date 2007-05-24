{-# OPTIONS_JHC -fffi #-}
module System.IO(
    Handle,
    IOMode(..),
    BufferMode(..),
    SeekMode(..),
    hPutChar,
    hPutStr,
    hPutStrLn,
    hPrint,
    try,hFlush,stdin,stdout,stderr,
    hGetContents,
    hClose,
    openFile,
    hIsOpen

    ) where
{-
module IO(
    Handle,
    IOMode(..),
    BufferMode(..),
    SeekMode(..),
    stdin,
    stdout,
    stderr
    )  where
    -}


import Jhc.IO
import Jhc.Handle
import Prelude.IOError
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Data.Char(ord)


data BufferMode = NoBuffering | LineBuffering | BlockBuffering (Maybe Int)
    deriving(Eq, Ord, Read, Show)
data SeekMode = AbsoluteSeek | RelativeSeek | SeekFromEnd
    deriving(Eq,Ord,Bounded,Enum,Read,Show)



try            :: IO a -> IO (Either IOError a)
try f          =  catch (do r <- f
                            return (Right r))
                        (return . Left)





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
                    return (cwintToChar ch:xs)
    unsafeInterleaveIO getContents'
--hIsEOF :: Handle -> IO Bool

foreign import primitive "I2I" cwintToChar :: CWint -> Char

foreign import ccall "stdio.h fflush" c_fflush :: Ptr Handle -> IO ()

foreign import ccall "wchar.h getwc" c_fgetwc :: Ptr Handle -> IO CWint
foreign import ccall "wchar.h putwc" c_fputwc :: CWchar -> Ptr Handle -> IO CWint

foreign import ccall "stdio.h feof" c_feof :: Ptr Handle -> IO CInt

