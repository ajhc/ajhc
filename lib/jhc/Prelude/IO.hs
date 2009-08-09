{-# OPTIONS_JHC -N -fffi -funboxed-values #-}
module Prelude.IO(
    IO(),
    ioError,
    catch,
    runExpr,
    FilePath(),
    putStr,
    putStrLn,
    print,
    getLine,
    getContents,
    readFile,
    interact,
    writeFile,
    appendFile,
    putChar,
    runExpr,
    getChar,
    userError
    ) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Jhc.Addr
import Jhc.Basics
import Jhc.IO
import Jhc.Monad
import Jhc.Order
import Jhc.Show
import Prelude.IOError


-- IO operations exported by the prelude

type  FilePath = String



{-# RULES "putStr/++"      forall xs ys . putStr (xs ++ ys) = putStr xs >> putStr ys #-}

putStr     :: String -> IO ()
putStr s   =  mapM_ putChar s

putStrLn   :: String -> IO ()
putStrLn s =  do putStr s
                 putChar '\n'

print      :: Show a => a -> IO ()
print x    =  putStrLn (show x)


getLine    :: IO String
getLine    =  do c <- getChar
                 if c == '\n' then return "" else
                    do s <- getLine
                       return (c:s)

getContents :: IO String
getContents = unsafeInterleaveIO getContents' where
    getContents' = do
        ch <- c_getwchar
        if ch == -1 then return [] else  do
            xs <- unsafeInterleaveIO getContents'
            return (unsafeChr ch:xs)


readFile :: FilePath -> IO String
readFile fn = do
    file <- withCString fn $ \fnc -> c_fopen fnc (ptrFromAddr__ "r"#)
    if  (file == nullPtr) then (fail "Could not open file.") else do
        let gc = do
                ch <- c_fgetwc file
                if ch == -1 then c_fclose file >> return [] else do
                        xs <- unsafeInterleaveIO gc
                        return (unsafeChr ch:xs)
        unsafeInterleaveIO gc


foreign import ccall "stdio.h fopen" c_fopen :: CString -> CString -> IO (Ptr ())
foreign import ccall "stdio.h fclose" c_fclose :: Ptr () -> IO CInt
foreign import ccall "wchar.h jhc_utf8_getc" c_fgetwc :: Ptr () -> IO Int

-- | The 'interact' function takes a function of type @String->String@
-- as its argument.  The entire input from the standard input device is
-- passed to this function as its argument, and the resulting string is
-- output on the standard output device.

interact        ::  (String -> String) -> IO ()
interact f      =   do s <- getContents
                       putStr (f s)
{-
interact    ::  (String -> String) -> IO ()
-- The hSetBuffering ensures the expected interactive behaviour
interact f  =  do hSetBuffering stdin  NoBuffering
                  hSetBuffering stdout NoBuffering
                  s <- getContents
                  putStr (f s)

-}


writeFile  :: FilePath -> String -> IO ()
writeFile  =  error "writeFile"

appendFile :: FilePath -> String -> IO ()
appendFile =  error "appendFile"



putChar :: Char -> IO ()
putChar c = c_putwchar (ord c)

-- | this is wrapped around arbitrary showable expressions when used as the main entry point
runExpr :: Show a => a -> World__ -> World__
runExpr x w = runNoWrapper (print x) w

--TODO EOF == -1
getChar :: IO Char
getChar = do
    ch <- c_getwchar
    if ch == -1 then fail "End of file." else return (unsafeChr ch)

foreign import primitive "I2I" cwintToChar :: CWint -> Char
foreign import primitive "U2U" charToCWchar :: Char -> CWchar

foreign import ccall "stdio.h jhc_utf8_putchar" c_putwchar :: Int -> IO ()
foreign import ccall "wchar.h jhc_utf8_getchar" c_getwchar :: IO Int


