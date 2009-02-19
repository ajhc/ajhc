{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE CPP  #-}
module CharIO(
    putStr,
    putStrLn,
    hPutStrLn,
    putErr,
    putErrLn,
    putErrDie,
    CharIO.readFile,
    CharIO.print,
    CharIO.hGetContents,
    runMain
    ) where

import Char
import Control.Exception
import Prelude hiding(putStr, putStrLn)
import System
import Support.Compat
import UTF8
import qualified IO
import qualified Prelude (putStr, putStrLn)

toUTF8 s = (map (chr. fromIntegral) $ toUTF s)
fromUTF8 s = fromUTF (map (fromIntegral . ord) s)


flushOut = Control.Exception.catch  (IO.hFlush IO.stdout) (\(e::SomeException) -> return ())

putStr = Prelude.putStr . toUTF8
putStrLn = Prelude.putStrLn . toUTF8
putErr s = flushOut >> IO.hPutStr IO.stderr (toUTF8 s)
putErrLn s = flushOut >> hPutStrLn IO.stderr s
putErrDie s = flushOut >> hPutStrLn IO.stderr s >> System.exitFailure
print x = putStrLn $ show x


hPutStrLn fh = IO.hPutStrLn fh . toUTF8

readFile fn = Prelude.readFile fn >>= \s -> return (fromUTF8 s)
hGetContents h =  IO.hGetContents h >>= \s -> return (fromUTF8 s)

runMain :: IO a -> IO ()
#if __GLASGOW_HASKELL__ < 610
runMain action = Control.Exception.catch (action >> return ()) $ \x -> case x of
        ExitException _ -> throw x
        _ -> putErrDie $ show x
#else
runMain action = Control.Exception.catches (action >> return ())
                   [ Handler $ \ (e::ExitCode) -> throw e
                   , Handler $ \ (e::SomeException) -> putErrDie $ show e ]
#endif
