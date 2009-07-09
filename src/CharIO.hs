{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE CPP  #-}
module CharIO(
    putStr,
    putStrLn,
    hPutStrLn,
    putErr,
    putErrLn,
    putErrDie,
    readFile,
    print,
    hGetContents,
    runMain
    ) where

import Control.Exception
import Prelude hiding(readFile, print, putStr, putStrLn)
import System
import Support.Compat
import qualified IO
import System.IO.UTF8 as U



flushOut = Control.Exception.catch  (IO.hFlush IO.stdout) (\(e::SomeException') -> return ())

putErr s = flushOut >> U.hPutStr IO.stderr s
putErrLn s = flushOut >> U.hPutStrLn IO.stderr s
putErrDie s = flushOut >> U.hPutStrLn IO.stderr s >> System.exitFailure



runMain :: IO a -> IO ()
#if __GLASGOW_HASKELL__ < 610
runMain action = Control.Exception.catch (action >> return ()) $ \x -> case x of
        ExitException _ -> throw x
        _ -> putErrDie $ show x
#else
runMain action = Control.Exception.catches (action >> return ())
                   [ Handler $ \ (e::ExitCode) -> throw e
                   , Handler $ \ (e::SomeException') -> putErrDie $ show e ]
#endif
