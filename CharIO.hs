module CharIO(
    putStr,
    putStrLn,
    putErr,
    putErrLn,
    putErrDie,
    CharIO.readFile,
    CharIO.print,
    CharIO.hGetContents,
    runMain
    ) where

import Prelude hiding(putStr, putStrLn)
import qualified Prelude (putStr, putStrLn)
import IO hiding(putStr, putStrLn)
import Control.Exception
import UTF8
import System
import Char

toUTF8 s = (map (chr. fromIntegral) $ toUTF s)
fromUTF8 s = fromUTF (map (fromIntegral . ord) s)

flushOut = Control.Exception.catch  (hFlush stdout) (\_ -> return ())

putStr = Prelude.putStr . toUTF8
putStrLn = Prelude.putStrLn . toUTF8
putErr s = flushOut >> IO.hPutStr IO.stderr (toUTF8 s)
putErrLn s = flushOut >> IO.hPutStrLn IO.stderr (toUTF8 s)
putErrDie s = flushOut >> IO.hPutStrLn IO.stderr (toUTF8 s) >> System.exitFailure
print x = putStrLn $ show x

readFile fn = Prelude.readFile fn >>= \s -> return (fromUTF8 s)
hGetContents h =  IO.hGetContents h >>= \s -> return (fromUTF8 s)

runMain :: IO a -> IO ()
runMain action = Control.Exception.catch (action >> return ()) $ \x -> case x of
        ExitException _ -> throw x
        _ -> putErrDie $ show x
