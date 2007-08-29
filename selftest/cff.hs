

import System.IO
import Support.CFF
import qualified Data.ByteString as BS
import System

main = do
    xs <- getArgs
    flip mapM xs $ \fn -> do
        h <- openBinaryFile fn ReadMode
        (ct,cfs) <- readCFF h
        hClose h
        putStrLn $ fn ++ " - " ++ show ct
        flip mapM cfs $ \ (ct,ds) -> do
            putStrLn $ show ct ++ ": " ++ show (BS.length ds)


