

import System

import Support.MD5

main = do
    as <- getArgs
    xs <- mapM md5file as
    mapM_ print xs
    mapM_ (putStrLn . md5show32) xs
