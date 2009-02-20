

import System

import Support.MD5

main = do
    as <- getArgs
    xs <- mapM md5file as
    mapM_ print xs
