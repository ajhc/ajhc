
import System

import Util.SHA1

main = do
    as <- getArgs
    xs <- mapM sha1file as
    mapM_ print xs
