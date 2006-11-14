

import System


main :: IO ()
main = do
    as <- getArgs
    mapM_ putStrLn as
