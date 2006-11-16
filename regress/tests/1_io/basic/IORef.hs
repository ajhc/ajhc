


import Data.IORef

fact :: Int -> IO Int
fact n = do
    ref <- newIORef 1
    let f 1 = return ()
        f n = modifyIORef ref (n*) >> f (n - 1)
    f n
    readIORef ref


main = do
    r <- fact 5
    putStrLn (replicate r 'x')

