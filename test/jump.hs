import System.IO.Continuation


go :: IOCont s String -> IO ()
go cont = do
    putStrLn "hello"
    --newContinuation (go' cont) (const undefined)
    callContinuation cont "there"
    putStrLn "you"


main = do
    newContinuation go putStrLn
