module Jhc.JumpPoint(IOCont(),newContinuation,callContinuation) where


--import Data.IORef
import Foreign.Ptr
import Foreign.Marshal.Alloc

type IORef a = a
writeIORef _ _ = return ()
newIORef a = return a
readIORef a = return a

data IOCont s a = IOCont !(IORef a) !JumpPoint
newtype JumpPoint = JumpPoint (Ptr JumpPoint)

newContinuation :: (forall s . IOCont s a -> IO b) -> (a -> IO b) -> IO b
newContinuation act cc = do
    jp@(JumpPoint jp') <- newJumpPoint__
    ref <- newIORef (error "shnizzle")
    r <- runJumpPoint__ jp
    case r of
        False -> do
            res <- act (IOCont ref jp)
            free jp'
            return res
        True -> do
            free jp'
            arg <- readIORef ref
            cc arg


callContinuation :: IOCont s a -> a -> IO b
callContinuation (IOCont ref jp) x = do
    writeIORef ref x
    jumpJumpPoint__ jp
    return $ error "callContinuation: end of the line"




newJumpPoint__ :: IO JumpPoint
newJumpPoint__ = do
    p <- mallocBytes jmp_buf_size
    return (JumpPoint p)


foreign import ccall jhc_setjmp :: JumpPoint -> IO Int
foreign import ccall jhc_longjmp :: JumpPoint -> IO ()
foreign import primitive "const.sizeof(jmp_buf)" jmp_buf_size  :: Int

runJumpPoint__ :: JumpPoint -> IO Bool
runJumpPoint__ jp = do
    r <- jhc_setjmp  jp
    return (r /= 0)

jumpJumpPoint__ :: JumpPoint -> IO ()
jumpJumpPoint__ jp = jhc_longjmp  jp




