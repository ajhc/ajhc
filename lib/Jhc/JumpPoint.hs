module Jhc.JumpPoint(IOCont(),newContinuation,callContinuation,newContinuation__) where


import Jhc.IO
import Jhc.Hole
import Foreign.Ptr
import Foreign.Marshal.Alloc


data IOCont s a = IOCont (Hole a) JumpPoint
newtype JumpPoint = JumpPoint (Ptr JumpPoint)

newContinuation :: (forall s . IOCont s a -> IO b) -> (a -> IO b) -> IO b
newContinuation act cc = newContinuation__ act cc


-- | this is unsafe and should only be used internally to the IO library
newContinuation__ :: (IOCont World__ a -> IO b) -> (a -> IO b) -> IO b
newContinuation__ act cc = do
    jp@(JumpPoint jp') <- newJumpPoint__
    ref <- newHole
    r <- runJumpPoint__ jp
    case r of
        False -> do
            res <- act (IOCont ref jp)
            free jp'
            return res
        True -> do
            free jp'
            cc (readHole ref)


callContinuation :: IOCont s a -> a -> IO b
callContinuation (IOCont ref jp) x = do
    fillHole ref x
    jumpJumpPoint__ jp




newJumpPoint__ :: IO JumpPoint
newJumpPoint__ = do
    p <- _malloc jmp_buf_size
    return (JumpPoint p)


foreign import ccall jhc_setjmp :: JumpPoint -> IO Int
foreign import ccall jhc_longjmp :: JumpPoint -> IO ()
foreign import primitive "const.sizeof(jmp_buf)" jmp_buf_size  :: Int

runJumpPoint__ :: JumpPoint -> IO Bool
runJumpPoint__ jp = do
    r <- jhc_setjmp  jp
    return (r /= 0)

jumpJumpPoint__ :: JumpPoint -> IO a
jumpJumpPoint__ jp = jhc_longjmp  jp >> return (error "jumpJumpPoint__")

foreign import ccall "stdlib.h malloc" _malloc :: Int -> IO (Ptr a)



