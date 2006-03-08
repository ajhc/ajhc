module Foreign.Storable where

import Foreign.Ptr

class Storable a where
    sizeOf :: a -> Int
    alignment :: a -> Int
    peekElemOff :: Ptr a -> Int -> IO a
    pokeElemOff :: Ptr a -> Int -> a -> IO ()
    peekByteOff :: Ptr b -> Int -> IO a
    pokeByteOff :: Ptr b -> Int -> a -> IO ()
    peek :: Ptr a -> IO a
    poke :: Ptr a -> a -> IO ()

    alignment x = sizeOf x
    peekElemOff addr idx = do
        peek (addr `plusPtr` (idx * sizeOf (_f addr)))
    pokeElemOff addr idx x = poke (addr `plusPtr` (idx * sizeOf x)) x
    peekByteOff addr off = peek (addr `plusPtr` off)
    pokeByteOff addr off x = poke (addr `plusPtr` off) x

_f :: Ptr a -> a
_f _ = undefined
