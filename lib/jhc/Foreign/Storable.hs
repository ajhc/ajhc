{-# OPTIONS_JHC -N #-}
module Foreign.Storable(Storable(..)) where

import Jhc.Basics
import Jhc.Addr
import Jhc.Int
import Jhc.IO

plusPtr :: Ptr a -> Int -> Ptr b
plusPtr (Ptr addr) off = Ptr (plusAddr addr off)

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
    peekElemOff addr idx = IO $ \w -> unIO (peek $! (addr `plusPtr` (idx `times` sizeOf (_f addr)))) w
    pokeElemOff addr idx x = IO $ \w -> unIO (let adr = (addr `plusPtr` (idx `times` sizeOf x)) in adr `seq` poke adr x) w
    peekByteOff addr off = IO $ \w -> unIO (peek $! (addr `plusPtr` off)) w
    pokeByteOff addr off x = IO $ \w -> unIO (let adr = (addr `plusPtr` off) in adr `seq` poke adr x) w

_f :: Ptr a -> a
_f _ = undefined


