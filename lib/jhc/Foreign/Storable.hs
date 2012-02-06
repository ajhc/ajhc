{-# OPTIONS_JHC -fno-prelude -fm4 -funboxed-values -funboxed-tuples -fffi #-}
module Foreign.Storable(Storable(..)) where

import Jhc.Addr
import Jhc.Basics
import Jhc.IO
import Jhc.Int

m4_include(Foreign/Storable.m4)

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
    peekElemOff addr idx = fromUIO $ \w -> unIO (peek $! (addr `plusPtr` (idx `times` sizeOf (_f addr)))) w
    pokeElemOff addr idx x = fromUIO $ \w -> unIO (let adr = (addr `plusPtr` (idx `times` sizeOf x)) in adr `seq` poke adr x) w
    peekByteOff addr off = fromUIO $ \w -> unIO (peek $! (castPtr $ addr `plusPtr` off)) w
    pokeByteOff addr off x = fromUIO $ \w -> unIO (let adr = castPtr (addr `plusPtr` off) in adr `seq` poke adr x) w

_f :: Ptr a -> a
_f _ = undefined

INST_STORABLE_X((Ptr a),Ptr,Addr_,bits<ptr>)
INST_STORABLE_X((FunPtr a),FunPtr,FunAddr_,bits<ptr>)

-- foreign import "Add" plusBitsPtr_ :: BitsPtr_ -> Int -> BitsPtr_
