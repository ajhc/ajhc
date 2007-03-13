{-# OPTIONS_GHC -cpp -fbang-patterns #-}
module C.Arch(determineArch,primitiveInfo,genericPrimitiveInfo,isFGrin) where



{-
    architecture specification consists of a string of form

    backend-cpu-flags

    where any of the fields may be omitted.

    valid backends are currently "ghc" and "grin", valid cpus are listed in arch/ and 'generic', and flags consist of 32 and 64
    for instance

    grin-i686

    there are only 2 ghc versions of the backend, ghc-32 and ghc-64.

-}

import Char
import Data.List
import System.IO.Unsafe
import System.Info
import qualified Data.Map as Map

import C.Prims
import Options
import Util.Gen
import qualified FlagOpts as FO

#include "../arch/generic.arch"
#include "../arch/i686.arch"
#include "../arch/x86_64.arch"

#include "MachDeps.h"

cpu_alias s = maybe arch_error id $ lookup s' $ [
    ("unknown","generic"),
    ("amd64","x86_64"),
    ("i386","i686"),
    ("i486","i686"),
    ("i586","i686")
    ] ++ [ (n,n) | n <- archs ] where s' = map toLower s

archs = ["generic","i686","x86_64"]

arch_map = [
    ("generic",Nothing,arch_generic,[]),
    ("i686",Nothing,arch_i686,[]),
    ("x86_64",Just 64,arch_x86_64,[]),
    ("x86_64",Just 32,arch_i686,["-m32"])
    ]

available_archs = snub $ "ghc":"ghc-64":"ghc-32":[ n | (n,_,_,_) <- arch_map ]  ++ [ n ++ "-" ++ show b |  (n,Just b,_,_) <- arch_map]

primitiveInfo :: Monad m => ExtType -> m PrimType
primitiveInfo et = Map.lookup et primMap

genericPrimitiveInfo :: Monad m => ExtType -> m PrimType
genericPrimitiveInfo et = Map.lookup et primMap


primMap :: Map.Map ExtType PrimType
primMap = Map.fromList [ (primTypeName a,a) | a <- as ] where
    (_,_,as,_) = unsafePerformIO determineArch

isFGrin :: Bool
isFGrin = case optArch options of
    Nothing -> True
    Just o -> "grin" `isPrefixOf` o

determineArch = do
    let specs = maybe [] (split (== '-')) (optArch options)
        (backendGhc,specs') | ("ghc":rs) <- specs = (True,rs)
                            | ("grin":rs) <- specs = (False,rs)
                            | ("fgrin":rs) <- specs = (False,rs)
                            | otherwise = (fopts FO.ViaGhc,specs)
        (cpu,bits) = case specs' of
            ["32"] -> (cpu_alias arch,32)
            ["64"] -> (cpu_alias arch,64)
            [cpu,"32"] -> (cpu_alias cpu,32)
            [cpu,"64"] -> (cpu_alias cpu,64)
            [cpu]      -> (cpu_alias cpu,WORD_SIZE_IN_BITS)
            []         -> (cpu_alias arch,WORD_SIZE_IN_BITS)
            _          -> arch_error
    let (fn,mp,opt) = case (backendGhc,cpu,bits) of
            (True,!_,32) -> ("ghc-" ++ show bits,arch_i686,[])
            (True,!_,64) -> ("ghc-" ++ show bits,arch_x86_64,[])
            (_,"generic",_) -> ("generic",arch_generic,[])
            (_,"i686",32)   -> ("i686",arch_i686,[])
            (_,"x86_64",32) -> ("x86_64-32",arch_i686, ["-m32"])
            (_,"x86_64",64) -> ("x86_64",arch_x86_64,[])
            _ -> arch_error

    return (backendGhc,fn,mp,opt)

arch_error =  error $ "\nunknown architecture, supported architectures are:\n" ++ show available_archs





