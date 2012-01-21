module C.FFI(
    CallConv(..),
    Safety(..),
    FfiType(..),
    FfiExport(..),
    FfiSpec(..),
    Requires(..)
    ) where

import C.Prims
import Data.Binary
import Data.Typeable

type CName    = String

data CallConv = CCall | StdCall | Primitive | DotNet deriving(Eq,Ord,Show)
    {-! derive: Binary !-}

data Safety = Safe | Unsafe deriving(Eq,Ord,Show)
    {-! derive: Binary !-}

data FfiType = Import CName Requires
             | ImportAddr CName Requires
             | Wrapper
             | Dynamic
             deriving(Eq,Ord,Show)

data FfiSpec = FfiSpec FfiType Safety CallConv
             deriving(Eq,Ord,Show)

data FfiExport = FfiExport {
    ffiExportCName :: CName,
    ffiExportSafety :: Safety,
    ffiExportCallConv :: CallConv,
    ffiExportArgTypes ::[ExtType],
    ffiExportRetType  :: ExtType
    }
 deriving(Eq,Ord,Show,Typeable)
     {-! derive: Binary !-}
