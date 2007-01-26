module C.FFI
    (CallConv(..), Safety(..), FfiType(..), FfiExport(..), FfiSpec(..), Requires(..), nullRequires
    ) where

import Data.Binary
import Data.Generics
import Data.Monoid

type CName    = String

data CallConv = CCall | StdCall | Primitive | DotNet deriving(Eq,Ord,Show,Data,Typeable)
    {-! derive: Binary !-}

data Safety = Safe | Unsafe deriving(Eq,Ord,Show,Data,Typeable)
    {-! derive: Binary !-}

data FfiType = Import CName Requires
             | ImportAddr CName Requires
             | Wrapper
             | Dynamic
             deriving(Eq,Ord,Show,Data,Typeable)
             {-! derive: Binary !-}

data Requires = Requires {
    reqIncludes :: [String],
    reqLibraries :: [String]
    } deriving(Typeable, Data, Eq, Ord)
    {-! derive: Monoid, Binary !-}

instance Show Requires where
    show (Requires [] []) = "()"
    show (Requires xs ys) = show (xs,ys)

nullRequires = Requires [] []

data FfiSpec = FfiSpec FfiType Safety CallConv
             deriving(Eq,Ord,Show,Data,Typeable)
             {-! derive: Binary !-}

data FfiExport = FfiExport CName Safety CallConv
             deriving(Eq,Ord,Show,Data,Typeable)
             {-! derive: Binary !-}

