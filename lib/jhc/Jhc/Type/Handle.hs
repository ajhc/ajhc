module Jhc.Type.Handle where

import Jhc.Prim
import Jhc.Basics
import Jhc.Type.Ptr
import Jhc.Type.Basic

data CFile

data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

data Handle = Handle {
    handleName :: [Char],
    handleFile :: !(Ptr (Ptr CFile)),
    handleBinary :: !Bool,
    handleIsPipe :: !Bool,
    handleIOMode :: !IOMode
    }

data IOErrorType
    = AlreadyExists
    | DoesNotExist
    | AlreadyInUse
    | Full
    | EOF
    | IllegalOperation
    | Permission
    | User

data IOError = IOError {
    ioeGetErrorType :: !IOErrorType,
    ioeGetErrorString :: String,
    ioeGetHandle :: Maybe Handle,
    ioeGetFileName :: Maybe String
    }
