module Jhc.Type.Handle where

-- CI import Jhc.Prim
import Jhc.Basics
import Jhc.Type.Ptr
-- CI import Jhc.Type.Basic
import Jhc.Type.C

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
