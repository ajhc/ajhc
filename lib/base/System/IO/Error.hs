module System.IO.Error (
    IOError(),  userError,  mkIOError,  annotateIOError,  isAlreadyExistsError,
    isDoesNotExistError,  isAlreadyInUseError,  isFullError,  isEOFError,
    isIllegalOperation,  isPermissionError,  isUserError,  ioeGetErrorString,
    ioeGetHandle,  ioeGetFileName,  IOErrorType(),  alreadyExistsErrorType,
    doesNotExistErrorType,  alreadyInUseErrorType,  fullErrorType,
    eofErrorType,  illegalOperationErrorType,  permissionErrorType,
    userErrorType,  ioError,  catch,  try
  ) where

import Jhc.Type.Handle

-- | The construct 'try' @comp@ exposes IO errors which occur within a
-- computation, and which are not fully handled.
--
-- Non-I\/O exceptions are not caught by this variant; to catch all
-- exceptions, use 'Control.Exception.try' from "Control.Exception".

try            :: IO a -> IO (Either IOError a)
try f          =  catch (do r <- f
                            return (Right r))
                        (return . Left)

-- -----------------------------------------------------------------------------
-- Constructing an IOError

-- | Construct an 'IOError' of the given type where the second argument
-- describes the error location and the third and fourth argument
-- contain the file handle and file path of the file involved in the
-- error if applicable.
mkIOError :: IOErrorType -> String -> Maybe Handle -> Maybe FilePath -> IOError
mkIOError = IOError

-- TODO(john): fix
annotateIOError :: IOError -> String -> Maybe Handle -> Maybe FilePath -> IOError
annotateIOError ioe _s _mh _mfp = ioe

check :: IOErrorType -> IOError -> Bool
check errorType ioe = ioeGetErrorType ioe == errorType

-- | An error indicating that an 'IO' operation failed because
-- one of its arguments already exists.
isAlreadyExistsError :: IOError -> Bool
isAlreadyExistsError = check AlreadyExists

-- | An error indicating that an 'IO' operation failed because
-- one of its arguments does not exist.
isDoesNotExistError :: IOError -> Bool
isDoesNotExistError  = check DoesNotExist

-- | An error indicating that an 'IO' operation failed because
-- one of its arguments is a single-use resource, which is already
-- being used (for example, opening the same file twice for writing
-- might give this error).
isAlreadyInUseError :: IOError -> Bool
isAlreadyInUseError  = check AlreadyInUse

-- | An error indicating that an 'IO' operation failed because
-- the device is full.
isFullError         :: IOError -> Bool
isFullError          = check Full

-- | An error indicating that an 'IO' operation failed because
-- the end of file has been reached.
isEOFError          :: IOError -> Bool
isEOFError           = check EOF

-- | An error indicating that an 'IO' operation failed because
-- the operation was not possible.
-- Any computation which returns an 'IO' result may fail with
-- 'isIllegalOperation'.  In some cases, an implementation will not be
-- able to distinguish between the possible error causes.  In this case
-- it should fail with 'isIllegalOperation'.
isIllegalOperation  :: IOError -> Bool
isIllegalOperation   = check IllegalOperation

-- | An error indicating that an 'IO' operation failed because
-- the user does not have sufficient operating system privilege
-- to perform that operation.
isPermissionError   :: IOError -> Bool
isPermissionError    = check Permission

-- | A programmer-defined error value constructed using 'userError'.
isUserError         :: IOError -> Bool
isUserError          = check User

deriving instance Eq IOErrorType
deriving instance Ord IOErrorType

alreadyExistsErrorType = AlreadyExists
doesNotExistErrorType = DoesNotExist
alreadyInUseErrorType = AlreadyInUse
fullErrorType = Full
eofErrorType = EOF
illegalOperationErrorType = IllegalOperation
permissionErrorType = Permission
userErrorType = User

instance Show IOError where
    showsPrec _ s = (ioeGetErrorString s ++)
