module System.IO.Error (

    -- * I\/O errors
    IOError,
    userError,		       	-- :: String  -> IOError
    --mkIOError,			-- :: IOErrorType -> String -> Maybe Handle
				--    -> Maybe FilePath -> IOError

    -- ** Classifying I\/O errors
    isAlreadyExistsError,	-- :: IOError -> Bool
    isDoesNotExistError,
    isAlreadyInUseError,
    isFullError,
    isEOFError,
    isIllegalOperation,
    isPermissionError,
    isUserError,

    -- * Throwing and catching I\/O errors

    ioError,		       	-- :: IOError -> IO a

    catch,			-- :: IO a -> (IOError -> IO a) -> IO a
    try,			-- :: IO a -> IO (Either IOError a)
    ioeGetErrorString,
    ioeGetFileName,
    ioeGetHandle,

    modifyIOError		-- :: (IOError -> IOError) -> IO a -> IO a
  ) where

import Data.Maybe
import Jhc.IO




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
--mkIOError :: IOErrorType -> String -> Maybe Handle -> Maybe FilePath -> IOError
--mkIOError = error "mkIOError"

-- -----------------------------------------------------------------------------
-- IOErrorType

-- | An error indicating that an 'IO' operation failed because
-- one of its arguments already exists.
isAlreadyExistsError :: IOError -> Bool
isAlreadyExistsError = isAlreadyExistsErrorType    . ioeGetErrorType

-- | An error indicating that an 'IO' operation failed because
-- one of its arguments does not exist.
isDoesNotExistError :: IOError -> Bool
isDoesNotExistError  = isDoesNotExistErrorType     . ioeGetErrorType

-- | An error indicating that an 'IO' operation failed because
-- one of its arguments is a single-use resource, which is already
-- being used (for example, opening the same file twice for writing
-- might give this error).
isAlreadyInUseError :: IOError -> Bool
isAlreadyInUseError  = isAlreadyInUseErrorType     . ioeGetErrorType

-- | An error indicating that an 'IO' operation failed because
-- the device is full.
isFullError         :: IOError -> Bool
isFullError          = isFullErrorType             . ioeGetErrorType

-- | An error indicating that an 'IO' operation failed because
-- the end of file has been reached.
isEOFError          :: IOError -> Bool
isEOFError           = isEOFErrorType              . ioeGetErrorType

-- | An error indicating that an 'IO' operation failed because
-- the operation was not possible.
-- Any computation which returns an 'IO' result may fail with
-- 'isIllegalOperation'.  In some cases, an implementation will not be
-- able to distinguish between the possible error causes.  In this case
-- it should fail with 'isIllegalOperation'.
isIllegalOperation  :: IOError -> Bool
isIllegalOperation   = isIllegalOperationErrorType . ioeGetErrorType

-- | An error indicating that an 'IO' operation failed because
-- the user does not have sufficient operating system privilege
-- to perform that operation.
isPermissionError   :: IOError -> Bool
isPermissionError    = isPermissionErrorType       . ioeGetErrorType

-- | A programmer-defined error value constructed using 'userError'.
isUserError         :: IOError -> Bool
isUserError          = isUserErrorType             . ioeGetErrorType

-- -----------------------------------------------------------------------------
-- IOErrorTypes

data IOErrorType = AlreadyExists | NoSuchThing | ResourceBusy
		 | ResourceExhausted | EOF | IllegalOperation
		 | PermissionDenied | UserError
                 deriving(Eq,Ord)

instance Show IOError where
    showsPrec _ s = showString (showIOError s)

-- -----------------------------------------------------------------------------
-- IOErrorType predicates

-- | I\/O error where the operation failed because one of its arguments
-- already exists.
isAlreadyExistsErrorType :: IOErrorType -> Bool
isAlreadyExistsErrorType AlreadyExists = True
isAlreadyExistsErrorType _ = False

-- | I\/O error where the operation failed because one of its arguments
-- does not exist.
isDoesNotExistErrorType :: IOErrorType -> Bool
isDoesNotExistErrorType NoSuchThing = True
isDoesNotExistErrorType _ = False

-- | I\/O error where the operation failed because one of its arguments
-- is a single-use resource, which is already being used.
isAlreadyInUseErrorType :: IOErrorType -> Bool
isAlreadyInUseErrorType ResourceBusy = True
isAlreadyInUseErrorType _ = False

-- | I\/O error where the operation failed because the device is full.
isFullErrorType :: IOErrorType -> Bool
isFullErrorType ResourceExhausted = True
isFullErrorType _ = False

-- | I\/O error where the operation failed because the end of file has
-- been reached.
isEOFErrorType :: IOErrorType -> Bool
isEOFErrorType EOF = True
isEOFErrorType _ = False

-- | I\/O error where the operation is not possible.
isIllegalOperationErrorType :: IOErrorType -> Bool
isIllegalOperationErrorType IllegalOperation = True
isIllegalOperationErrorType _ = False

-- | I\/O error where the operation failed because the user does not
-- have sufficient operating system privilege to perform that operation.
isPermissionErrorType :: IOErrorType -> Bool
isPermissionErrorType PermissionDenied = True
isPermissionErrorType _ = False

-- | I\/O error that is programmer-defined.
isUserErrorType :: IOErrorType -> Bool
isUserErrorType UserError = True
isUserErrorType _ = False

-- -----------------------------------------------------------------------------
-- Miscellaneous

-- | Catch any 'IOError' that occurs in the computation and throw a
-- modified version.
modifyIOError :: (IOError -> IOError) -> IO a -> IO a
modifyIOError f io = catch io (\e -> ioError (f e))

ioeGetErrorType	      :: IOError -> IOErrorType
ioeGetErrorType _ = UserError

ioeGetHandle _ = error "ioeGetHandle"
ioeGetHandle _ = error "ioeGetHandle"

ioeGetErrorString s = showIOError s
ioeGetFileName _ = error "ioeGetFileName"


