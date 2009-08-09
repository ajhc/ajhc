{-# OPTIONS_JHC -N #-}
module Prelude.IOError(IOError(),showIOError,userError) where

import Jhc.IO
import Jhc.Show

instance Show IOError where
    showsPrec _ s = showString (showIOError s)

{-

data IOError = IOError {
     ioe_handle   :: Maybe Handle,   -- the handle used by the action flagging
				     -- the error.
     ioe_type     :: IOErrorType,    -- what it was.
     ioe_location :: String,	     -- location.
     ioe_description :: String,      -- error type specific information.
     ioe_filename :: Maybe FilePath  -- filename the error is related to.
   } deriving(Eq)


-- | An abstract type that contains a value for each variant of 'IOError'.
data IOErrorType
  = AlreadyExists
  | NoSuchThing
  | ResourceBusy
  | ResourceExhausted
  | EOF
  | IllegalOperation
  | PermissionDenied
  | UserError

instance Show IOErrorType where
  showsPrec _ e =
    showString $
    case e of
      AlreadyExists	-> "already exists"
      NoSuchThing       -> "does not exist"
      ResourceBusy      -> "resource busy"
      ResourceExhausted -> "resource exhausted"
      EOF		-> "end of file"
      IllegalOperation	-> "illegal operation"
      PermissionDenied  -> "permission denied"
      UserError		-> "user error"

instance Show IOException where
    showsPrec p (IOError hdl iot loc s fn) =
      (case fn of
	 Nothing -> case hdl of
		        Nothing -> id
			Just h  -> showsPrec p h . showString ": "
	 Just name -> showString name . showString ": ") .
      (case loc of
         "" -> id
	 _  -> showString loc . showString ": ") .
      showsPrec p iot .
      (case s of
	 "" -> id
	 _  -> showString " (" . showString s . showString ")")

-}

