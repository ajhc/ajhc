module Util.ExitCodes where

import System.Exit

-- The command was used incorrectly, e.g., with
-- the wrong number of arguments, a bad flag, a bad
-- syntax in a parameter, or whatever.
exitCodeUsage = ExitFailure 64

-- EX_DATAERR -- The input data was incorrect in some way.
-- 	This should only be used for user's data & not
-- 	system files.
exitCodeDataError = ExitFailure 65

-- EX_NOINPUT -- An input file (not a system file) did not
-- 	exist or was not readable.  This could also include
-- 	errors like "No message" to a mailer (if it cared
-- 	to catch it).
exitCodeNoInput = ExitFailure 66

-- EX_NOUSER -- The user specified did not exist.  This might
-- 	be used for mail addresses or remote logins.
exitCodeNoUser = ExitFailure 67

-- EX_NOHOST -- The host specified did not exist.  This is used
-- 	in mail addresses or network requests.
exitCodeNoHost = ExitFailure 68

-- EX_UNAVAILABLE -- A service is unavailable.  This can occur
-- 	if a support program or file does not exist.  This
-- 	can also be used as a catchall message when something
-- 	you wanted to do doesn't work, but you don't know
-- 	why.
exitCodeUnavailable = ExitFailure 69

-- EX_SOFTWARE -- An internal software error has been detected.
-- 	This should be limited to non-operating system related
-- 	errors as possible.
exitCodeSoftware = ExitFailure 70

-- EX_OSERR -- An operating system error has been detected.
-- 	This is intended to be used for such things as "cannot
-- 	fork", "cannot create pipe", or the like.  It includes
-- 	things like getuid returning a user that does not
-- 	exist in the passwd file.
exitCodeOSError = ExitFailure 71

-- EX_OSFILE -- Some system file (e.g., /etc/passwd, /etc/utmp,
-- 	etc.) does not exist, cannot be opened, or has some
-- 	sort of error (e.g., syntax error).
exitCodeOSFile = ExitFailure 72

-- EX_CANTCREAT -- A (user specified) output file cannot be
-- 	created.
exitCodeCantCreate = ExitFailure 73

-- EX_IOERR -- An error occurred while doing I/O on some file.
exitCodeIOErr = ExitFailure 74

-- EX_TEMPFAIL -- temporary failure, indicating something that
-- 	is not really an error.  In sendmail, this means
-- 	that a mailer (e.g.) could not create a connection,
-- 	and the request should be reattempted later.
exitCodeTempFailure = ExitFailure 75

-- EX_PROTOCOL -- the remote system returned something that
-- 	was "not possible" during a protocol exchange.
exitCodeProtocol = ExitFailure 76

-- EX_NOPERM -- You did not have sufficient permission to
-- 	perform the operation.  This is not intended for
-- 	file system problems, which should use NOINPUT or
-- 	CANTCREAT, but rather for higher level permissions.
exitCodeNoPerm = ExitFailure 77

-- configuration error
exitCodeConfig = ExitFailure 78

