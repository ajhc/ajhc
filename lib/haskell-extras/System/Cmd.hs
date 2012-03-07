{-# OPTIONS_JHC -fffi #-}
module System.Cmd ( system ) where

import Foreign.C.String
import Foreign.C.Types
import System.Exit ( ExitCode(..) )

system      :: String -> IO ExitCode

system s = withCString s c_system >>= \r -> case r of
    0 -> return ExitSuccess
    _ -> return $ ExitFailure (fromIntegral r)

foreign import unsafe ccall "system" c_system :: CString -> IO CInt
